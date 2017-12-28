/*
	Leda memory management
*/

# include <stdio.h>
# include "memory.h"

extern int displayOperators;	/* true if we are debugging */

/*
	static memory space -- never recovered
*/
static struct ledaValue * staticBase;
static struct ledaValue * staticTop;
static struct ledaValue * staticPointer;

/*
	dynamic (managed) memory space
*/

static struct ledaValue * spaceOne;
static struct ledaValue * spaceTwo;
static int spaceSize;

struct ledaValue * memoryBase;
struct ledaValue * memoryPointer;
struct ledaValue * memoryTop;

static int inSpaceOne;
static struct ledaValue * oldBase;
static struct ledaValue * oldTop;

void dbaddr(int addr, int offset)
{
	struct ledaValue * t = (struct ledaValue *) addr;
	printf("DB offset %d in %d is %d\n", offset, t, t->data[offset]);
}

/*
	roots for memory access
*/
struct ledaValue * rootStack[ROOTSTACKLIMIT];
int rootTop = 0;
struct ledaValue * globalContext;
struct ledaValue * currentContext;

/*
	gcinit -- initialize the memory management system
*/
void gcinit(int staticsz, int dynamicsz)
{
		/* should do something better than this, but  ... */
	if (sizeof(int) != sizeof(struct  ledaValue *))
		yyerror("pointers not same size as ints");
	if (sizeof(float) != sizeof(struct ledaValue *))
		yyerror("floats not same size as pointers");

		/* allocate the memory areas */
	staticBase = (struct ledaValue *) 
		malloc(staticsz * sizeof(struct ledaValue));
	spaceOne = (struct ledaValue *) 
		malloc(dynamicsz * sizeof(struct ledaValue));
	spaceTwo = (struct ledaValue *) 
		malloc(dynamicsz * sizeof(struct ledaValue));
	if ((staticBase == 0) || (spaceOne == 0) || (spaceTwo == 0)) {
		fprintf(stderr,"not enough memory for space allocations\n");
		exit(1);
		}

	staticTop = staticBase + staticsz;
	staticPointer = staticTop;

	spaceSize = dynamicsz;
	memoryBase = spaceOne;
	memoryPointer = memoryBase + spaceSize;
	if (displayOperators)
		printf("space one %d, top %d , space two %d , top %d \n", 
			spaceOne, spaceOne + spaceSize, 
			spaceTwo, spaceTwo + spaceSize);
	inSpaceOne = 1;
}

/*
	gc_move is the heart of the garbage collection algorithm.
	It takes as argument a pointer to a value in the old space,
	and moves it, and everything it points to, into the new space
	The returned value is the address in the new space.
*/
static struct ledaValue * gc_move(struct ledaValue * ptr)
{
	register struct ledaValue * old_address = ptr;
	struct ledaValue * previous_object = 0;
	struct ledaValue * new_address = 0;
	struct ledaValue * replacement  = 0;
	int sz;

	while (1) {

		/* part 1.  Walking down the tree
			keep stacking objects to be moved until we find
			one that we can handle */
		while (1) {
		/* if we find a pointer in the current space 
			to the new space (other than indirections) then
			something is very wrong */
		if ((old_address >= memoryBase) && (old_address <= memoryTop)) {
			yyerror("GC invariant failure -- address in new space");
			}
				/* else see if not  in old space */
			if ((old_address < oldBase) || 
					(old_address > oldTop)) {
				replacement = old_address;
				old_address = previous_object;
				break;
			}
				/* else see if already forwarded */
			else if (old_address->size & 01)  {
				if (old_address->size & 02)
					replacement = old_address->data[0];
				else {
					sz = old_address->size >> 2;
					replacement = old_address->data[sz];
					}
				old_address = previous_object;
				break;
			}
				/* else see if binary object */
			else if (old_address->size & 02) {
				sz = old_address->size >> 2;
				memoryPointer -= (sz + 2);
				new_address = memoryPointer;
				new_address->size = (sz << 2) | 02;
				while (sz) {
					new_address->data[sz] = old_address->data[sz];
					sz--;
					}
				old_address->size |= 01;
				new_address->data[0] = previous_object;
				previous_object = old_address;
				old_address = old_address->data[0];
				previous_object->data[0] = new_address;
				/* now go chase down class pointer */
			}
				/* must be non-binary object */
			else  {
				sz = old_address->size >> 2;
				memoryPointer -= (sz + 2);
				new_address = memoryPointer;
				new_address->size = (sz << 2);
				old_address->size |= 01;
				new_address->data[sz] = previous_object;
				previous_object = old_address;
				old_address = old_address->data[sz];
				previous_object->data[sz] = new_address;
			}
		}

		/* part 2.  Fix up pointers, 
			move back up tree as long as possible
			old_address points to an object in the old space,
			which in turns points to an object in the new space,
			which holds a pointer that is now to be replaced.
			the value in replacement is the new value */

		while (1) {
			if (old_address == 0)  /* backed out entirely */
				return replacement;
				/* case 1, binary or last value */
			if ((old_address->size & 02) ||
				((old_address->size>>2) == 0)) {
					/* fix up class pointer */
				new_address = old_address->data[0];
				previous_object = new_address->data[0];
				new_address->data[0] = replacement;
				old_address->data[0] = new_address;
				replacement = new_address;
				old_address = previous_object;
			}
			else {
				sz = old_address->size >> 2;
				new_address = old_address->data[sz];
				previous_object = new_address->data[sz];
				new_address->data[sz] = replacement;
				sz -= 1;
				old_address->size = (sz << 2) | 01;
				new_address->data[sz] = previous_object;
				previous_object = old_address;
				old_address = old_address->data[sz];
				previous_object->data[sz] = new_address;
				break; /* go track down this value */
			}
		}

	}
}

/*
	gcollect -- garbage collection entry point
*/
struct ledaValue * gcollect(int sz)
{	int i;

	if (displayOperators)
		printf("doing gc\n");
	/* first change spaces */
	if (inSpaceOne) {
		memoryBase = spaceTwo;
		inSpaceOne = 0;
		oldBase = spaceOne;
		}
	else {
		memoryBase = spaceOne;
		inSpaceOne = 1;
		oldBase = spaceTwo;
		}
	memoryPointer = memoryTop = memoryBase + spaceSize;
	{long i; char * p = (char *) memoryBase;
	char * q = (char *) memoryTop;
	for (; p < q; ) *p++ = 0;
	}
	oldTop = oldBase + spaceSize;

	/* then do the collection */
	currentContext = gc_move(currentContext);
	for (i = globalContext->size >> 2; i >= 0; i--)
		globalContext->data[i] = gc_move(globalContext->data[i]);
	for (i = 0; i < rootTop; i++) {
		rootStack[i] = gc_move(rootStack[i]);
		}
	if (displayOperators)
		printf("finished gc\n");

	/* then see if there is room for allocation */
	memoryPointer -= sz + 2;
	if (memoryPointer < memoryBase) {
		yyerror("insufficient memory after garbage collection");
		}
	memoryPointer->size =  sz << 2;
	return memoryPointer;
}

/*
	static allocation -- tries to allocate values in an area
	that will not be subject to garbage collection
*/

struct ledaValue * staticAllocate(int sz)
{
	staticPointer -= sz + 2;
	if (staticPointer < staticBase)
		yyerror("insufficient static memory");
	staticPointer->size = sz << 2;
	return staticPointer;
}

/*
	if definition is not in-lined, here  is what it should be
*/
# ifndef gcalloc
struct ledaValue * gcalloc(int sz)
{
	memoryPointer -= sz + 2;
	if (memoryPointer < memoryBase) {
		return gcollect(sz);
		}
	memoryPointer->size =  sz << 2;
	return memoryPointer;
}
# endif
