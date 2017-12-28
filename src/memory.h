/*
	memory management for the Leda system
	Uses a variation on the Baker two-space algorithm
*/

/*
	The fundamental data type is the object.
	The first field in an object is a size, the low order two
		bits being used to maintain:
			* binary flag, used if data is binary
			* indirection flag, used if object has been relocated
	The first two data fields are always the class 
		and a surrounding context, while remaining
		data fields are values (either binary or object pointers)

	A few objects (class tables, other items that are guaranteed
	not to change) are allocated in static memory space -- space
	which is not ever garbage collected.
	The only pointer from static memory back to dynamic memory is
	the global context.

*/

struct ledaValue {
	int size;
	struct ledaValue * data [0];
	};

/*
	memoryBase holds the pointer to the current space,
	memoryPointer is the pointer into this space.
	To allocate, decrement memoryPointer by the correct amount.
	If the result is less than memoryBase, then garbage collection
	must take place

*/

extern struct ledaValue * memoryPointer;
extern struct ledaValue * memoryBase;

/*
	roots for the memory space
	these are traced down during memory management
*/
# define ROOTSTACKLIMIT 250
extern struct ledaValue * rootStack[];
extern int rootTop;
extern struct ledaValue * globalContext;
extern struct ledaValue * currentContext;

/*
	entry points
*/

void gcinit(int, int);
struct ledaValue * gcollect(int);
struct ledaValue * staticAllocate(int);

# define gxcalloc(sz) (((memoryPointer-=(sz+1))<memoryBase)?\
	gcollect(sz):(memoryPointer->size=sz<<2,memoryPointer))
# ifndef gcalloc
extern struct ledaValue * gcalloc(int);
# endif
