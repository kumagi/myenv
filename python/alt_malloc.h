#ifndef ALT_MALLOC_H
#define ALT_MALLOC_H

//#define USE_PYTHON_MEMORY_ALLOCATOR
#ifdef USE_PYTHON_MEMORY_ALLOCATOR
# define alt_malloc(x) PyMem_Malloc(x)
# define alt_realloc(x,y) PyMem_Realloc(x,y)
# define alt_free(x) PyMem_Del(x)
#else
# define alt_malloc(x) malloc(x)
# define alt_realloc(x,y) realloc(x,y)
# define alt_free(x) free(x)
#endif

#endif
