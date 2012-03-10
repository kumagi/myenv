#ifndef PYTHON_SWITCH_H
#define PYTHON_SWITCH_H

#ifdef PYPY_VERSION
# define INITERROR return
# define PYPY
# define PyByteArray_GET_SIZE PyString_GET_SIZE
# define PyByteArray_AS_STRING PyString_AS_STRING
# define PyByteArray_Check PyString_Check

#else
# if (PY_MAJOR_VERSION == 3 && PY_MINOR_VERSION >= 1) || PY_MAJOR_VERSION > 3
#  define PY3
#  define INITERROR return NULL
#  define PyInt_Check PyLong_Check
# else
#  define INITERROR return
# endif
#endif

#endif /* PYTHON_SWITCH_H */
