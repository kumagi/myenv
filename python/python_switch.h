#ifndef PYTHON_SWITCH_H
#define PYTHON_SWITCH_H

#if (PY_MAJOR_VERSION == 3 && PY_MINOR_VERSION >= 1) || PY_MAJOR_VERSION > 3
# define PY3
# define INITERROR return NULL
# define PyInt_Check PyLong_Check
#else
# define INITERROR return
#endif

#endif
