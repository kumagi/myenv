/* -*- Mode: C; tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil; compile-command: "tox" -*- */
#include <Python.h>
#include <assert.h>
#define __STDC_FORMAT_MACROS
#include <inttypes.h>
//(setq compile-command "tox && notify-send 'ok' || nofity-send 'ng'")
//(setq compile-command "make profile")
#include "python_switch.h"
#include "pack.h"
#include "unpack.h"
/* c */
char* const name = "msgpackya";
static PyMethodDef module_methods[] = {
  {"pack", (PyCFunction)msgpack_packb, METH_O,
      "pack(o, stream)\n"
      "pack an object `o` and write it to stream."},
  {"packb",(PyCFunction)msgpack_packb, METH_O,
   "packb(o) -> bytes\n"
   "pack `o` and return packed bytes."},
  {"dump", (PyCFunction)msgpack_packb, METH_O,
   "alias of pack(...)"},
  {"dumps", (PyCFunction)msgpack_packb, METH_O,
   "alias of packb(...)"},
  {"packs",(PyCFunction)msgpack_packb, METH_O,
   "alias of packb(...)"},

  {"unpack",(PyCFunction)msgpack_unpackb, METH_O,
   "unpack(stream) -> object\n"
   "Unpack an object from stream"},
  {"unpackb",(PyCFunction)msgpack_unpackb, METH_O,
   "unpacks(bytes) -> object\n"
   "Unpack packed_bytes to object. Returns an unpacked Object."},
  {"load", (PyCFunction)msgpack_unpackb, METH_O,
   "alias of unpack(...)"},
  {"loads", (PyCFunction)msgpack_unpackb, METH_O,
   "alias of unpackb(...)"},
  {"unpacks", (PyCFunction)msgpack_unpackb, METH_O,
   "alias of unpackb(...)"},
  {"unpacksss",(PyCFunction)msgpack_unpackb, METH_O,
   "alias of unpackb(...)"},
  {0,0,0,0} // sentinel
};

#ifdef PY3
static struct PyModuleDef core_module_def = {
  PyModuleDef_HEAD_INIT,
  "msgpackya",
  NULL,
  -1,
  module_methods,
};
#endif

#ifdef PY3
# define MOD_INIT(name) PyMODINIT_FUNC PyInit_##name(void)
#else
# define MOD_INIT(name) PyMODINIT_FUNC init##name(void)
#endif

MOD_INIT(msgpackya){
  PyObject *m;
#ifdef PY3
  m = PyModule_Create(&core_module_def);
#else
  m = Py_InitModule3(name, module_methods, "");
#endif

  if(m == NULL){
    INITERROR;
  }

  /* Packer */
  if (PyType_Ready(&packerType) < 0){
    return NULL;
  }
  Py_INCREF(&packerType);
  PyModule_AddObject(m, "Packer", (PyObject *)&packerType);

  /* Unpacker */
  if (PyType_Ready(&unpackerType) < 0){
    return NULL;
  }
  Py_INCREF(&unpackerType);
  PyModule_AddObject(m, "Unpacker", (PyObject *)&unpackerType);


#ifdef PY3
  return m;
#endif
}
