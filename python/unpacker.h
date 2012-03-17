/* -*- Mode: C; tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil; compile-command: "tox" -*- */
#ifndef UNPACKER_H
#define UNPACKER_H
#include <Python.h>
#include "alt_malloc.h"
#include <stdio.h>

typedef struct {
  PyObject_HEAD;
  size_t size;
  size_t consumed;
  char* data;
  size_t alloc;
} Unpacker;
#define MSGPACK_INITIAL_UNPACK_BUFFER_SIZE 256


static int unpacker_init(Unpacker *self, PyObject *args, PyObject *kwds);
static PyObject *unpacker_new(PyTypeObject *subtype, PyObject *args, PyObject *kwds);
static void unpacker_delete(PyObject* self);
static int unpacker_print(PyObject* self, FILE* target, int flag);
static PyObject* unpacker_str(PyObject* self);

#define macro_unpacker_method(name)\
static PyObject* unpacker_##name(PyObject *self, PyObject *args);
macro_unpacker_method(feed);
macro_unpacker_method(next);
#undef macro_unpacker_method
static PyObject* unpacker_unpack(PyObject* self);

#define macro_unpacker_method_noarg(name)\
static PyObject* unpacker_##name(PyObject *self);
macro_unpacker_method_noarg(iter);
macro_unpacker_method_noarg(iternext);
#ifndef NDEBUG
macro_unpacker_method_noarg(dump);
#endif /* NDEBUG */
#undef macro_unpacker_method_noarg

static PyMethodDef unpacker_methods[] = {
  {"feed", (PyCFunction)unpacker_feed,METH_O, "feed buffer"},
  {"next", (PyCFunction)unpacker_next,METH_O, "next buffer"},
  {"unpack", (PyCFunction)unpacker_unpack,METH_NOARGS, "feed buffer"},
#ifndef NDEBUG
  {"dump", (PyCFunction)unpacker_dump,METH_NOARGS, "dump buffer"},
#endif /* NDEBUG */
  {NULL, NULL, 0, NULL}
};
#ifdef PY3
#define Py_TPFLAGS_HAVE_ITER 0
#endif

PyTypeObject unpackerType = {
  PyVarObject_HEAD_INIT(&PyType_Type, 0)
  "msgpackya.Unpacker",        /* char *tp_name; */
  sizeof(Unpacker),   /* int tp_basicsize; */
  0,                /* int tp_itemsize; */
  unpacker_delete,   /* destructor tp_dealloc; */
  unpacker_print,     /* printfunc  tp_print;   */
  NULL,             /* getattrfunc  tp_getattr;  __getattr__ */
  NULL,             /* setattrfunc  tp_setattr;  __setattr__ */
  NULL,             /* cmpfunc  tp_compare;  __cmp__ */
  NULL,             /* reprfunc  tp_repr;    __repr__ */
  NULL,             /* PyNumberMethods *tp_as_number; */
  NULL,             /* PySequenceMethods *tp_as_sequence; */
  NULL,             /* PyMappingMethods *tp_as_mapping; */
  NULL,             /* hashfunc tp_hash;     __hash__ */
  NULL,                /* ternaryfunc tp_call;  __call__ */
  unpacker_str,        /* reprfunc tp_str;    __str__ */
  NULL, /*tp_getattro*/
  NULL, /*tp_setattro*/
  NULL, /*tp_as_buffer*/
  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_HAVE_ITER,/*tp_flags*/
  "a streaming unpacker", /*tp_doc*/
  NULL, /*tp_traverse*/
  NULL, /*tp_clear*/
  NULL, /*tp_richcompare*/
  0, /*tp_weaklistoffset*/
  unpacker_iter, /*tp_iter*/
  unpacker_iternext, /*tp_iternext*/
  unpacker_methods, /*tp_methods*/
  NULL, /*tp_members*/
  NULL, /*tp_getset*/
  NULL, /*tp_base*/
  NULL, /*tp_dict*/
  NULL, /*tp_descr_get*/
  NULL, /*tp_descr_set*/
  0, /*tp_dictoffset*/
  NULL, /*tp_init*/
  NULL, /*tp_alloc (initialized by TpReady())*/
  unpacker_new, /*tp_new*/
  NULL, /*tp_free*/
  NULL, /*tp_is_gc*/
  NULL, /*tp_bases*/
  NULL, /*tp_mro*/
  NULL, /*tp_cache*/
  NULL, /*tp_subclasses*/
  NULL, /*tp_weaklist*/
  NULL, /*tp_del*/
#if PY_VERSION_HEX >= 0x02060000
  0, /*tp_version_tag*/
#endif
};

static int unpacker_init(Unpacker *self, PyObject *args, PyObject *kwds)
{
  assert(self != NULL);
  Unpacker* const unpacker = (Unpacker*)self;
  unpacker->size = 0;
  unpacker->consumed = 0;
  unpacker->alloc = MSGPACK_INITIAL_UNPACK_BUFFER_SIZE;
  unpacker->data = (char*)alt_malloc(MSGPACK_INITIAL_UNPACK_BUFFER_SIZE);
	memset(unpacker->data, 0, sizeof(MSGPACK_INITIAL_UNPACK_BUFFER_SIZE));
  return 0;
}
static PyObject *unpacker_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  Unpacker* unpacker = (Unpacker*)type->tp_alloc(type, 0);
  unpacker_init(unpacker,NULL,NULL);
  return (PyObject*)unpacker;
}
static void unpacker_delete(PyObject* self)
{
  Unpacker* const unpacker = (Unpacker*)self;
  // fprintf(stderr,"Unpacker deleted\n");
  alt_free(unpacker->data);
  Py_DECREF(self);
}
static int unpacker_print(PyObject* self, FILE* target, int flag)
{
  fprintf(target, "<msgpackya.Unacker at %p>", self);
  return 0;
}
static PyObject* unpacker_str(PyObject* self)
{
  Unpacker *unpacker = (Unpacker*)self;
  char buf[100];
  snprintf(buf, 100, "<msgpackya.Unpacker %p, length: %zd, consumed:%zd>", unpacker, unpacker->size, unpacker->consumed);
  const size_t length = strlen(buf);
  return PyBytes_FromStringAndSize(buf, length);
}

static PyObject* unpacker_iter(PyObject *self)
{
  Py_INCREF(self);
  return self;
}



static PyObject* unpacker_iternext(PyObject *self)
{
  return unpacker_unpack(self);
}

#ifndef NDEBUG
static PyObject* unpacker_dump(PyObject *self)
{
  const Unpacker* const unpacker = (Unpacker*)self;
  size_t i;
  printf("size: %zd consumed:%zd data:[0x", unpacker->size, unpacker->consumed);
  for(i = 0; i < unpacker->size; ++i){
    printf(" %02x", unpacker->data[i] & 255);
  }
  printf("]\n");
  Py_RETURN_NONE;
}
#endif /* NDEBUG */

static PyObject* unpacker_feed(PyObject *self, PyObject *arg)
{
	Unpacker* unpacker = (Unpacker*)self;
  if(!PyBytes_Check(arg)){
    PyErr_SetString(PyExc_StopIteration, "No more unpack data.");
  }

  const char* buf = PyBytes_AS_STRING(arg);
  const size_t len = PyBytes_GET_SIZE(arg);
	if(unpacker->alloc - unpacker->size < len) {
		size_t nsize = (unpacker->alloc) ? unpacker->alloc * 2 : MSGPACK_INITIAL_UNPACK_BUFFER_SIZE;

		while(nsize < unpacker->size + len) { nsize *= 2; }

		void* const tmp = alt_realloc(unpacker->data, nsize);
		if(!tmp) { return NULL; }
		unpacker->data = (char*)tmp;
		unpacker->alloc = nsize;
	}
	memcpy(unpacker->data + unpacker->size, buf, len);
	unpacker->size += len;
	Py_RETURN_NONE;
}

static PyObject* unpacker_next(PyObject *self, PyObject *arg)
{
  return 0;
}
#endif
