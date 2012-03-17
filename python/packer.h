/* -*- Mode: C; tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil; compile-command: "tox" -*- */
#ifndef PACKER_H
#define PACKER_H

#include <Python.h>
#include "python_switch.h"

#include <longintrepr.h>
#include "alt_malloc.h"
#include "msgpack/pack_define.h"

#define msgpack_pack_inline_func(name) \
	static inline void msgpack_pack ## name

#define msgpack_pack_inline_func_cint(name) \
	static inline void msgpack_pack ## name

#define msgpack_pack_user buffer*

#define msgpack_pack_append_buffer(user, buf, len) \
  (*user->callback)(user->data, buf, len)

typedef struct {
  PyObject_HEAD;
  size_t size;
  char* data;
  size_t alloc;
} Packer;

#ifndef NDEBUG
static void dump_packer(void* b){
  const Packer* const packer = (Packer*)b;
  size_t i;
  printf("0x");
  for(i = 0; i < packer->size; ++i){
    printf("%02x", packer->data[i] & 255);
  }
  printf("\n");
}

static void dump_buf(const void* b, int len){
  const char* const buf = (char*)b;
  int i;
  printf("0x");
  for(i = 0; i < len; ++i){
    printf("%02x", buf[i] & 255);
  }
  printf("\n");
}

#endif

#define get_typename(x) Py_TYPE(x)->tp_name
#define get_ob_size(x) ((PyVarObject*)x)->ob_size
#define MSGPACK_INITIAL_BUFFER_SIZE 4096

/* Packer public methods */
static int init_packer(Packer *self, PyObject *args, PyObject *kwds);
static PyObject *new_packer(PyTypeObject *subtype, PyObject *args, PyObject *kwds);
static void packer_delete(PyObject* self);
static int packer_print(PyObject* self, FILE* target, int flag);
static long packer_hash(PyObject* self);
static PyObject* packer_str(PyObject* self);

static PyObject* packer_pack(PyObject* packer, PyObject* target);

static PyMethodDef packer_methods[] = {
  {"pack", (PyCFunction)packer_pack, METH_O, "packing object"},
  {NULL, NULL, 0, NULL}
};

PyTypeObject packerType = {
  PyVarObject_HEAD_INIT(&PyType_Type, 0)
  "msgpackya.Packer",        /* char *tp_name; */
  sizeof(Packer),   /* int tp_basicsize; */
  0,                /* int tp_itemsize; */
  packer_delete,   /* destructor tp_dealloc; */
  packer_print,     /* printfunc  tp_print;   */
  NULL,             /* getattrfunc  tp_getattr;  __getattr__ */
  NULL,             /* setattrfunc  tp_setattr;  __setattr__ */
  NULL,             /* cmpfunc  tp_compare;  __cmp__ */
  NULL,             /* reprfunc  tp_repr;    __repr__ */
  NULL,             /* PyNumberMethods *tp_as_number; */
  NULL,             /* PySequenceMethods *tp_as_sequence; */
  NULL,             /* PyMappingMethods *tp_as_mapping; */
  packer_hash,      /* hashfunc tp_hash;     __hash__ */
  NULL,                /* ternaryfunc tp_call;  __call__ */
  packer_str,        /* reprfunc tp_str;    __str__ */
  NULL, /*tp_getattro*/
  NULL, /*tp_setattro*/
  NULL, /*tp_as_buffer*/
  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
  "a Packer object for saving malloc/free", /*tp_doc*/
  NULL, /*tp_traverse*/
  NULL, /*tp_clear*/
  NULL, /*tp_richcompare*/
  0, /*tp_weaklistoffset*/
  NULL, /*tp_iter*/
  NULL, /*tp_iternext*/
  packer_methods, /*tp_methods*/
  NULL, /*tp_members*/
  NULL, /*tp_getset*/
  NULL, /*tp_base*/
  NULL, /*tp_dict*/
  NULL, /*tp_descr_get*/
  NULL, /*tp_descr_set*/
  0, /*tp_dictoffset*/
  NULL, /*tp_init*/
  NULL, /*tp_alloc (initialized by TpReady())*/
  new_packer, /*tp_new*/
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

static int init_packer(Packer *self, PyObject *args, PyObject *kwds)
{
  assert(self != NULL);
  Packer* const packer = (Packer*)self;
  packer->size = 0;
  packer->alloc = MSGPACK_INITIAL_BUFFER_SIZE;
  packer->data = (char*)alt_malloc(MSGPACK_INITIAL_BUFFER_SIZE);
	memset(packer->data, 0, sizeof(MSGPACK_INITIAL_BUFFER_SIZE));
  return 0;
}
static void clear_packer(Packer *self)
{
  assert(self != NULL);
  Packer* const packer = (Packer*)self;
  packer->size = 0;
	memset(packer->data, 0, packer->alloc);
}

static PyObject *new_packer(PyTypeObject *type, PyObject *args, PyObject *kwds)
{

  /* // FIXME: use arguments.
  PyObject* callback;
  char* encoding, unicode_errors;
  size_t encoding_len, unicode_errors_len;
  if(!PyArg_ParseTuple(args, "Os#s#",&callback,
                        &encoding, &encoding_len,
                       &unicode_errors, &unicode_errors_len)){
  }
  */

  Packer* packer = (Packer*)type->tp_alloc(type, 0);
  //Packer* packer = PyObject_NEW_VAR(Packer,&packerType, 0);
  init_packer(packer,NULL,NULL);
  return (PyObject*)packer;
}
static void packer_delete(PyObject* self)
{
  Packer* const packer = (Packer*)self;
  alt_free(packer->data);
  Py_DECREF(self);
}
static int packer_print(PyObject* self, FILE* target, int flag)
{
  fprintf(target, "<msgpackya.Packer at %p>", self);
  return 0;
}

static PyObject* packer_str(PyObject* self)
{
  Packer *packer = (Packer*)self;
  char buf[100];
  snprintf(buf, 100, "<Packer %p, length: %zd, data:%8s...>", packer, packer->size, packer->data);
  const size_t length = sizeof(buf);
  return PyBytes_FromStringAndSize(buf, length);
}
static long packer_hash(PyObject* self)
{
  return 0;
}

/* buffer */
typedef int (*msgpack_packer_write)(void* data, const uint8_t* buf, unsigned int len);
typedef struct {
	void* data;
	msgpack_packer_write callback;
} buffer;

/* buffer public methods */
static int msgpack_python_buffer_write(void* data, const uint8_t* buf, unsigned int len)
{
	Packer* packer = (Packer*)data;
	if(packer->alloc - packer->size < len) {
#if 0
		size_t nsize = (packer->alloc) ? packer->alloc * 2 : MSGPACK_INITIAL_BUFFER_SIZE;
		while(nsize < packer->size + len) { nsize *= 2; }
		void* const tmp = alt_realloc(packer->data, nsize);
		if(!tmp) { return -1; }
		packer->data = (char*)tmp;
#endif
    size_t nsize = (packer->alloc + len) * 2;
    void* const tmp = alt_realloc(packer->data, nsize);
    if(!tmp)return -1;
    packer->data = (char*)tmp;
		packer->alloc = nsize;
	}

	memcpy(packer->data + packer->size, buf, len);
	packer->size += len;
	return 0;
}
static inline void buffer_init(buffer* const p, void* data, msgpack_packer_write callback)
{
  p->data = data;
  p->callback = callback;
}
static inline void* buffer_data(buffer* const b)
{
  return ((Packer*)b->data)->data;
}
static inline size_t buffer_size(buffer* const b)
{
  return ((Packer*)b->data)->size;
}

#endif /* ifdef PACKER_H */
#include "msgpack/pack_template.h"
