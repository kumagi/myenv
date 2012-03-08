/* -*- Mode: C; tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*- */
#include <Python.h>
#include <longintrepr.h>
#include <assert.h>
#define __STDC_FORMAT_MACROS
#include <inttypes.h>
//(setq compile-command "tox && notify-send 'ok' || nofity-send 'ng'")


#if (PY_MAJOR_VERSION == 3 && PY_MINOR_VERSION >= 1) || PY_MAJOR_VERSION > 3
# define PY3
# define INITERROR return NULL
# define PyInt_Check PyLong_Check
#else
# define INITERROR return
#endif

#define USE_PYTHON_MEMORY_ALLOCATOR
#ifdef USE_PYTHON_MEMORY_ALLOCATOR
# define alt_malloc(x) PyMem_Malloc(x)
# define alt_realloc(x,y) PyMem_Realloc(x,y)
# define alt_free(x) PyMem_Del(x)
#else
# define alt_malloc(x) malloc(x)
# define alt_realloc(x,y) realloc(x,y)
# define alt_free(x) free(x)
#endif

#include "msgpack/pack_define.h"

#define msgpack_pack_inline_func(name) \
	static inline void msgpack_pack ## name

#define msgpack_pack_inline_func_cint(name) \
	static inline void msgpack_pack ## name

#define msgpack_pack_user msgpack_packer*

#define msgpack_pack_append_buffer(user, buf, len) \
  (*user->callback)(user->data, buf, len)

typedef struct {
	size_t size;
	char* data;
	size_t alloc;
} py_buffer_t;

#ifndef NDEBUG
static void dump_pbuf(void* b){
  const py_buffer_t* const pbuf = b;
  int i;
  printf("0x");
  for(i = 0; i < pbuf->size; ++i){
    printf("%02x", pbuf->data[i] & 255);
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

typedef int (*msgpack_packer_write)(void* data, const uint8_t* buf, unsigned int len);
typedef struct {
	void* data;
	msgpack_packer_write callback;
} msgpack_packer;

#include "msgpack/pack_template.h"

static int msgpack_python_buffer_write(void* data, const uint8_t* buf, unsigned int len);

#include <stdio.h>

#define get_typename(x) Py_TYPE(x)->tp_name
#ifdef PY3
#define get_ob_size(x) ((PyVarObject*)x)->ob_size
#else
#define get_ob_size(x) ((PyVarObject*)x)->ob_size
#endif
#define MSGPACK_INITIAL_BUFFER_SIZE 4096

static inline void pbuf_init(py_buffer_t* const buf)
{
  buf->size = 0;
  buf->alloc = MSGPACK_INITIAL_BUFFER_SIZE;
  buf->data = alt_malloc(MSGPACK_INITIAL_BUFFER_SIZE);
	memset(buf->data, 0, sizeof(MSGPACK_INITIAL_BUFFER_SIZE));
}
static inline void pbuf_free(py_buffer_t* const buf)
{
  alt_free(buf->data);
}

static inline void msgpack_packer_pbuf_init(msgpack_packer* const p)
{
  p->data = alt_malloc(sizeof(py_buffer_t));
  pbuf_init(p->data);
  p->callback = msgpack_python_buffer_write;
}

static inline const char* msgpack_packer_pbuf_buff(msgpack_packer* const p)
{
  return ((const py_buffer_t*)p->data)->data;
}
static inline const size_t msgpack_packer_pbuf_size(msgpack_packer* const p)
{
  return ((const py_buffer_t*)p->data)->size;
}

static inline void msgpack_packer_pbuf_free(msgpack_packer* const p)
{
  pbuf_free(p->data);
	alt_free(p->data);
}

// typedef int (*msgpack_packer_write)(void* data, const char* buf, unsigned int len);
static int msgpack_python_buffer_write(void* data, const uint8_t* buf, unsigned int len)
{
	py_buffer_t* pbuf = (py_buffer_t*)data;
	if(pbuf->alloc - pbuf->size < len) {
		size_t nsize = (pbuf->alloc) ? pbuf->alloc * 2 : MSGPACK_INITIAL_BUFFER_SIZE;

		while(nsize < pbuf->size + len) { nsize *= 2; }

		void* const tmp = alt_realloc(pbuf->data, nsize);
		if(!tmp) { return -1; }

		pbuf->data = (char*)tmp;
		pbuf->alloc = nsize;
	}

	memcpy(pbuf->data + pbuf->size, buf, len);
	pbuf->size += len;
	return 0;
}

static void msgpackya_pack_bool(msgpack_packer* p, PyObject* target)
{
  if(target == Py_True){
    msgpack_pack_true(p);
  }else{
    msgpack_pack_false(p);
  }
}

static void msgpackya_pack_int(msgpack_packer* p, PyObject* target)
{
  if(PyBool_Check(target)){
    msgpackya_pack_bool(p, target);
    return;
  }
# ifndef PY3 /* Py3 uses PyLong only */
  if(PyLong_Check(target)){
# endif
    const Py_ssize_t is_minus = Py_SIZE(target) < 0;
    const Py_ssize_t digits = Py_SIZE(target) < 0 ? -Py_SIZE(target) : Py_SIZE(target);

    uint64_t sum = 0;
    int i;
    const PyLongObject* const long_obj = (const PyLongObject*)target;
    for(i = 0; i < digits; ++i){
      sum += (long_obj->ob_digit[i] & PyLong_MASK) * (1LLU << (i * PyLong_SHIFT));
    }

    if(is_minus) {
      if(sum <= 0xffffffffffffffff) {
        msgpack_pack_int64(p, -sum);
        return;
      }
    }else{
      if(sum <= 0xffffffffffffffff) {
        msgpack_pack_uint64(p, sum);
        return;
      }
    }
    /* TODO: throw exception [range error] here */
# ifndef PY3
  }else if(PyInt_Check(target)){
    const PyIntObject* const int_obj = (const PyIntObject*)target;
    msgpack_pack_long_long(p, int_obj->ob_ival);
  }else{
    exit(1);
  }
# endif /* PY3 */
}

/* float (actually double)*/
static void msgpackya_pack_float(msgpack_packer* p, PyObject* target)
{
  msgpack_pack_double(p, PyFloat_AS_DOUBLE(target));
}

/* bytes */
static void msgpackya_pack_bytes(msgpack_packer* p, PyObject* target)
{
  msgpack_pack_raw(p, PyBytes_GET_SIZE(target));
  msgpack_pack_raw_body(p, PyBytes_AS_STRING(target), PyBytes_GET_SIZE(target));
}

/* unicode */
static void msgpackya_pack_unicode(msgpack_packer* p, PyObject* target)
{
  PyObject* py_str = PyUnicode_AsEncodedString(target, "utf-8", "error");
  msgpackya_pack_bytes(p, py_str);
}

static void msgpack_pack_object(msgpack_packer* p, PyObject* target);

/* list */
static void msgpackya_pack_list(msgpack_packer* p, PyObject* target)
{
  PyListObject* py_list = (PyListObject*)target;
  const int size = PyList_GET_SIZE(py_list);
  msgpack_pack_array(p, size);
  int i;
  for(i=0; i<size; ++i){
    msgpack_pack_object(p, PyList_GET_ITEM(py_list, i));
  }
}

/* tuple */
static void msgpackya_pack_tuple(msgpack_packer* p, PyObject* target)
{
  PyTupleObject* py_tuple = (PyTupleObject*)target;
  const int size = PyTuple_GET_SIZE(py_tuple);
  msgpack_pack_array(p, size);
  int i;
  for(i=0; i<size; ++i){
    msgpack_pack_object(p, PyTuple_GET_ITEM(py_tuple, i));
  }
}

/* dict */
static void msgpackya_pack_dict(msgpack_packer* p, PyObject* target)
{
  const Py_ssize_t size = PyDict_Size(target);
  PyListObject* keys = (PyListObject*)PyDict_Keys(target);
  msgpack_pack_map(p, size);
  int i;
  for(i=0; i<size; ++i){
    /* key */
    msgpack_pack_object(p, PyList_GET_ITEM(keys, i));
    PyObject* key = PyList_GET_ITEM(keys, i);
    msgpack_pack_object(p, PyDict_GetItem(target, key));
  }
}

/* bytearray */
static void msgpackya_pack_bytearray(msgpack_packer* p, PyObject* target)
{
  const int size = PyByteArray_GET_SIZE(target);
  msgpack_pack_raw(p, size);
  msgpack_pack_raw_body(p, PyByteArray_AS_STRING(target), size);
}

static void msgpack_pack_object(msgpack_packer* p, PyObject* target)
{
  //const char* const typename = get_typename(target);
  //printf("packing packer <- %s\n", typename);
  if(PyInt_Check(target) || PyLong_Check(target)){
    msgpackya_pack_int(p, target);
  }else if(PyBytes_Check(target)){
    msgpackya_pack_bytes(p, target);
  }else if(PyUnicode_Check(target)){
    msgpackya_pack_unicode(p, target);
  }else if(PyFloat_Check(target)){
    msgpackya_pack_float(p, target);
  }else if(PyList_Check(target)){
    msgpackya_pack_list(p, target);
  }else if(PyTuple_Check(target)){
    bmsgpackya_pack_tuple(p, target);
  }else if(PyDict_Check(target)){
    msgpackya_pack_dict(p, target);
  }else if(PyByteArray_Check(target)){
    msgpackya_pack_bytearray(p, target);
  }else if(target == Py_None){
    msgpack_pack_nil(p);
  }
}

static PyObject* msgpack_packb(PyObject* self,PyObject *target)
{
  assert(target != NULL);

  msgpack_packer p;
  msgpack_packer_pbuf_init(&p);

  msgpack_pack_object(&p, target);

  //dump_pbuf(p.data);
  //printf("packed %zd data\n", msgpack_packer_pbuf_size(&p));
  PyObject* result = PyBytes_FromStringAndSize(msgpack_packer_pbuf_buff(&p),
                                               msgpack_packer_pbuf_size(&p));
  /*
  printf("PyObject(%p):refcnt(%ld):ob_type:(%p)\n",
         result, result->ob_refcnt, result->ob_type);
  //*/

  /* copy to PyStringObject */
  msgpack_packer_pbuf_free(&p);
  return result;
}

/* unpack */
#include "msgpack/unpack_define.h"

#define msgpack_unpack_func(ret, name) \
	ret template ## name
#define msgpack_unpack_callback(name) \
	template_callback ## name
#define msgpack_unpack_object PyObject*

#define msgpack_unpack_struct(name) \
	struct template ## name

typedef int unpack_user;
#define msgpack_unpack_user unpack_user

static inline PyObject* template_callback_root(unpack_user* u)
{ return Py_None; }

static inline int template_callback_true(unpack_user* u, PyObject** o)
{ *o = Py_True; return 0; }
static inline int template_callback_false(unpack_user* u, PyObject** o)
{ *o = Py_False; return 0; }

#define create_PyLong(int_type)\
static inline int template_callback_ ## int_type(unpack_user* u, int_type ## _t d, PyObject** o)\
{ *o = PyLong_FromLong(d); return 0; }
create_PyLong(uint8)
create_PyLong(uint16)
create_PyLong(uint32)
create_PyLong(uint64)
create_PyLong(int8)
create_PyLong(int16)
create_PyLong(int32)
create_PyLong(int64)
#undef create_PyLong

static inline int template_callback_float(unpack_user* u, float d, PyObject** o)
{ *o = PyFloat_FromDouble((double)d); return 0; }

static inline int template_callback_double(unpack_user* u, double d, PyObject** o)
{ *o = PyFloat_FromDouble(d); return 0; }

static inline int template_callback_nil(unpack_user* u, PyObject** o)
{ *o = Py_None; return 0; }

static inline int template_callback_array(unpack_user* u, unsigned int n, PyObject** o)
{ *o = PyTuple_New(n); return 0; }

static inline int template_callback_array_item(unpack_user* u, PyObject** c, PyObject* o)
{
  Py_ssize_t i;
  while(PyTuple_GET_ITEM(*c, i) != NULL) ++i;
  PyTuple_SET_ITEM(*c, i, o);
  return 0;
}

static inline int template_callback_map(unpack_user* u, unsigned int n, PyObject** o)
{ *o = PyDict_New(); return 0; }

static inline int template_callback_map_item(unpack_user* u, PyObject** c, PyObject* k, PyObject* v)
{ PyDict_SetItem(*c, k, v); return 0; }

static inline int template_callback_raw(unpack_user* u, const char* b, const char* p, unsigned int l, PyObject** o)
{
  *o = PyBytes_FromStringAndSize(p, l); return 0;
}

struct template_context;
typedef struct template_context msgpack_unpack_t;

static void template_init(msgpack_unpack_t* u);

static PyObject* template_data(msgpack_unpack_t* u);

static int template_execute(msgpack_unpack_t* u,
		const char* data, size_t len, size_t* off);

#include "msgpack/unpack_template.h"


static PyObject* msgpack_unpackb(PyObject* self, PyObject* target)
{
  if(!PyBytes_Check(target)){
    printf("invalid type");
  }
  PyBytesObject* bytes = (PyBytesObject*)target;
  size_t offset = 0;

	msgpack_unpack_t ctx;
  template_init(&ctx);

  template_execute(&ctx, PyBytes_AS_STRING(bytes), PyBytes_GET_SIZE(target), &offset);
  PyObject* result = template_data(&ctx);
  return result;
}

char* const name = "msgpackya";
static PyMethodDef msgpack_methods[] = {
  {"pack", (PyCFunction)msgpack_packb, METH_VARARGS,
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
  msgpack_methods,
};
#endif

#ifdef PY3
PyObject* PyInit_msgpackya(void) {
#else
PyMODINIT_FUNC initmsgpackya(void) {
#endif

  PyObject *m;
#ifdef PY3
  m = PyModule_Create(&core_module_def);
#else
  m = Py_InitModule3(name, msgpack_methods, "");
#endif

  if(m == NULL){
    INITERROR;
  }

#ifdef PY3
  return m;
#endif
}
