#ifndef PACK_H
#define PACK_H

#include "Python.h"
#include "python_switch.h"
#include <longintrepr.h>
#include "alt_malloc.h"
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
  const py_buffer_t* const pbuf = (py_buffer_t*)b;
  size_t i;
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
  buf->data = (char*)alt_malloc(MSGPACK_INITIAL_BUFFER_SIZE);
	memset(buf->data, 0, sizeof(MSGPACK_INITIAL_BUFFER_SIZE));
}
static inline void pbuf_free(py_buffer_t* const buf)
{
  alt_free(buf->data);
}

static inline void msgpack_packer_pbuf_init(msgpack_packer* const p)
{
  p->data = alt_malloc(sizeof(py_buffer_t));
  pbuf_init((py_buffer_t*)p->data);
  p->callback = msgpack_python_buffer_write;
}

static inline const char* msgpack_packer_pbuf_buff(msgpack_packer* const p)
{
  return ((const py_buffer_t*)p->data)->data;
}
static inline size_t msgpack_packer_pbuf_size(msgpack_packer* const p)
{
  return ((const py_buffer_t*)p->data)->size;
}

static inline void msgpack_packer_pbuf_free(msgpack_packer* const p)
{
  pbuf_free((py_buffer_t*)p->data);
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
  /* head */
  const int size = PyTuple_GET_SIZE(target);

  /* body */
  msgpack_pack_array(p, size);
  int i;
  for(i=0; i<size; ++i){
    msgpack_pack_object(p, PyTuple_GET_ITEM(target, i));
  }
}

/* dict */
static void msgpackya_pack_dict(msgpack_packer* p, PyObject* target)
{
  /* head */
  const Py_ssize_t size = PyDict_Size(target);
  msgpack_pack_map(p, size);

  /* body */
  PyObject *key, *value;
  Py_ssize_t pos = 0;
  while (PyDict_Next(target, &pos, &key, &value)) {
    msgpack_pack_object(p, key);
    msgpack_pack_object(p, value);
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
    msgpackya_pack_tuple(p, target);
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

#endif
