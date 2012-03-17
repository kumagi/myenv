#ifndef PACK_H
#define PACK_H
//(setq compile-command "tox && notify-send 'ok' || nofity-send 'ng'")

#include <Python.h>
#include "packer.h"
#include "python_switch.h"
#include "alt_malloc.h"

#include <stdio.h>

static inline void buffer_free(buffer* const p)
{
  packer_delete((PyObject*)p->data);
	alt_free(p->data);
}

static void msgpackya_pack_bool(buffer* p, PyObject* target)
{
  if(target == Py_True){
    msgpack_pack_true(p);
  }else{
    msgpack_pack_false(p);
  }
}

static void msgpackya_pack_int(buffer* p, PyObject* target)
{
  if(PyBool_Check(target)){
    msgpackya_pack_bool(p, target);
    return;
  }
#ifdef PYPY
  if(PyInt_Check(target)){
    const long var = PyInt_AsLong(target);
    msgpack_pack_int32(p, var);
  }else if(PyLong_Check(target)){
    const long long var = PyInt_AsLong(target);
    msgpack_pack_int64(p, var);
  }
#else
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
#endif /* PYPY */
}

/* float (actually double)*/
static void msgpackya_pack_float(buffer* p, PyObject* target)
{
  msgpack_pack_double(p, PyFloat_AS_DOUBLE(target));
}

/* bytes */
static void msgpackya_pack_bytes(buffer* p, PyObject* target)
{
  msgpack_pack_raw(p, PyBytes_GET_SIZE(target));
  msgpack_pack_raw_body(p, PyBytes_AS_STRING(target), PyBytes_GET_SIZE(target));
}

/* unicode */
static void msgpackya_pack_unicode(buffer* p, PyObject* target)
{
  PyObject* py_str = PyUnicode_AsEncodedString(target, "utf-8", "error");
  msgpackya_pack_bytes(p, py_str);
}

static void msgpack_pack_object(buffer* p, PyObject* target);

/* list */
static void msgpackya_pack_list(buffer* p, PyObject* target)
{
  const int size = PyList_GET_SIZE(target);
  msgpack_pack_array(p, size);
  int i;
  for(i=0; i<size; ++i){
    msgpack_pack_object(p, PyList_GET_ITEM(target, i));
  }
}

/* tuple */
static void msgpackya_pack_tuple(buffer* p, PyObject* target)
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
static void msgpackya_pack_dict(buffer* p, PyObject* target)
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
static void msgpackya_pack_bytearray(buffer* p, PyObject* target)
{
  const int size = PyByteArray_GET_SIZE(target);
  msgpack_pack_raw(p, size);
  msgpack_pack_raw_body(p, PyByteArray_AS_STRING(target), size);
}

static void msgpack_pack_object(buffer* p, PyObject* target)
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

  buffer b;
  Packer p;
  init_packer(&p, NULL, NULL);

  buffer_init(&b, &p, msgpack_python_buffer_write);

  /* pack */
  msgpack_pack_object(&b, target);

  PyObject* result = PyBytes_FromStringAndSize((char*)buffer_data(&b),
                                               buffer_size(&b));

  /*
  printf("PyObject(%p):refcnt(%ld):ob_type:(%p)\n",
         result, result->ob_refcnt, result->ob_type);
  //*/

  /* copy to PyStringObject */
  packer_delete((PyObject*)&p);
  return result;
}

static PyObject* packer_pack(PyObject* packer, PyObject* target){
  assert(packer != NULL);
  Packer* p = (Packer*)packer;
  buffer b;
  buffer_init(&b, p, msgpack_python_buffer_write);

  /* pack */
  msgpack_pack_object(&b, target);
  PyObject* result = PyBytes_FromStringAndSize((char*)buffer_data(&b),
                                               buffer_size(&b));

  /*
  printf("PyObject(%p):refcnt(%ld):ob_type:(%p)\n",
         result, result->ob_refcnt, result->ob_type);
  //*/
  clear_packer(p);

  return result;
}


#endif
