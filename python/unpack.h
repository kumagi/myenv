/* -*- Mode: C; tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil; compile-command: "tox" -*- */
#ifndef UNPACK_H
#define UNPACK_H

#include <Python.h>
#include "unpacker.h"
#include "python_switch.h"
#include "alt_malloc.h"
//(setq compile-command "tox && notify-send 'ok' || nofity-send 'ng'")

/* unpack */

#include "msgpack/unpack_define.h"

#define msgpack_unpack_func(ret, name) \
	ret template ## name
#define msgpack_unpack_callback(name) \
	template_callback ## name
#define msgpack_unpack_object PyObject*

#define msgpack_unpack_struct(name) \
	struct template ## name

#define USERDATA_DEFLEN 32
typedef struct {
  uint32_t target;
  uint32_t filled;
} array_fill;

typedef struct {
  array_fill* counter_head;
  array_fill counter[USERDATA_DEFLEN];
  uint32_t index;
  uint32_t max;
} unpack_user;
#define msgpack_unpack_user unpack_user

void init_userdata(unpack_user* usr)
{
  usr->counter_head = usr->counter;
  usr->index = 0;
  usr->max = USERDATA_DEFLEN;
}
void delete_userdata(unpack_user* usr)
{
  if(usr->counter_head != usr->counter){
    alt_free(usr->counter_head);
  }
  return;
}
void dump_userdata(unpack_user* usr)
{
  fprintf(stderr, "counter_head: %p index:%u max:%u\n"
          ,usr->counter_head
          ,usr->index
          ,usr->max);
}

void new_array_userdata(unpack_user* usr, uint32_t len)
{
  usr->index += 1;
  if(usr->max == usr->index){
    array_fill* new_buff = (array_fill*)alt_malloc(sizeof(array_fill)*usr->max*2);
    memcpy(new_buff, usr->counter_head, usr->index);
    fprintf(stderr,"extended\n");
  }
  //  dump_userdata(usr);
  usr->counter_head[usr->index].target = len;
  usr->counter_head[usr->index].filled = 0;
}
uint32_t get_arrays_last_index_userdata(unpack_user* usr)
{
  return usr->counter_head[usr->index].filled;
}

void fill_userdata(unpack_user* usr){
  usr->counter_head[usr->index].filled += 1;
  if(usr->counter_head[usr->index].filled == usr->counter_head[usr->index].target){
    usr->index -= 1;
  }
}

static inline PyObject* template_callback_root(unpack_user* u)
{ return Py_None; }

static inline int template_callback_true(unpack_user* u, PyObject** o)
{ *o = Py_True; return 0; }
static inline int template_callback_false(unpack_user* u, PyObject** o)
{ *o = Py_False; return 0; }


#define create_Long(int_type)\
  static inline int template_callback_ ## int_type(unpack_user* u, int_type ## _t d, PyObject** o) \
  { *o = PyLong_FromLong(d); return 0; }
#define create_Ulong(int_type)\
  static inline int template_callback_ ## int_type(unpack_user* u, int_type ## _t d, PyObject** o) \
  {*o = PyLong_FromUnsignedLong(d); return 0; }
#ifdef PY3
create_Long(int8)
create_Long(int16)
create_Long(int32)
create_Long(int64)
create_Ulong(uint8)
create_Ulong(uint16)
create_Ulong(uint32)
create_Ulong(uint64)
#else
#define create_Int(int_type)\
  static inline int template_callback_ ## int_type(unpack_user* u, int_type ## _t d, PyObject** o) \
  {*o = PyInt_FromLong(d); return 0; }
create_Int(int8)
create_Int(int16)
create_Int(int32)
create_Long(int64)
create_Int(uint8)
create_Int(uint16)
create_Int(uint32)
create_Ulong(uint64)
#undef create_PyInt
#endif /* PY3 */

#undef create_Long
#undef create_Ulong

static inline int template_callback_float(unpack_user* u, float d, PyObject** o)
{ *o = PyFloat_FromDouble((double)d); return 0; }

static inline int template_callback_double(unpack_user* u, double d, PyObject** o)
{ *o = PyFloat_FromDouble(d); return 0; }

static inline int template_callback_nil(unpack_user* u, PyObject** o)
{ *o = Py_None; return 0; }

static inline int template_callback_array(unpack_user* u, unsigned int n, PyObject** o)
{
  *o = PyTuple_New(n);
  new_array_userdata(u,n);
  return 0;
}

static inline int template_callback_array_item(unpack_user* u, PyObject** c, PyObject* o)
{
  uint32_t i = get_arrays_last_index_userdata(u);
  PyTuple_SET_ITEM(*c, i, o);
  fill_userdata(u);
  return 0;
}

static inline int template_callback_map(unpack_user* u, unsigned int n, PyObject** o)
{
  PyObject *p = PyDict_New();
  if(!p){ return -1;}
  *o = p;
  return 0;
}

static inline int template_callback_map_item(unpack_user* u, PyObject** c, PyObject* k, PyObject* v)
{
  if (PyDict_SetItem(*c, k, v) == 0) {
    Py_DECREF(k);
    Py_DECREF(v);
    return 0;
  }
  return -1;
}

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
  size_t offset = 0;
	msgpack_unpack_t ctx;
  init_userdata(&ctx.user);
  template_init(&ctx);

  template_execute(&ctx, PyBytes_AS_STRING(target), PyBytes_GET_SIZE(target), &offset);

  PyObject* result = template_data(&ctx);
  delete_userdata(&ctx.user);
  return result;
}


static PyObject* unpacker_unpack(PyObject *self)
{
  Unpacker* unpacker = (Unpacker*)self;
	msgpack_unpack_t ctx;

  if(unpacker->size - unpacker->consumed == 0){
    PyErr_SetString(PyExc_StopIteration, "No more unpack data.");
    return NULL;
  }
  init_userdata(&ctx.user);
  template_init(&ctx);

  //printf("rest buffer:%zd bytes ", unpacker->size - unpacker->consumed);
  int before_consumed = unpacker->consumed;
  int executed = template_execute(&ctx, unpacker->data,
                   unpacker->size,
                   &unpacker->consumed);
  if(executed != 1){
    unpacker->consumed = before_consumed;
    PyErr_SetString(PyExc_StopIteration, "No more unpack data.");
    return NULL;
  }
  //printf("consumed %zd bytes\n", unpacker->consumed - before_consumed);
  //unpacker_dump(self);
  //printf("\n");

  PyObject* result = template_data(&ctx);
  if(unpacker->size == unpacker->consumed){
    unpacker->size = unpacker->consumed = 0;
  }
  delete_userdata(&ctx.user);

  return result;
}

#endif
