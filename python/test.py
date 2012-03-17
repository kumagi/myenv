# -*- Mode: Python; tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil; compile-command: "tox" -*-
# -*- coding: utf-8 -*-
import sys
import inspect
import msgpackya
import msgpack

def is_py3():
    return sys.hexversion >=  0x3000000

def dump_byte(byte):
  result = "0x"
  if(is_py3()):
    for b in byte:
      if(b < 16):
        result += "0"
      result += str(hex(b))[2:]
  else:
    for b in byte:
      if(ord(b) < 16):
        result += "0"
      result += str(hex(ord(b)))[2:]
  return result

def check_packb(arg):
  packed = msgpackya.packb(arg)
  target = msgpack.packb(arg)
  if(target == packed):
    return True
  else:
    print("error: " + str(arg) +
          " expected:" + str(dump_byte(target)) +
          "\tactually:" + str(dump_byte(packed)))
    return False

def check_packb_and_unpack(arg):
  a,b = [msgpack, msgpackya]
  c,d = [msgpack.unpackb, msgpackya.unpackb]
  target = a.packb(arg)
  packed = b.packb(arg)
  if(target == packed
     and c(target) ==  c(packed)
     ==  d(target) ==  d(packed)):
    return True
  else:
    print("error: " + str(arg) +
          " expected:" + str(a.unpackb(target)) +
          "\tactually:" + str(b.unpackb(packed)))
    return False

def pack_check(num):
  print(str(num)+":"+dump_byte(msgpackya.packb(num)))

# all char
char_range = [x for x in range(-128, 127)]

# edge in integer
int_edges = []
def edge_array(middle, error):
  return [x + middle for x in range(-error, error)]
int_errors = 4
for edge in [1<<7, 1<<8, 1<<15, 1<<16, 1<<31, 1<<32, (1<<63)-int_errors]:
  int_edges += edge_array(edge, int_errors)
  int_edges += edge_array(-edge, int_errors)
int_edges += edge_array(1<<64-int_errors, int_errors)

# random float
random_floats = []
def small_float():
  import random
  denominator = random.randint(1, 1000000)
  return 1.0 / denominator
def big_float():
  import random
  denominator = random.randint(1, 10)
  numerator = random.randint(0, 10000000000)
  return numerator / denominator
random_floats += [small_float() for _ in range(100)]
random_floats += [big_float() for _ in range(1000)]
random_floats += [1.0, 0.0, -1.0]

# random string
random_string = []
def random_short_string():
  ascii_charactor = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!@#$%^&*()_+{}:"|;<>?/,.[]=_'
  import random
  length = random.randint(1, 1000)
  return ''.join(random.choice(ascii_charactor) for _ in range(1, length))
def random_long_string():
  ascii_charactor = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!@#$%^&*()_+{}:"|;<>?/,.[]=_'
  import random
  length = random.randint(1000, 100000)
  return ''.join(random.choice(ascii_charactor) for _ in range(1000, length))
#random_string += [random_short_string() for _ in range(100)]
#random_string += [random_long_string() for _ in range(100)]
random_string += ["hhdf"]
random_string += ["こんにちは"]
#random_string += [u"こんにちは"]

# list
some_list = []
some_list += [ [1,2,3] ]
some_list += [[]]
some_list += [[3,45,2],[2.321,3,3,[32]]]

# tuple
some_tuple = ([32], 2, 4.2, [32,3,3.2])
empty_tuple = ()
str_tuple = ("hoge",3,322,"aaa", "ds")
test_tuple = [some_tuple, empty_tuple]

# dict
some_dict = []
some_dict += [{3:5}, {(32,5): 5, 8:4, (3,9):2}, {"hoge":2,3:43.2,"fuga":"auaua"}]

# testset
testset  = char_range + int_edges + random_floats + random_string
testset += [True, False, None] + some_list + test_tuple + some_dict
for i in testset:
  #print("checking:"+str(i))
  check_packb(i)
print("pack test done.")
for i in testset:
  check_packb_and_unpack(i)
  pass
print("unpack test done.")

target = testset

packed_buffer = bytes()
p = msgpackya.Packer()
for i in target:
  packed_buffer += p.pack(i)

expected_packed_buffer = bytes()
ep = msgpack.Packer()
for i in target:
  expected_packed_buffer += ep.pack(i)

assert(packed_buffer == expected_packed_buffer)
print("Packer ok")

# stream unpacker
## expect raising
unpackyer = msgpackya.Unpacker()
unpacker = msgpack.Unpacker()
unpackyer.feed(p.pack('auau'))
unpackyer.feed(p.pack('auau'))
try:
  while True:
    unpackyer.unpack()
except StopIteration:
  pass

print("auau ok")
unpackyer.feed(packed_buffer)
unpacker.feed(packed_buffer)
for actually in unpackyer:
  try:
    expected = unpacker.unpack()
    assert(expected == actually)
  except AssertionError:
    print("expected:" + str(expected) + "\nactually:" + str(actually))
unpackyer.feed(packed_buffer)
unpacker.feed(packed_buffer)
for expect in unpacker:
  try:
    unpacked = unpackyer.unpack()
    assert(expect == unpacked)
  except AssertionError:
    print("expected:" + str(expect) + "\nactually:" + str(unpacked))
print("stream Unpacker ok")

unpackyer.feed(packed_buffer)
unpacker.feed(packed_buffer)
assert(tuple(unpacker) == tuple(unpackyer))
print("Unpacker to tuple ok")
