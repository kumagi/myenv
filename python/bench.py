import random
import time
import sys

mode=sys.argv[1]

def pack_time(target, times):
  ya = org = None
  if(mode == "pypy"):
    import msgpackya
    packya_packb = msgpackya.packb
    begin = time.time()
    for i in range(times):
      packya_packb(target)
    ya = time.time() - begin
  if(mode == "pure"):
    import msgpack_pure
    packya_packb = msgpack_pure.packb
    begin = time.time()
    for i in range(times):
      packya_packb(target)
    ya = time.time() - begin
  if(mode == "kya" or mode == "comp"):
    import msgpackya
    packya_packb = msgpackya.packb
    begin = time.time()
    for i in range(times):
      packya_packb(target)
    ya = time.time() - begin

  if(mode == "orig" or mode == "comp"):
    begin = time.time()
    import msgpack
    pack_packb = msgpack.packb
    for i in range(times):
      pack_packb(target)
    org = time.time() - begin
  if(mode == "comp"):
     return ("msgpackya :"+str(ya), "cython:"+str(org))
  return ya or org
def unpack_time(target, times):
  ya = org = None
  if(mode == "kya" or mode == "comp"):
    import msgpackya
    packed = msgpackya.packb(target)
    packya_unpackb = msgpackya.unpackb
    begin = time.time()
    for i in range(times):
      packya_unpackb(packed)
    ya = time.time() - begin

  if(mode == "orig" or mode == "comp"):
    import msgpack
    packed = msgpack.packb(target)
    pack_packb = msgpack.packb
    begin = time.time()
    for i in range(times):
      pack_packb(target)
    org = time.time() - begin
  if(mode == "comp"):
     return ("msgpackya :"+str(ya), "cython:"+str(org))
  return ya or org

int_list = [x for x in range(10000)]
dict_example = {}
for x in [x for x in range(0,10000)]:
  dict_example[x] = x*x
bigstring = "a"*100000000
shortstring = ["x"*random.randint(100,200) for x in range(10000)]
items = {"empty":[[], 100000],
         "int":[int_list, 10],
         "dict":[dict_example, 100],
         "bigstring":[bigstring, 1],
         "shortstring":[shortstring, 10]}

item = list(set(items.keys()).intersection(set(sys.argv)))[0]
pack_elapsed = pack_time(items[item][0], items[item][1])
unpack_elapsed = unpack_time(items[item][0], items[item][1])
print("pack:"+item+":", pack_elapsed)
print("unpack:"+item+":", unpack_elapsed)
