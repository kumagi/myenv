import msgpack
import msgpackya
import random
# pack: 20.0% PyUnicodeUCS2_EncodeUTF8
# packya: 30.8% PyUnicodeUCS2_EncodeUTF8
import time

def pack_time(target, times):
  begin = time.time()
  for i in range(times):
    msgpackya.packb(target)
  ya = time.time() - begin
  begin = time.time()
  for i in range(times):
    msgpack.packb(target)
  org = time.time() - begin
  #return None
  return ("ya :"+str(ya), "org:"+str(org))
elapsed = pack_time([], 1000)
print("empty_list:",elapsed)

elapsed = pack_time([random.randint(0,x+1) for x in range(0,100000)], 100)
print("100000_list:",elapsed)

dict_example = {}
for x in [x for x in range(0,1000)]:
  dict_example[x] = x*x
elapsed = pack_time(dict_example, 100)
print("10000_dict:",elapsed)

long_string = "a"*10000000
elapsed = pack_time(long_string, 100)
print("long_string:",elapsed)

short_string_list = ["x"*random.randint(100,200) for x in range(10000)]
elapsed = pack_time(short_string_list,100)
print("short_string_list:",elapsed)
