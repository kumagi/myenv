#!/usr/bin/env python
import os
from distutils.core import setup, Extension

msgpackb = Extension("msgpackya",
                        sources=["msgpackya.c"],
                        depends = ["packer.h", "pack.h",
                                   "unpacker.h", "unpack.h",
                                   "alt_malloc.h", "python_switch.h"],
                        libraries=[],
                        include_dirs=[],
                        library_dirs=[],
                        define_macros=[],
                        extra_compile_args=["-Wstrict-aliasing=2", "-O3"]
                        )

setup(name="MessagePackYetAnother",
      version="0.1",
      description="Yet Another MessagePack serialize/deserialize extention",
      license="3-clause BSD <http://www.opensource.org/licenses/bsd-license.php>",
      author="KUMAZAKI Hiroki",
      author_email="hiroki.kumazaki@gmail.com",
      url="http://twitter.com/kumagi",
      ext_modules=[msgpackb],
      packages=["."])
