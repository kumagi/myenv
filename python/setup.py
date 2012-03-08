#!/usr/bin/env python
import os
from distutils.core import setup, Extension

msgpack_ext = Extension("msgpackya", ["msgpackya.c"],
                        depends = [],
                        libraries=[],
                        include_dirs=[".", "../"],
                        library_dirs=[],
                        define_macros=[],
                        extra_compile_args=["-Wstrict-aliasing=2","-fomit-frame-pointer" ]
                        )

setup(name="MessagePackYetAnother",
      version="0.1",
      description="Yet Another MessagePack serialize/deserialize extention",
      license="3-clause BSD <http://www.opensource.org/licenses/bsd-license.php>",
      author="Hiroki Kumazaki",
      author_email="hiroki.kumazaki@gmail.com",
      url="http://twitter.com/kumagi",
      ext_modules=[msgpack_ext],
      packages=["."])
