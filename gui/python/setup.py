from distutils.core import setup
import py2exe

setup(windows=["flas.py"],
      data_files=["id_rsa"],
      options={"py2exe":{"dist_dir": "bin",
                         "compressed": True,
                         "bundle_files": 1,
                         "optimize": 2}})
