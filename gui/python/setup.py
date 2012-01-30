from distutils.core import setup
import py2exe

setup(
    windows=["flas.py"],
    options={
        "py2exe":{
            #"includes":[
            #    "PySide.QtCore",
            #    "PySide.QtGui",
            #    "PySide.QtSql"
            #],
            "dist_dir": "dist",
            "compressed": True,
            "bundle_files": 1,
            "optimize": 2
        }
    }
)
