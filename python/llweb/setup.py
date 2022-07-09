#!/usr/bin/env python

import os.path
import sys

from distutils.core import setup, Extension

PACKAGE_NAME = 'llweb'
LLWEB_VERSION = '0.1.1'
LLWEB_SOURCE = 'llweb'
CLASSIFIERS = """\
Development Status :: 4 - Beta
Intended Audience :: Developers
License :: OSI Approved :: MIT
Programming Language :: Python
Programming Language :: C
Topic :: Software Development
Topic :: Software Development :: Libraries :: Python Modules
Operating System :: Microsoft :: Windows
Operating System :: Unix
"""

setup(
    name=PACKAGE_NAME,
    version=LLWEB_VERSION,
    author='Phoenix',
    author_email='phoenix@secondlife.com',
    url='http://bitbucket.org/phoenix_linden/llweb/',
    description='LLSD web infrastructure components',
    platforms=["any"],
    package_dir={PACKAGE_NAME:LLWEB_SOURCE},
    packages=[PACKAGE_NAME],
    license='MIT',
    classifiers=filter(None, CLASSIFIERS.split("\n")),
    #requires=['llbase', 'eventlet'],
    )
