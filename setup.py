from setuptools.command.install import install
from setuptools import setup, find_packages
from setuptools.command.build_py import build_py
import subprocess
import os
import platform


class CustomBuild(build_py):
    def run(self):
        self.execute(self.target_build, ())
        build_py.run(self)

    def target_build(self):
        if platform.system() == 'Windows':
            cwd = os.getcwd()
            os.chdir('PyGeopack/__data/geopack/')
            subprocess.check_call(['cmd','/c','compile.bat'])
            os.chdir(cwd)
            #subprocess.check_call(['cmd', '/c', 'testmodule2/__data/geopack/compile.bat'])
        else:
            subprocess.check_call(['make', '-C', 'PyGeopack/__data/geopack'])


with open("README.md", "r") as fh:
    long_description = fh.read()

def getversion():
	'''
	read the version string from __init__
	
	'''
	#get the init file path
	thispath = os.path.abspath(os.path.dirname(__file__))+'/'
	initfile = thispath + 'PyGeopack/__init__.py'
	
	#read the file in
	f = open(initfile,'r')
	lines = f.readlines()
	f.close()
	
	#search for the version
	version = 'unknown'
	for l in lines:
		if '__version__' in l:
			s = l.split('=')
			version = s[-1].strip().strip('"').strip("'")
			break
	return version
	
version = getversion()

setup(
    name="PyGeopack",
    version=version,
    author="Matthew Knight James",
    author_email="mattkjames7@gmail.com",
    description="Geopack08 wrapper for Python",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/mattkjames7/PyGeopack",
    packages=find_packages(),
    package_data={'testmodule2': ['**/*']},
    cmdclass={'build_py': CustomBuild},  
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: GNU General Public License (GPL)",
        "Operating System :: POSIX",
    ],
    install_requires=[
		'numpy',
		'PyFileIO',
		'RecarrayTools',
		'DateTimeTools>=1.1.0',
		'kpindex>=1.0.1',
		'pyomnidata>=1.0.1',
	],
	include_package_data=True,
)



