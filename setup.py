from setuptools import setup, find_packages
from setuptools.command.build_py import build_py
import subprocess
import os
import platform
import shutil
from pathlib import Path


class CustomBuild(build_py):
    def run(self):
        build_py.run(self)
        self.execute(self.target_build, ())

    def target_build(self):
        geopack_dir = Path(__file__).parent / 'PyGeopack' / '__data' / 'geopack'
        build_dir = Path(__file__).parent / 'build' / 'geopack-cmake'
        lib_dir = Path(self.build_lib) / 'PyGeopack' / '__data' / 'geopack' / 'lib'
        lib_dir.mkdir(parents=True, exist_ok=True)

        rpath = '$ORIGIN'
        if platform.system() == 'Darwin':
            rpath = '@loader_path'

        configure_cmd = [
            'cmake',
            '-S',
            str(geopack_dir),
            '-B',
            str(build_dir),
            '-DGEOPACK_BUILD_TESTS=OFF',
            '-DGEOPACK_BUILD_SHARED=ON',
            '-DCMAKE_BUILD_TYPE=Release',
            f'-DCMAKE_INSTALL_PREFIX={geopack_dir}',
            f'-DCMAKE_BUILD_RPATH={rpath}',
            f'-DCMAKE_INSTALL_RPATH={rpath}',
            '-DCMAKE_BUILD_WITH_INSTALL_RPATH=ON',
        ]
        subprocess.check_call(configure_cmd)
        subprocess.check_call(['cmake', '--build', str(build_dir), '--config', 'Release'])

        expected = lib_dir / self._library_name()
        built = self._find_built_library(build_dir)
        if built.resolve() != expected.resolve():
            shutil.copy2(built, expected)

        for dependency in self._find_runtime_dependencies(build_dir):
            target = lib_dir / dependency.name
            if dependency.resolve() != target.resolve():
                shutil.copy2(dependency, target)

    def _library_name(self):
        if platform.system() == 'Windows':
            return 'libgeopack.dll'
        elif platform.system() == 'Darwin':
            return 'libgeopack.dylib'
        else:
            return 'libgeopack.so'

    def _shared_library_suffix(self):
        if platform.system() == 'Windows':
            return '.dll'
        elif platform.system() == 'Darwin':
            return '.dylib'
        else:
            return '.so'

    def _shared_library_glob(self):
        if platform.system() == 'Windows':
            return '*.dll'
        elif platform.system() == 'Darwin':
            return '*.dylib*'
        else:
            return '*.so*'

    def _find_built_library(self, build_dir):
        suffix = self._shared_library_suffix()
        candidates = [
            path for path in build_dir.rglob(f'*{suffix}')
            if path.stem in ('geopack', 'libgeopack')
        ]
        if not candidates:
            raise FileNotFoundError(f'Could not find built geopack shared library in {build_dir}')
        return candidates[0]

    def _find_runtime_dependencies(self, build_dir):
        expected_name = self._library_name()
        deps = []
        for path in build_dir.rglob(self._shared_library_glob()):
            if path.name == expected_name or path.stem in ('geopack', 'libgeopack'):
                continue
            deps.append(path)
        return deps


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
