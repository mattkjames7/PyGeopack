import setuptools
from setuptools.command.install import install
import os
# import atexit

# def PostInstall():
	# #store the current working directory
	# CWD = os.getcwd()
		
	# #move into the libgeopack folder to make the module
	# print('PyGeopack: Running makefile')
	# os.chdir('PyGeopack/__data/libgeopack')
	# os.system('make')
		
	# #move into the datafolder to extract the giant data file
	# print('PyGeopack: Extracting model data')
	# os.chdir('data')
	# os.system('7z x -y TSdata.bin.tar.7z')
	# os.system('tar -xf TSdata.bin.tar')
		
	# #revert to original working directory
	# print('PyGeopack: reverting to original working directory')
	# os.chdir(CWD)		
	


# class CustomInstallCommand(install):
	# """Customized setuptools install command will hopefully decompress the data file and make the C++/Fortran code"""
	# def __init__(self, *args, **kwargs):
		# #go ahead with install
		# #install.run(self)
		# super(CustomInstallCommand, self).__init__(*args, **kwargs)

# #		atexit.register(PostInstall)



with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="PyGeopack",
    version="0.0.4",
    author="Matthew Knight James",
    author_email="mattkjames7@gmail.com",
    description="Geopack08 wrapper for Python",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/mattkjames7/PyGeopack",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    install_requires=[
		'numpy',
	],
	include_package_data=True,
	#cmdclass = { 'install':CustomInstallCommand },
)



