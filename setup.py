import setuptools
from setuptools.command.install import install
import os

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="PyGeopack",
    version="0.0.6",
    author="Matthew Knight James",
    author_email="mattkjames7@gmail.com",
    description="Geopack08 wrapper for Python",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/mattkjames7/PyGeopack",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: GNU General Public License (GPL)",
        "Operating System :: POSIX",
    ],
    install_requires=[
		'numpy',
	],
	include_package_data=True,
)



