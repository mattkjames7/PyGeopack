cmake_minimum_required(VERSION 3.21)

project(${SKBUILD_PROJECT_NAME} VERSION ${SKBUILD_PROJECT_VERSION} LANGUAGES NONE)

include(ExternalProject)

set(_pygeopack_source_dir "${CMAKE_CURRENT_SOURCE_DIR}/PyGeopack/__data/geopack")
set(_pygeopack_build_dir "${CMAKE_CURRENT_BINARY_DIR}/geopack-build")

set(_pygeopack_origin_path "")
set(_pygeopack_library_glob "")
if(WIN32)
  set(_pygeopack_library_glob "*.dll")
elseif(APPLE)
  set(_pygeopack_origin_path "@loader_path")
  set(_pygeopack_library_glob "*.dylib*")
elseif(UNIX)
  set(_pygeopack_origin_path "$ORIGIN")
  set(_pygeopack_library_glob "*.so*")
endif()

option(PYGEOPACK_GEOPACK_USE_OPENMP "Enable OpenMP in geopack dependency" ON)

set(_pygeopack_cmake_args
  -DGEOPACK_BUILD_TESTS=OFF
  -DGEOPACK_BUILD_SHARED=ON
  -DGEOPACK_USE_OPENMP=${PYGEOPACK_GEOPACK_USE_OPENMP}
  -DCMAKE_BUILD_TYPE=Release
)

if(NOT _pygeopack_origin_path STREQUAL "")
  list(APPEND _pygeopack_cmake_args
    "-DCMAKE_BUILD_RPATH=${_pygeopack_origin_path}"
    "-DCMAKE_INSTALL_RPATH=${_pygeopack_origin_path}"
    -DCMAKE_BUILD_WITH_INSTALL_RPATH=ON
  )
endif()

ExternalProject_Add(geopack_external
  SOURCE_DIR "${_pygeopack_source_dir}"
  BINARY_DIR "${_pygeopack_build_dir}"
  INSTALL_COMMAND ""
  BUILD_BYPRODUCTS "${_pygeopack_build_dir}"
  CMAKE_ARGS ${_pygeopack_cmake_args}
)

set(PYGEOPACK_BUILD_DIR "${_pygeopack_build_dir}")
set(PYGEOPACK_LIBRARY_GLOB "${_pygeopack_library_glob}")
set(_pygeopack_install_code [=[
file(GLOB_RECURSE _pygeopack_runtime_libs LIST_DIRECTORIES false
  "@PYGEOPACK_BUILD_DIR@/@PYGEOPACK_LIBRARY_GLOB@")
if(NOT _pygeopack_runtime_libs)
  message(FATAL_ERROR "No runtime libraries were produced in @PYGEOPACK_BUILD_DIR@")
endif()
file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/." TYPE FILE FILES ${_pygeopack_runtime_libs})
]=])
string(CONFIGURE "${_pygeopack_install_code}" _pygeopack_install_code @ONLY)
install(CODE "${_pygeopack_install_code}" COMPONENT python)