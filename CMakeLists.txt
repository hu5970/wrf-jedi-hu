# (C) Copyright 2017 UCAR.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

################################################################################
# WRFJEDI
################################################################################

cmake_minimum_required( VERSION 3.3.2 FATAL_ERROR )

project( wrfjedi C CXX Fortran )

set( CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake;${CMAKE_MODULE_PATH})

set( ECBUILD_DEFAULT_BUILD_TYPE Release )
set( ENABLE_OS_TESTS           OFF CACHE BOOL "Disable OS tests" FORCE )
set( ENABLE_LARGE_FILE_SUPPORT OFF CACHE BOOL "Disable testing of large file support" FORCE )
set( ENABLE_MPI ON CACHE BOOL "Compile with MPI" )

include( ecbuild_system NO_POLICY_SCOPE )

ecbuild_requires_macro_version( 2.9 )

################################################################################
# Project
################################################################################

ecbuild_declare_project()

ecbuild_enable_fortran( REQUIRED )
ecbuild_add_cxx11_flags()

set( WRFJEDI_LINKER_LANGUAGE CXX )

################################################################################
# Dependencies
################################################################################

# Boost
set( Boost_MINIMUM_VERSION "1.47" )
find_package( Boost ${Boost_MINIMUM_VERSION} REQUIRED
              COMPONENTS unit_test_framework )
include_directories( ${Boost_INCLUDE_DIR} )

# NetCDF
set( NETCDF_F90 ON CACHE BOOL "Compile with Fortran NetCDF" )
find_package( NetCDF REQUIRED )
include_directories( ${NETCDF_INCLUDE_DIR} )

# MPI
ecbuild_add_option( FEATURE MPI DEFAULT ON
                    DESCRIPTION "Support for MPI distributed parallelism"
                    REQUIRED_PACKAGES "MPI COMPONENTS CXX Fortran" )


#if ( NOT ECKIT_HAVE_MPI )
#  ecbuild_error("OOPS requested to build with MPI but eckit wasn't built with MPI: ECKIT_HAVE_MPI = ${ECKIT_HAVE_MPI}")
#endif()

#if( HAVE_MPI )
  set( OOPS_MPI_C_LIBRARIES            ${MPI_CXX_LIBRARIES} )
  ecbuild_include_mpi()
  link_libraries(${MPI_CXX_LIBRARIES} ${MPI_Fortran_LIBRARIES})
#endif()

#
find_package( LibXml2 REQUIRED )
include_directories( ${LIBXML2_INCLUDE_DIR} )


# OpenMP
ecbuild_add_option( FEATURE OMP
                    DESCRIPTION "Support for OpenMP threaded parallelism"
                    REQUIRED_PACKAGES "OMP COMPONENTS CXX Fortran" )

if( HAVE_OMP )
  ecbuild_enable_omp()
else()
  ecbuild_enable_ompstubs()
endif()

# oops
ecbuild_use_package( PROJECT oops VERSION 0.2.1 REQUIRED )
include_directories( ${OOPS_INCLUDE_DIRS} )

# ufo
ecbuild_use_package( PROJECT ufo )
include_directories( ${UFO_INCLUDE_DIRS} )

# ioda
ecbuild_use_package( PROJECT ioda )
include_directories( ${IODA_INCLUDE_DIRS} )

# wrf+
if ( NOT DEFINED WRFPLUS_PATH AND DEFINED ENV{WRFPLUS_PATH} )
  set ( WRFPLUS_PATH $ENV{WRFPLUS_PATH} )
  set ( wrfplus_SOURCE_DIR $ENV{WRFPLUS_PATH} )
  message( STATUS "Using pre-compiled WRFPLUS : " ${WRFPLUS_PATH} )
endif()

if ( DEFINED WRFPLUS_PATH ) 
  set ( wrfplus_SOURCE_DIR ${WRFPLUS_PATH} )
endif()

if ( NOT DEFINED WRFPLUS_PATH )
  include( DownloadProject )
  download_project(PROJ                wrfplus
                   GIT_REPOSITORY      https://github.com/JCSDA/WRF.git
                   GIT_TAG             feature/tlad
                   ${UPDATE_DISCONNECTED_IF_AVAILABLE}
  )
endif()

include_directories( ${wrfplus_SOURCE_DIR}/external/esmf_time_f90
                     ${wrfplus_SOURCE_DIR}/external/io_int
                     ${wrfplus_SOURCE_DIR}/main
                     ${wrfplus_SOURCE_DIR}/frame 
                     ${wrfplus_SOURCE_DIR}/share)

link_directories( 
    ${wrfplus_SOURCE_DIR}/main
    ${wrfplus_SOURCE_DIR}/external/fftpack/fftpack5
    ${wrfplus_SOURCE_DIR}/external/io_grib1
    ${wrfplus_SOURCE_DIR}/external/io_grib_share
    ${wrfplus_SOURCE_DIR}/external/io_int
    ${wrfplus_SOURCE_DIR}/external/io_netcdf
    ${wrfplus_SOURCE_DIR}/external/esmf_time_f90 
    ${wrfplus_SOURCE_DIR}/external/RSL_LITE
 )

################################################################################
# Definitions
################################################################################

# Additional user supplied compiler definitions

include( wrfjedi_compiler_flags )

################################################################################
# Export package info
################################################################################

list( APPEND WRFJEDI_TPLS NetCDF ioda ufo oops wrfplus)

set( WRFJEDI_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}
                          ${CMAKE_CURRENT_BINARY_DIR}
                          ${CMAKE_Fortran_MODULE_DIRECTORY} )

set( WRFJEDI_LIBRARIES wrfjedi )

get_directory_property( WRFJEDI_DEFINITIONS COMPILE_DEFINITIONS )

foreach( _tpl ${WRFJEDI_TPLS} )
  string( TOUPPER ${_tpl} TPL )
  list( APPEND WRFJEDI_EXTRA_DEFINITIONS   ${${TPL}_DEFINITIONS}  ${${TPL}_TPL_DEFINITIONS}  )
  list( APPEND WRFJEDI_EXTRA_INCLUDE_DIRS  ${${TPL}_INCLUDE_DIRS} ${${TPL}_TPL_INCLUDE_DIRS} )
  list( APPEND WRFJEDI_EXTRA_LIBRARIES     ${${TPL}_LIBRARIES}    ${${TPL}_TPL_LIBRARIES}    )
endforeach()

################################################################################
# Sources
################################################################################

include_directories( ${WRFJEDI_EXTRA_INCLUDE_DIRS} )

add_subdirectory( model )
add_subdirectory( mains )
add_subdirectory( test )

################################################################################
# Finalise configuration
################################################################################

# prepares a tar.gz of the sources and/or binaries
ecbuild_install_project( NAME wrfjedi )

# print the summary of the configuration
ecbuild_print_summary()