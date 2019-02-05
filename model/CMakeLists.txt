list( APPEND oops_wrf_src_files
ErrorCovarianceWRFJEDI.h
Fortran.h
GeometryWRFJEDI.h
IncrementWRFJEDI.h
LocalizationMatrixWRFJEDI.h
ModelBiasCovarianceWRFJEDI.h
ModelBiasIncrementWRFJEDI.h
ModelBiasWRFJEDI.h
ModelWRFJEDI.h
RunWRFJEDI.h
StateWRFJEDI.h
WRFJEDITraits.h
RunWRFJEDI.cc
GeometryWRFJEDI.cc
ModelWRFJEDI.cc
StateWRFJEDI.cc
FieldsWRFJEDI.cc
wrfjedi_kinds_mod.F90
wrfjedi_run_mod.F90
#wrfjedi_init.f90
#wrfjedi_run.f90
wrfjedi_finalize.f90
wrfjedi_module_integrate.f90
wrfjedi_geom_mod.F90
wrfjedi_geom_interface.F90
wrfjedi_model_mod.F90
wrfjedi_model_interface.F90
wrfjedi_fields_mod.F90
wrfjedi_fields_interface.F90
wrfjedi_trajectories.F90
wrfjedi_getvaltraj_mod.F90
wrfjedi_derived_types.F90
pool_hash.c
wrfjedi_pool_routines.F90
wrfjedi4da_mod.F90
)

#SET( WRFJEDI_INCLUDES ${WRF_MODEL_PATH}/external/esmf_time_f90 ${WRF_MODEL_PATH}/external/io_int)
#list(APPEND WRFJEDI_INCLUDES ${WRF_MODEL_PATH}/main ${WRF_MODEL_PATH}/frame ${WRF_MODEL_PATH}/share)
#
#set(WRFJEDI_LIBRARIES)
#SET(WRF_LIBS_EXTERNAL ${WRF_MODEL_PATH}/external/fftpack/fftpack5/libfftpack.a ${WRF_MODEL_PATH}/external/io_grib1/libio_grib1.a ${WRF_MODEL_PATH}/external/io_grib_share/libio_grib_share.a ${WRF_MODEL_PATH}/external/io_int/libwrfio_int.a ${WRF_MODEL_PATH}/external/esmf_time_f90/libesmf_time.a ${WRF_MODEL_PATH}/external/RSL_LITE/librsl_lite.a)
#SET(WRF_LIBS_NF -L${WRF_MODEL_PATH}/external/io_netcdf -lwrfio_nf -L$ENV{NETCDF}/lib -lnetcdff -lnetcdf)
#
#list(APPEND WRFJEDI_LIBRARIES -L${WRF_MODEL_PATH}/main -lwrflib)
#list(APPEND WRFJEDI_LIBRARIES ${WRF_LIBS_EXTERNAL})
#list(APPEND WRFJEDI_LIBRARIES ${WRF_MODEL_PATH}/frame/module_internal_header_util.o ${WRF_MODEL_PATH}/frame/pack_utils.o ${WRF_LIBS_NF})

ecbuild_add_library( TARGET   wrf-jedi
                     SOURCES  ${oops_wrf_src_files}
                     LIBS     oops ufo wrflib
                     INSTALL_HEADERS LISTED
                     LINKER_LANGUAGE ${WRFJEDI_LINKER_LANGUAGE}
                    )

