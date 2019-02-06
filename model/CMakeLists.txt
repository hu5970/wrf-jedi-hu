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

ecbuild_add_library( TARGET   wrf-jedi

                     SOURCES  
                        ${oops_wrf_src_files}
                        
                     LIBS
                        oops
                        ufo

                     INSTALL_HEADERS LISTED

                     LINKER_LANGUAGE ${WRFJEDI_LINKER_LANGUAGE}
                    )

