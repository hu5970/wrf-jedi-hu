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
)

set ( WRFJEDI_LIBRARIES $ENV{WRFJEDI_LIBRARIES} )
set ( WRFJEDI_INCLUDES  $ENV{WRFJEDI_INCLUDES} )

ecbuild_add_library( TARGET   wrf-jedi
                     SOURCES  ${oops_wrf_src_files}
                     LIBS     oops ufo ${WRFJEDI_LIBRARIES}
                     INCLUDES ${WRFJEDI_INCLUDES}
                     INSTALL_HEADERS LISTED
                     LINKER_LANGUAGE ${OOPS_LINKER_LANGUAGE}
                    )

