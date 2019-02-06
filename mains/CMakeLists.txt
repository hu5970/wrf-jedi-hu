
ecbuild_add_executable( TARGET  wrfjedi_hofx.x

                        SOURCES
                            wrfjediHofX.cc

                        INCLUDES
                            ${wrfjedi_SOURCE_DIR}/model

                        LIBS
                            wrf-jedi
                            ${wrfplus_SOURCE_DIR}/frame/module_internal_header_util.o
                            ${wrfplus_SOURCE_DIR}/frame/pack_utils.o
                            wrflib
                            fftpack
                            io_grib1
                            io_grib_share
                            wrfio_int
                            wrfio_nf
                            esmf_time
                            rsl_lite
                        
                        LINKER_LANGUAGE ${WRFJEDI_LINKER_LANGUAGE}
                      )

ecbuild_add_executable( TARGET  wrfjedi_forecast.x

                        SOURCES 
                            wrfjediForecast.cc

                        INCLUDES
                            ${wrfjedi_SOURCE_DIR}/model

                        LIBS
                            wrf-jedi
                            ${wrfplus_SOURCE_DIR}/frame/module_internal_header_util.o
                            ${wrfplus_SOURCE_DIR}/frame/pack_utils.o
                            wrflib
                            fftpack
                            io_grib1
                            io_grib_share
                            wrfio_int
                            wrfio_nf
                            esmf_time
                            rsl_lite
                        
                        LINKER_LANGUAGE ${WRFJEDI_LINKER_LANGUAGE}
                      )
