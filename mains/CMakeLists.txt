
ecbuild_add_executable( TARGET  wrfjedi_hofx.x
                        SOURCES wrfjediHofX.cc
                        LIBS    wrf-jedi
                      )

ecbuild_add_executable( TARGET  wrfjedi_forecast.x
                        SOURCES wrfjediForecast.cc
                        LIBS    wrf-jedi
                      )
