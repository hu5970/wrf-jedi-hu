list( APPEND wrf_test_input
  testinput/hofx.json
  compare.sh
)

list( APPEND wrf_testoutput
  testoutput/hofx.test
)

# This line copies all files to binary dir

# Create Data directory for test input and symlink all files
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput)
foreach(FILENAME ${wrf_test_input})
    execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
           ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
           ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME} )
endforeach(FILENAME)

# Create Data directory for reference output and symlink all files
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testoutput)
foreach(FILENAME ${wrf_testoutput})
    execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
           ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
           ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME} )
endforeach(FILENAME)

ecbuild_add_resources( TARGET   wrf_test_scripts
                       SOURCES_PACK
                       ${wrf_test_input}
                     )

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Data)

#####################################################################

# Tests that create data other tests might use (should check what is really needed...)

# Test interface classes with WRF

# ecbuild_add_test( TARGET  test_wrf_geometry
#                   BOOST
#                   SOURCES executables/TestGeometry.cc
#                   ARGS    "testinput/interfaces.json"
#                   LIBS    wrf-jedi )

# ecbuild_add_test( TARGET  test_wrf_state
#                   BOOST
#                   SOURCES executables/TestState.cc
#                   ARGS    "testinput/interfaces.json"
#                   LIBS    wrf-jedi )

# ecbuild_add_test( TARGET  test_wrf_model
#                   BOOST
#                   SOURCES executables/TestModel.cc
#                   ARGS    "testinput/interfaces.json"
#                   LIBS    wrf-jedi )

# ecbuild_add_test( TARGET  test_wrf_increment
#                   BOOST
#                   SOURCES executables/TestIncrement.cc
#                   ARGS    "testinput/interfaces.json"
#                   LIBS    wrf-jedi )

ecbuild_add_test( TARGET  test_wrf_hofx
                  TYPE SCRIPT
                  COMMAND "compare.sh"
                  ARGS "${CMAKE_BINARY_DIR}/bin/wrfjedi_hofx.x testinput/hofx.json"
                       testoutput/hofx.test
                  DEPENDS wrfjedi_hofx.x )

