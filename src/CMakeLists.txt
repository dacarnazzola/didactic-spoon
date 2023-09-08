# explicitly name all files in src/ directory
set(MODULE_FILES 
    atmosphere_m.f90
    cartesian_system_m.f90
    constants_m.f90
    random_m.f90
    statistics_m.f90
    vector_math_m.f90
   )

# compile source files to lib_modules
add_library(lib_modules STATIC ${MODULE_FILES})

# compile tests executables
add_executable(test_atmosphere test_atmosphere.f90)
target_link_libraries(test_atmosphere lib_modules)

add_executable(test_cartesian_system test_cartesian_system.f90)
target_link_libraries(test_cartesian_system lib_modules)

add_executable(test_random test_random.f90)
target_link_libraries(test_random lib_modules)
