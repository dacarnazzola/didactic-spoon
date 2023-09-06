# explicitly name all files in src/ directory
set(MODULE_FILES 
    constants_m.f90
    random_m.f90
    statistics_m.f90
   )

# compile tests executables
add_executable(test_random ${MODULE_FILES} test_random.f90)
