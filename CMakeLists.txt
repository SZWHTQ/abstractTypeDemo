cmake_minimum_required(VERSION 3.12)

project(
    abstractTypeTest
    LANGUAGES Fortran
)

add_executable(main main.f90 abstractTypeModule.f90)

set_target_properties(
    main
    PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)