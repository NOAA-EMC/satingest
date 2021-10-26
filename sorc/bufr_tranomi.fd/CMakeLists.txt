if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -convert big_endian -list -auto -assume noold_ldout_format")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian")
endif()

list(APPEND fortran_src
  tranomi.f90
  to3_datasets.f90
)

set(exe_name bufr_tranomi)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  w3emc::w3emc_4
  bufr::bufr_4)
  #hdf5::hdf5 hdf5::hdf5_fortran)
target_include_directories(${exe_name} PRIVATE "${HDF5_Fortran_INCLUDE_DIRS}")
target_link_libraries(${exe_name} ${HDF5_Fortran_LIBRARIES})

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
