if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -integer-size 32")
endif()

list(APPEND fortran_src
  sno96grb.f
)

set(exe_name snow_sno96grb.x)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  bacio::bacio_4
  w3emc::w3emc_4)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
