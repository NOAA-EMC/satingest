if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -real-size 64")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-real-8")
endif()

list(APPEND fortran_src
  sno16grb.f
)

set(exe_name snow_sno16grb.x)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  bacio::bacio_4
  w3emc::w3emc_d)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
