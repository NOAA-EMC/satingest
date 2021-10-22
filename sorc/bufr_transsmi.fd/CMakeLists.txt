if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -convert big_endian -list -fp-model strict -assume noold_ldout_format -assume byterecl -real-size 64")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -fdefault-real-8")
endif()

list(APPEND fortran_src
  transsmi.f
)

set(exe_name bufr_transsmi.x)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  bacio::bacio_4
  w3emc::w3emc_d
  bufr::bufr_d)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
