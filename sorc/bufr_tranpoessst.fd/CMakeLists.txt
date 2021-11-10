if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -integer-size 32 -real-size 64")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-real-8")
endif()

list(APPEND fortran_src
  tranpoessst.f
)

set(exe_name bufr_tranpoessst)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  w3emc::w3emc_d
  bufr::bufr_d)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
