list(APPEND def_files
  prod_satingest.def
  )

install(FILES       ${def_files}
        DESTINATION "ecflow")

list(APPEND ecf_files
  jsatingest_aerosol.ecf
  jsatingest_airs.ecf
  jsatingest_avhrr.ecf
  jsatingest_npp_atms.ecf
  jsatingest_npp_cris_431.ecf
  jsatingest_npp_omps.ecf
  jsatingest_ozone14.ecf
  jsatingest_ozone_orbit.ecf
  jsatingest_poes_sst.ecf
  jsatingest_qscatter.ecf
  jsatingest_radsnd.ecf
  jsatingest_satwnd.ecf
  jsatingest_snowday.ecf
  jsatingest_snowore.ecf
  jsatingest_ssmi.ecf
  jsatingest_tovs.ecf
  )

foreach(ecf_target ${ecf_files})
  install(PROGRAMS    satingest.ecf
          DESTINATION "ecflow"
          RENAME      ${ecf_target})
endforeach()
