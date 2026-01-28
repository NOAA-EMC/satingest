# Set URL, sha256sum and tar name of the file to download.
set(URL "https://ftp.emc.ncep.noaa.gov/static_files/public/obsproc")
set(SHA "867fd8c2ca1f87d5207fab0e1f9ea5ad8e7154aecc2df58dcde483c41dbaef5c")
string(SUBSTRING ${SHA} 0 6 SHORTSHA)
set(TAR "satingest-fix-${SHORTSHA}.tgz") # poor-man's version control

# Download binary fix files and check SHA256
file(DOWNLOAD
  ${URL}/${TAR}
  ${CMAKE_CURRENT_BINARY_DIR}/${TAR}
  INACTIVITY_TIMEOUT 30
  TIMEOUT 90
  SHOW_PROGRESS
  STATUS status
  EXPECTED_HASH SHA256=${SHA}
  )

# Extract downloaded tarball.
file(ARCHIVE_EXTRACT INPUT ${TAR})

# Collect list of fix files to install.
list(APPEND fix_files
bufr_amsua15_ta2tb.txt
bufr_goes.cloud.bufrtable              
bufr_goes.sounding_1x1.bufrtable       
bufr_ompstc.bufrtable.no_corners       
bufrtab.003                            
bufrtab.005                            
bufrtab.008                            
bufrtab.012                            
bufrtab.021                            
bufrtab.GOES16ASR_NESDIS               
bufrtab.GOES16CSR_NESDIS               
bufrtab_saphir_foreign                 
bufr_amsua16_ta2tb.txt                 
bufr_goes.imgrdn.bufrtable             
bufr_ompstc.bufrtable                  
bufr_V10satwind.4qual.bufrtable        
idsb_ingest.source.html                
lnd_sea_mask_dat                       
t_aerosol_af_viirs
t_aerosol_af_viirs.f
t_aerosol_avhrr_weekly_gvf
t_aerosol_avhrr_weekly_gvf.f
t_aerosol_modis_fire
t_aerosol_modis_fire.f
t_aerosol_nic_lks
t_aerosol_nic_lks.f
t_aerosol_viirs_weekly_gvf
t_aerosol_viirs_weekly_gvf.f
t_aerosol_wf_abba
t_aerosol_wf_abba.f
t_airs_amsr2
t_airs_amsr2.f
t_airs_amsr2_GSI
t_airs_amsr2_GSI.f
t_airs_iasi_efov
t_airs_iasi_efov.f
t_avhrr_avhrr_18_19
t_avhrr_avhrr_18_19.f
t_avhrr_avhrr_m1_m2_m3
t_avhrr_avhrr_m1_m2_m3.f
t_hrsatwind_cimss
t_hrsatwind_cimss.f
t_hrsatwind_circd_dmw
t_hrsatwind_circd_dmw.f
t_hrsatwind_cshrtwv_dmw
t_hrsatwind_cshrtwv_dmw.f
t_hrsatwind_cvisb_dmw
t_hrsatwind_cvisb_dmw.f
t_hrsatwind_cwvics_dmw
t_hrsatwind_cwvics_dmw.f
t_hrsatwind_cwvict_dmw
t_hrsatwind_cwvict_dmw.f
t_npp_atms
t_npp_atms.f
t_npp_cris_2211_cris_fsr_2211
t_npp_cris_2211_cris_fsr_2211.f
t_npp_cris_431_cris_431
t_npp_cris_431_cris_431.f
t_npp_omps_omps_lp
t_npp_omps_omps_lp.f
t_npp_omps_ompsnp_v8
t_npp_omps_ompsnp_v8.f
t_npp_omps_ompstc_v8
t_npp_omps_ompstc_v8.f
t_ozone_orbit_gome
t_ozone_orbit_gome.f
t_poes_sst_amsr2_sst
t_poes_sst_amsr2_sst.f
t_poes_sst_navy_avhrr
t_poes_sst_navy_avhrr.f
t_poes_sst_viirs_clear_sky
t_poes_sst_viirs_clear_sky.f
t_poes_sst_viirs_probclear_sky
t_poes_sst_viirs_probclear_sky.f
t_qscatter_ascat_50km
t_qscatter_ascat_50km.f
t_radsnd_efclamzip
t_radsnd_efclamzip.f
t_radsnd_goes_asr_base
t_radsnd_goes_asr_base.f
t_radsnd_goes_csr_base
t_radsnd_goes_csr_base.f
t_radsnd_lgycld
t_radsnd_lgycld.f
t_satwnd_avhrr
t_satwnd_avhrr.f
t_satwnd_ircd_dmw
t_satwnd_ircd_dmw.f
t_satwnd_leogeo
t_satwnd_leogeo.f
t_satwnd_nb_avhrr
t_satwnd_nb_avhrr.f
t_satwnd_nb_viirs
t_satwnd_nb_viirs.f
t_satwnd_shrtwv_dmw
t_satwnd_shrtwv_dmw.f
t_satwnd_visb_dmw
t_satwnd_visb_dmw.f
t_satwnd_wvics_dmw
t_satwnd_wvics_dmw.f
t_satwnd_wvict_dmw
t_satwnd_wvict_dmw.f
t_snowday_copy_forward_grib_16_ims
t_snowday_copy_forward_grib_16_ims.f
t_snowday_copy_forward_grib_96_ims
t_snowday_copy_forward_grib_96_ims.f
t_snowore_ascii2grib_16_ims
t_snowore_ascii2grib_16_ims.f
t_snowore_ascii2grib_96_ims
t_snowore_ascii2grib_96_ims.f
t_snowore_gribcopy_autoice_nh
t_snowore_gribcopy_autoice_nh.f
t_snowore_gribcopy_autoice_sh
t_snowore_gribcopy_autoice_sh.f
t_snowore_gribcopy_autosnow_nh
t_snowore_gribcopy_autosnow_nh.f
t_snowore_gribcopy_autosnow_sh
t_snowore_gribcopy_autosnow_sh.f
t_ssmi_gmi_tmbr
t_ssmi_gmi_tmbr.f
t_ssmi_ssmis
t_ssmi_ssmis.f
t_tovs_amsua
t_tovs_amsua.f
t_tovs_atovs
t_tovs_atovs.f
t_tovs_hirs4
t_tovs_hirs4.f
t_tovs_mhs
t_tovs_mhs.f
  )

list(APPEND fix_files
  ${CMAKE_CURRENT_BINARY_DIR}/fix/bufr_hirsrtcf_ibm.dat
  ${CMAKE_CURRENT_BINARY_DIR}/fix/bufr_lowtopog.dat
  ${CMAKE_CURRENT_BINARY_DIR}/fix/nesdis.lstags_transsmi
  )

# Install. 
install(FILES ${fix_files}
        DESTINATION "fix")
