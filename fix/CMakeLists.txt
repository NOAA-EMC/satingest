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
  aerosol
  aerosol.af_viirs
  aerosol.avhrr_weekly_gvf
  aerosol.burned_area
  aerosol.burning_emissions
  aerosol.dust
  aerosol.modis_aod
  aerosol.modis_fire
  aerosol.nic_lks
  aerosol.sat_comp_mosaic_imgry
  aerosol.viirs_weekly_gvf
  aerosol.wf_abba
  airs
  airs.airs_efov
  airs.amsr2
  airs.iasi_efov
  airs.mt_saphir
  avhrr
  avhrr.avhrr_18_19
  avhrr.avhrr_m1_m2_m3
  bufr_V10satwind.4qual.bufrtable
  bufr_amsua15_ta2tb.txt
  bufr_amsua16_ta2tb.txt
  bufr_goes.cloud.bufrtable
  bufr_goes.imgrdn.bufrtable
  bufr_goes.sounding_1x1.bufrtable
  bufr_ompstc.bufrtable
  bufr_ompstc.bufrtable.no_corners
  bufrtab.003
  bufrtab.005
  bufrtab.008
  bufrtab.012
  bufrtab.021
  bufrtab.GOES16ASR_NESDIS
  bufrtab.GOES16CSR_NESDIS
  bufrtab_saphir_foreign
  idsb_ingest.source.html
  lnd_sea_mask_dat
  npp_atms
  npp_atms.atms
  npp_cris_2211
  npp_cris_2211.cris_fsr_2211
  npp_cris_431
  npp_cris_431.cris_fsr_431
  npp_omps
  npp_omps.omps_lp
  npp_omps.ompsnp_v8
  npp_omps.ompstc_v8
  ozone14
  ozone14.ozone19_v8bufr
  ozone_orbit
  ozone_orbit.gome
  ozone_orbit.omi
  ozone_orbit.ozone_v8bufr
  poes_sst
  poes_sst.amsr2_sst
  poes_sst.navy_avhrr
  poes_sst.viirs_clear_sky
  poes_sst.viirs_probclear_sky
  qscatter
  qscatter.ascat_50km
  radsnd
  radsnd.efclamzip
  radsnd.goes16_asr_base
  radsnd.goes_csr_base
  radsnd.lgycld
  satwnd
  satwnd.avhrr
  satwnd.cimss
  satwnd.ircd_dmw
  satwnd.leo-geo
  satwnd.modir
  satwnd.modwv
  satwnd.nb_viirs
  satwnd.shrtwv_dmw
  satwnd.visb_dmw
  satwnd.wvics_dmw
  satwnd.wvict_dmw
  snowday
  snowday.copy_forward_grib_16_ims
  snowday.copy_forward_grib_16_usaf_nh
  snowday.copy_forward_grib_16_usaf_sh
  snowday.copy_forward_grib_96_ims
  snowore
  snowore.ascii2grib_16_ims
  snowore.ascii2grib_96_ims
  snowore.gribcopy_autosnow_sh
  snowore.gribcopy_usaf_nh
  snowore.gribcopy_usaf_sh
  ssmi
  ssmi.edr
  ssmi.gmi_tmbr
  ssmi.sdr
  ssmi.ssmis
  tovs
  tovs.amsua
  tovs.atovs
  tovs.hirs4
  tovs.mhs
  )

list(APPEND fix_files
  ${CMAKE_CURRENT_BINARY_DIR}/fix/bufr_hirsrtcf_ibm.dat
  ${CMAKE_CURRENT_BINARY_DIR}/fix/bufr_lowtopog.dat
  ${CMAKE_CURRENT_BINARY_DIR}/fix/nesdis.lstags_transsmi
  )

# Install.
install(FILES ${fix_files}
        DESTINATION "fix")
