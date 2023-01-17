set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# NAVY AVHRR SST Retrievals and Tb/Albedo (NOAA- and METOP- series satellites)
#  --> Physical SST Retrievals
#  (see http://www.emc.ncep.noaa.gov/mmb/data_processing/satellite_ingest.doc/document.htm#poessst
#   for satellites)
# Removed NOAA-18 5/2018 since NAVOCEANO will cease the production and distribution of MCSST
# products derived from NOAA-18 GAC data on May 14, 2018.
# ----------------------------------------------------------------------------
      eval MACHINE${n}=\${MACHINE${n}:-wcoss_navosst@ftps-out1.ncep.noaa.gov}
      eval ORBITLIST${n}=\${ORBITLIST${n}:-navysst.orbitlist}
      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
mcsst_mtc_d*tmp \
mcsst_mtb_d*tmp"'} 
#mcsst_npp_d*tmp \
#mcsst_n20_d*tmp"'}
      eval TANKFILE${n}=\${TANKFILE${n}:-b012/xx012}
      eval EXECUTE${n}=\${EXECUTE${n}:-ingest_transst_poes_navo.sh}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.012}
      eval HISTLENMIN${n}=\${HISTLENMIN${n}:-3500}
      eval HISTLENMAX${n}=\${HISTLENMAX${n}:-3800}
      eval CRITICAL${n}=\${CRITICAL${n}:-YES}



echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
