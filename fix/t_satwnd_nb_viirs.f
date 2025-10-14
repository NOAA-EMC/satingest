set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# The VIIRS IR/Long-Wave Wind Family (VIIRS) (NPP & NOAA-20 satellites)
# ----------------------------------------------------------
      eval ORBITLIST${n}=\${ORBITLIST${n}:-poeswind.viirs.infrared.list}
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-thecloud}
      eval knumber=3
      module load awscli

      export CLOUDDIR=( \
nccf-prod-distribution-group-01/NCEP_EMC/JPSS-PG-Results/JPSS-1/VIIRS_WINDS_NB_BUFR/bufr-dmw-vpw-ccap-20230125
      \
nccf-prod-distribution-group-01/NCEP_EMC/JPSS-PG-Results/SNPP/VIIRS_WINDS_NB_BUFR/bufr-dmw-vpw-ccap-20230125
      \
nccf-prod-distribution-group-01/NCEP_EMC/JPSS-PG-Results/NOAA-21/VIIRS_WINDS_NB_BUFR/bufr-dmw-vpw-ccap-20230125
      )
     

eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
NB-PAMV_v3r2_j01_s???????????????_e???????????????_c???????????????.bufr \
NB-PAMV_v3r2_npp_s???????????????_e???????????????_c???????????????.bufr \
NB-PAMV_v3r2_n21_s???????????????_e???????????????_c???????????????.bufr"'}

      eval TANKFILE${n}=\${TANKFILE${n}:-b005/xx091}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranmtypsbt}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.005}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-bufr_V10satwind.4qual.bufrtable}
      eval HOURS2ALARM${n}=\${HOURS2ALARM${n}:-9}
      eval HISTLENMIN${n}=\${HISTLENMIN${n}:-3300}
      eval HISTLENMAX${n}=\${HISTLENMAX${n}:-3600}


# ----------------------------------------------------------------------

echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
