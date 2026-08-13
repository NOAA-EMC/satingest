set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# The VIIRS IR/Long-Wave Wind Family (VIIRS) (NPP & NOAA-20 satellites)
# ----------------------------------------------------------
      eval ORBITLIST${n}=\${ORBITLIST${n}:-poeswind.viirs.infrared.list}
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
      eval knumber=3
     
eval DNCF1="$AWSDIR/VIIRS_WINDS_NB_BUFR/SNPP/"
eval DNCF2="$AWSDIR/VIIRS_WINDS_NB_BUFR/NOAA-20/"
eval DNCF3="$AWSDIR/VIIRS_WINDS_NB_BUFR/NOAA-21/"

eval SNCF1="NB-PAMV_v3r2_npp_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF2="NB-PAMV_v3r2_j01_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF3="NB-PAMV_v3r2_n21_s???????????????_e???????????????_c???????????????.bufr"

eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
$SNCF1 \
$SNCF2 \
$SNCF3"'}

      eval TANKFILE${n}=\${TANKFILE${n}:-b005/xx091}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranmtypsbt}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.005}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-bufr_V10satwind.4qual.bufrtable}
      eval HOURS2ALARM${n}=\${HOURS2ALARM${n}:-9}
      eval HISTLENMIN${n}=\${HISTLENMIN${n}:-3300}
      eval HISTLENMAX${n}=\${HISTLENMAX${n}:-3600}
      eval SUBDATE_CHECK${n}=\${SUBDATE_CHECK${n}:-YES}




# ----------------------------------------------------------------------

echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
echo " DONE WITH AWS SERVER "
