set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------

# VIIRS SST (CLEAR SKY W/O LAND)
# ------------------------------------------------------------------
      eval ORBITLIST${n}=\${ORBITLIST${n}:-viirsst_clearsky.orbitlist} 
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
      eval knumber=3

eval DNCF1="$AWSDIR/ACSPO_VIIRS_Clear_BUFR/NOAA-20/"
eval DNCF2="$AWSDIR/ACSPO_VIIRS_Clear_BUFR/NOAA-21/"
eval DNCF3="$AWSDIR/ACSPO_VIIRS_Clear_BUFR/SNPP/"

eval SNCF1="VIIRS-ACSPO-SST-Clear_v2r80_j01_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF3="VIIRS-ACSPO-SST-Clear_v2r80_n21_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF3="VIIRS-ACSPO-SST-Clear_v2r80_npp_s???????????????_e???????????????_c???????????????.bufr"
      
        eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
$SNCF1 \
$SNCF2 \
$SNCF3"'}

      eval FTYPE${n}=\${FTYPE${n}:-bufr}
      eval TANKFILE${n}=\${TANKFILE${n}:-b012/xx023}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranmtypsbt}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.012}
      eval HISTLENMIN${n}=\${HISTLENMIN${n}:-4500}
      eval HISTLENMAX${n}=\${HISTLENMAX${n}:-4800}
      eval CRITICAL${n}=\${CRITICAL${n}:-NO}
      eval SUBDATE_CHECK${n}=\${SUBDATE_CHECK${n}:-YES}

echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
echo " DONE WITH AWS SERVER "
