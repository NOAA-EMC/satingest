set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------



# The OMPS BUFR Version 8 Total Column Ozone Family (OMPSTC_V8)
#  (S-NPP & NOAA-20 satellites)
# -------------------------------------------------------------
      eval PROC_MULT_FILES${n}=\${PROC_MULT_FILES${n}:-YES}
      eval IFILES_MAX_MULT${n}=\${IFILES_MAX_MULT${n}:-150}
      eval ORBITLIST${n}=\${ORBITLIST${n}:-ompstcv8.orbitlist}
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
      eval knumber=3

eval DNCF1="$AWSDIR/OMPS_TC_V8_BUFR/SNPP/"
eval DNCF2="$AWSDIR/OMPS_TC_V8_BUFR/NOAA-20/"
eval DNCF3="$AWSDIR/OMPS_TC_V8_BUFR/NOAA-21/"

eval SNCF1="OMPS-TC_v4r5_npp_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF2="OMPS-TC_v4r5_j01_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF3="OMPS-TC_v4r5_n21_s???????????????_e???????????????_c???????????????.bufr"

      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
$SNCF1 \
$SNCF2 \
$SNCF3"'}
      eval TANKFILE${n}=\${TANKFILE${n}:-b008/xx018}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranjb.sh}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.008}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-noforgntable}
      eval HISTLENMIN${n}=\${HISTLENMIN${n}:-24700}
      eval HISTLENMAX${n}=\${HISTLENMAX${n}:-25000}
      eval log${n}=\${log${n}:-$OUTDIR/tranjb_ingest_ompstcv8.out}



echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
echo " DONE WITH AWS SERVER "
