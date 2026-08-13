set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------



# The OMPS BUFR Version 8 Nadir Profile Ozone Family (OMPSNP_V8)
#  (S-NPP satellite)
# --------------------------------------------------------------
      eval PROC_MULT_FILES${n}=\${PROC_MULT_FILES${n}:-YES}
      eval IFILES_MAX_MULT${n}=\${IFILES_MAX_MULT${n}:-150}
      eval ORBITLIST${n}=\${ORBITLIST${n}:-ompsnpv8.orbitlist}
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
      eval knumber=3

eval DNCF1="$AWSDIR/OMPS_NP_V8_BUFR/SNPP/"
eval DNCF2="$AWSDIR/OMPS_NP_V8_BUFR/NOAA-20/"
eval DNCF3="$AWSDIR/OMPS_NP_V8_BUFR/NOAA-21/"

eval SNCF1="OMPS-NP_v4r5_npp_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF2="OMPS-NP_v4r5_j01_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF3="OMPS-NP_v4r5_n21_s???????????????_e???????????????_c???????????????.bufr"

      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
$SNCF1 \
$SNCF2 \
$SNCF3"'}
      eval TANKFILE${n}=\${TANKFILE${n}:-b008/xx017}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranjb.sh}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.008}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-noforgntable}
      eval HISTLENMIN${n}=\${HISTLENMIN${n}:-24700}
      eval HISTLENMAX${n}=\${HISTLENMAX${n}:-25000}
      eval log${n}=\${log${n}:-$OUTDIR/tranjb_ingest_ompsnpv8.out}



echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
echo " DONE WITH AWS SERVER "
