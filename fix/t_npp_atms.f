set -xa
echo " Starting $TASK and  $TASK.$family "


# The ATMS Family (ATMS) {S-NPP and JPSS-1 (NOAA-20) satellites}
# --------------------------------------------------------------
      eval PROC_MULT_FILES${n}=\${PROC_MULT_FILES${n}:-YES}
      eval IFILES_MAX_MULT${n}=\${IFILES_MAX_MULT${n}:-180}
      eval ORBITLIST${n}=\${ORBITLIST${n}:-atms.orbitlist}
#-----------------
# choose ...
#

eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
eval knumber=3

eval DNCF1="$AWSDIR/ATMS_BUFR/NOAA-20/"
eval DNCF2="$AWSDIR/ATMS_BUFR/NOAA-21/"
eval DNCF3="$AWSDIR/ATMS_BUFR/SNPP/"

eval SNCF1="ATMS_v1r0_j01_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF2="ATMS_v1r0_n21_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF3="ATMS_v1r0_npp_s???????????????_e???????????????_c???????????????.bufr"

      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
$SNCF1 \
$SNCF2 \
$SNCF3"'}

#-----------------
      eval TANKFILE${n}=\${TANKFILE${n}:-b021/xx203}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranjb.sh}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.021}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-noforgntable}
      eval HISTLENMIN${n}=\${HISTLENMIN${n}:-79700}
      eval HISTLENMAX${n}=\${HISTLENMAX${n}:-80000}
      eval log${n}=\${log${n}:-$OUTDIR/tranjb_ingest_atms.out}



echo " Completed $TASK and $TASK.$family "
echo " DONE WITH AWS SERVER "
# ----------------------------------------------------------------------
