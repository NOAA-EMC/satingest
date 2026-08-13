set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# The GOME OZONE Family (GOME) ( METOP-1(B) satellites)
# -------------------------------------------------------------------
      eval ORBITLIST${n}=\${ORBITLIST${n}:-ozone.gome.orbitlist}
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
      eval knumber=2

eval DNCF1="$AWSDIR/GOME-TO-L2_BUFR/MetOp-B/"
eval DNCF2="$AWSDIR/GOME-TO-L2_BUFR/MetOp-C/"

eval SNCF1="V8TOZ-EDR_v1r1_m01_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF2="V8TOZ-EDR_v1r1_m03_s???????????????_e???????????????_c???????????????.bufr"

      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'" \
$SNCF2"'}
      eval FTYPE${n}=\${FTYPE${n}:-bufr}
      eval TANKFILE${n}=\${TANKFILE${n}:-b008/xx012}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranmtypsbt}
      eval mod_sec3_desc${n}=\${mod_sec3_desc${n}:-NO}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.008}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-noforgntable}
      eval HISTLENMIN${n}=\${HISTLENMIN${n}:-10500}
      eval HISTLENMAX${n}=\${HISTLENMAX${n}:-10800}
      eval CRITICAL${n}=\${CRITICAL${n}:-NO}
      eval log${n}=\${log${n}:-$OUTDIR/tranjb_ingest_ozone_gome.out}



echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
echo " DONE WITH AWS SERVER "
