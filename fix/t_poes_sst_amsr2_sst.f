set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# AMSR2 SST Retrievals (GCOM-W satellite)
# ---------------------------------------
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
      eval ORBITLIST${n}=\${ORBITLIST${n}:-amsr2sst.orbitlist}
      eval knumber=1
      eval DNCF1="$AWSDIR/AMSR2_SST_BUFR/GCOM-W1/"
      
eval SNCF1="AMSR2-SST_v2r3_GW1_s???????????????_e???????????????_c???????????????.bufr"

      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"\
$SNCF1"'}

      eval FTYPE${n}=\${FTYPE${n}:-bufr}
      eval TANKFILE${n}=\${TANKFILE${n}:-b012/xx222}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranmtypsbt}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.012}
      eval CRITICAL${n}=\${CRITICAL${n}:-NO}
      eval log${n}=\${log${n}:-$OUTDIR/tranjb_ingest_amsr2_sst.out}



echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
echo " DONE WITH AWS SERVER "
