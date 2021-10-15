set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# AMSR2 SST Retrievals (GCOM-W satellite)
# ---------------------------------------
      eval ORBITLIST${n}=\${ORBITLIST${n}:-amsr2sst.orbitlist}
      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"\
PDAFileLinks/NDE/AMSR2/AMSR2-SST_v2r2_GW1_s???????????????_e???????????????_c???????????????.bufr"'}
      eval FTYPE${n}=\${FTYPE${n}:-bufr}
      eval TANKFILE${n}=\${TANKFILE${n}:-b012/xx222}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranmtypsbt}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.012}
      eval CRITICAL${n}=\${CRITICAL${n}:-NO}
      eval log${n}=\${log${n}:-$OUTDIR/tranjb_ingest_amsr2_sst.out}



echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
