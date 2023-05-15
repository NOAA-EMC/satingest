set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------

# The OMI OZONE Family (OMI) (AURA satellite)
# -------------------------------------------
      eval ORBITLIST${n}=\${ORBITLIST${n}:-ozone.omi.orbitlist}
      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"\
PDAFileLinks/PULL/OZONE/OMI-Aura_L2-OMTO3_????m????t????-o??????_v883-????m????t??????.he5"'}
      eval FTYPE${n}=\${FTYPE${n}:-none}
      eval TANKFILE${n}=\${TANKFILE${n}:-b008/xx013}
      eval EXECUTE${n}=\${EXECUTE${n}:-ingest_script_omi.sh}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.008}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-noforgntable}
      eval CRITICAL${n}=\${CRITICAL${n}:-NO}
      eval log${n}=\${log${n}:-$OUTDIR/tranjb_ingest_ozone_omi.out}




echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
