set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# The IASI Every FOV Family (IASI_efov) (METOP-1(B) and
# METOP-3(C) satellites)
# ----------------------------------------------------------------------------
      eval ORBITLIST${n}=\${ORBITLIST${n}:-metop.iasi_efov.orbitlist}
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
      eval knumber=2

eval DNCF1="$AWSDIR/NUCAPS_GMAO_616_BUFR/MetOp-B/"
eval DNCF2="$AWSDIR/NUCAPS_GMAO_616_BUFR/MetOp-C/"

eval SNCF1="NUCAPS-GMAO-616_v3r2_m01_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF2="NUCAPS-GMAO-616_v3r2_m03_s???????????????_e???????????????_c???????????????.bufr"
     
       eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
$SNCF1 \
$SNCF2"'}
      eval FTYPE${n}=\${FTYPE${n}:-bufr}
      eval TANKFILE${n}=\${TANKFILE${n}:-b021/xx241}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranjb.sh}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.021}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-noforgntable}
      eval HISTLENMIN${n}=\${HISTLENMIN${n}:-17700}
      eval HISTLENMAX${n}=\${HISTLENMAX${n}:-18000}
      eval CRITICAL${n}=\${CRITICAL${n}:-NO}
      eval log${n}=\${log${n}:-$OUTDIR/tranjb_ingest_iasi.out}
      eval SUBDATE_CHECK${n}=\${SUBDATE_CHECK${n}:-YES}





echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
echo " DONE WITH AWS SERVER "
