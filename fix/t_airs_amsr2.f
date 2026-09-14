set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------

# The AMSR2 Family (AMSR2) (GCOM-W satellite)
# -------------------------------------------
      eval ORBITLIST${n}=\${ORBITLIST${n}:-amsr2.tmbr.orbitlist}
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
      eval knumber=3

eval DNCF1="$AWSDIR/AMSR_MBT_89A_BUFR/GOSAT-GW/"
eval DNCF2="$AWSDIR/AMSR_MBT_89B_BUFR/GOSAT-GW/"
eval DNCF3="$AWSDIR/AMSR_MBT_LR_BUFR/GOSAT-GW/"

eval SNCF1="AMSR3-MBT-89A_v1r0_ggw_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF2="AMSR3-MBT-89B_v1r0_ggw_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF3="AMSR3-MBT-LR_v1r0_ggw_s???????????????_e???????????????_c???????????????.bufr"

      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
$SNCF1 \
$SNCF2 \
$SNCF3"'}
      


      eval FTYPE${n}=\${FTYPE${n}:-bufr}
      eval TANKFILE${n}=\${TANKFILE${n}:-b021/xx248}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranjb.sh}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.021}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-noforgntable}
      eval CRITICAL${n}=\${CRITICAL${n}:-NO}
      eval log${n}=\${log${n}:-$OUTDIR/tranjb_ingest_amsr2_tmbr.out}
      eval SUBDATE_CHECK${n}=\${SUBDATE_CHECK${n}:-YES}



echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
