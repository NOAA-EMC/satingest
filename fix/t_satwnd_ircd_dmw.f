set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# The NESDIS GOES DMW IR/Long-Wave Wind Family (IRCD_DMW) {Channel 14 (11.2 um)}
#  (GOES-16/17 satellites)
# ------------------------------------------------------------------------------

      eval ORBITLIST${n}=\${ORBITLIST${n}:-goesDMW.infrared.list}
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
      eval knumber=2

export DNCF1="$AWSDIR/NB_DMWF_C14_BUFR/GOES-18/"
export DNCF2="$AWSDIR/NB_DMWF_C14_BUFR/GOES-19/"

eval SNCF1="NB-DMWF-M6C14_v1r0_g18_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF2="NB-DMWF-M6C14_v1r0_g19_s???????????????_e???????????????_c???????????????.bufr"

eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
$SNCF1 \
$SNCF2"'}

      eval TANKFILE${n}=\${TANKFILE${n}:-b005/xx030}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranmtypsbt}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.005}
      eval SUBDATE_CHECK${n}=\${SUBDATE_CHECK${n}:-YES}



echo " Completed $TASK and $TASK.$family "
# ---------------------------------------------------------------------
echo " DONE WITH AWS SERVER "
