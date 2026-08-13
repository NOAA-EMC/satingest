set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------



# The NESDIS GOES DMW IR/Short-Wave Wind Family (CSHRTWV_DMW)
#  {Channel 7 (3.9 um)} (GOES-19/18 satellites)
# ----------------------------------------------------------

      eval ORBITLIST${n}=\${ORBITLIST${n}:-CgoesDMW.shrtwave.list}
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
      eval knumber=2

eval DNCF1="$AWSDIR/NB_DMWC_C07_BUFR/GOES-18/"
eval DNCF2="$AWSDIR/NB_DMWC_C07_BUFR/GOES-19/"

eval SNCF1="NB-?M?C-M?C07_v1r0_g18_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF2="NB-?M?C-M?C07_v1r0_g19_s???????????????_e???????????????_c???????????????.bufr"

      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
$SNCF1 \
$SNCF2"'}
      eval TANKFILE${n}=\${TANKFILE${n}:-b005/xx056}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranmtypsbt}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.005}
      eval SUBDATE_CHECK${n}=\${SUBDATE_CHECK${n}:-YES}




echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
echo " DONE WITH AWS SERVER "
