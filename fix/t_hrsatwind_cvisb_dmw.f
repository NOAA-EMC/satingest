set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------

# The NESDIS GOES DMW Visible Wind Family (CVISB_DMW) {Channel 2 (0.64 um)}
#  (GOES-18/19 satellites)
# ------------------------------------------------------------------------

      eval ORBITLIST${n}=\${ORBITLIST${n}:-CgoesDMW.visible.list}
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
      eval knumber=2

eval DNCF1="$AWSDIR/NB_DMWC_C02_BUFR/GOES-18/"
eval DNCF2="$AWSDIR/NB_DMWC_C02_BUFR/GOES-19/"

eval SNCF1="NB-?M?C-M?C02_v1r0_g18_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF2="NB-?M?C-M?C02_v1r0_g19_s???????????????_e???????????????_c???????????????.bufr"

      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
$SNCF1 \
$SNCF2"'}
      eval TANKFILE${n}=\${TANKFILE${n}:-b005/xx054}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranmtypsbt}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.005}
      eval SUBDATE_CHECK${n}=\${SUBDATE_CHECK${n}:-YES}





echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
echo " DONE WITH AWS SERVER "
