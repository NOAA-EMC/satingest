set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# The SSM/IS UPP (bright. temps) Family (SSMIS) (DMSP F-17, F-18 satellites)
# --------------------------------------------------------------------------
      eval ORBITLIST${n}=\${ORBITLIST${n}:-SSMIS.orbitlist}
      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
PDAFileLinks/PULL/DMSP/NPR.TDUP.SB.D?????.S????.E????.B???????.NS \
PDAFileLinks/PULL/DMSP/NPR.TDUP.SC.D?????.S????.E????.B???????.NS"'}
      eval FTYPE${n}=\${FTYPE${n}:-bufr}
      eval TANKFILE${n}=\${TANKFILE${n}:-b021/xx201}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranmtypsbt}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.021}
      eval SUBDATE_CHECK${n}=\${SUBDATE_CHECK${n}:-YES}
      eval MESSAGE_LENGTH${n}=\${MESSAGE_LENGTH${n}:-3500}
      eval CRITICAL${n}=\${CRITICAL${n}:-NO}



echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
