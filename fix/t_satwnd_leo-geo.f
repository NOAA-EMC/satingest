set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------

# SSEC/WISC Leo-geo winds
# ----------------------------------------------------------
      eval MACHINE${n}=\${MACHINE${n}:-stratus.ssec.wisc.edu}
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-lftp}
      eval ORBITLIST${n}=\${ORBITLIST${n}:-leo-geo.list}
      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"\
pub/winds/leogeo/satwnd.bufrcdft.LG_?.D?????.T?????"'}
      eval TANKFILE${n}=\${TANKFILE${n}:-b005/xx072}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranmtypsbt}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.005}




echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
