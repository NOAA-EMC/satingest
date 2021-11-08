set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


#  Great Lakes ice concentration data from National Ice Center
#  (zipped ascii format)
#  ----------------------------------------------------------

      eval ORBITLIST${n}=nic_lks
      eval REMOTEDSNGRP${n}='"\
PDAFileLinks/NIC/NIC_LKS_????_???_??.zip"'
      eval FTYPE${n}=none
      eval TANKFILE${n}=\${TANKFILE${n}:-nic_lks}
      eval EXECUTE${n}=ingest_copy.sh
      eval BUFRTABLE${n}=none
      eval FORGNTABLE${n}=noforgntable
      eval HOURS2ALARM${n}=24
      eval ndayarch${n}=\${ndayarch${n}:-10}
      eval CRITICAL${n}=NO



echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
