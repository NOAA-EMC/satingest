set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------

# The ATOVS Family (ATOVS) (NOAA-19, METOP-1(B) satellites)
# ---------------------------------------------------------
      eval ORBITLIST${n}=\${ORBITLIST${n}:-atovs.orbitlist}
      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
PDAFileLinks/PULL/ATOVS/NPR.ATNC.NP.D?????.S????.E????.???????? \
PDAFileLinks/PULL/ATOVS/NPR.ATNC.M1.D?????.S????.E????.????????"'}
      eval FTYPE${n}=\${FTYPE${n}:-ncepbufr}
      eval TANKFILE${n}=\${TANKFILE${n}:-b003/xx104}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranjb.sh}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.003}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-noforgntable}
      eval CRITICAL${n}=\${CRITICAL${n}:-NO}




echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
