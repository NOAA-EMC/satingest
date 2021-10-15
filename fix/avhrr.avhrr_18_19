set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------



# The NOAA-18/19 satellite AVHRR Family (AVHRR_18/19)
# ---------------------------------------------
      eval ORBITLIST${n}=\${ORBITLIST${n}:-avhrr1b_18_19.gac.orbitlist}
      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
PDAFileLinks/PULL/AVHRRGAC/NSS.GHRR.NN.D?????.S????.E????.????????.?? \
PDAFileLinks/PULL/AVHRRGAC/NSS.GHRR.NP.D?????.S????.E????.????????.??"'}
      eval FTYPE${n}=\${FTYPE${n}:-none}
      eval TANKFILE${n}=\${TANKFILE${n}:-b021/xx053}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranavhrr}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.021}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-noforgntable}



echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
