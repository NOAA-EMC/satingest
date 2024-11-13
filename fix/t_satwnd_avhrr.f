set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# The NESDIS AVHRR IR/Long-Wave Wind Family (AVHRR)
#  (METOP-B, NOAA-15, -18 & -19 Satellites)
# --------------------------------------------------
      eval ORBITLIST${n}=\${ORBITLIST${n}:-poeswind.avhrr.infrared.list}
      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
PDAFileLinks/PULL/POESAMV/satwnd.bufrcdft.METOPB.D?????.T??:??:??Z \
PDAFileLinks/PULL/POESAMV/satwnd.bufrcdft.N15.D?????.T??:??:??Z \
PDAFileLinks/PULL/POESAMV/satwnd.bufrcdft.N18.D?????.T??:??:??Z \
PDAFileLinks/PULL/POESAMV/satwnd.bufrcdft.N19.D?????.T??:??:??Z"'}
      eval TANKFILE${n}=\${TANKFILE${n}:-b005/xx080}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_transatw}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.005}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-bufr_V10satwind.4qual.bufrtable}
      eval HOURS2ALARM${n}=\${HOURS2ALARM${n}:-9}




echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
