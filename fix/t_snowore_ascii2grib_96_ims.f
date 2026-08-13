set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------

# The National Ice Center IMS 96th mesh (4 km) Global snow/sea-ice
#  ascii-to-grib processing family
# ----------------------------------------------------------------

      eval ORBITLIST${n}=\${ORBITLIST${n}:-snocovr.dailylist96}
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
      eval knumber=1

eval DNCF1="$AWSDIR/NESDIS_IMS-V3_4km_asc_gz/"
eval SNCF1="NIC.IMS_v3_???????00_4km.asc.gz"

      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"\
$SNCF1"'}           # this picks up 00Z file only
      eval TANKFILE${n}=\${TANKFILE${n}:-none}
      eval EXECUTE${n}=\${EXECUTE${n}:-ingest_sncvgrib96.sh}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-none}
      eval HOURS2ALARM${n}=\${HOURS2ALARM${n}:-24}




echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
echo " DONE WITH AWS SERVER "
