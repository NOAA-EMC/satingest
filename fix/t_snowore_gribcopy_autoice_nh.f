set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# The NESDIS 2 km S. Hem. autosnow product grib2 file copy processing family
# --------------------------------------------------------------------------

      eval ORBITLIST${n}=\${ORBITLIST${n}:-autoice2km.grib2}
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
      eval knumber=1

eval DNCF1="$AWSDIR/GMASI_ICE_NH/"

eval SNCF1="GMASI-Ice-NH2km_v1r0_blend_s???????????????_e???????????????_c???????????????.grib2"
     
      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"\
$SNCF1"'}
      eval TANKFILE${n}=\${TANKFILE${n}:-wgrbbul}
      eval EXECUTE${n}=ingest_copytotarget.sh
      eval TARGETFILE${n}=\${TARGETFILE${n}:-'"NH2km_autoice.grb.grib2"'}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-none}
      eval HOURS2ALARM${n}=\${HOURS2ALARM${n}:-48}



echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
echo " DONE WITH AWS SERVER "
