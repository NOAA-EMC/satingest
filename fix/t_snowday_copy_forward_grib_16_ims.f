set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# NESDIS IMS 16th mesh Global snow/sea-ice grib copy-forward file
# ---------------------------------------------------------------

      eval dsname${n}='"imssnow.grb"'
      eval TANKSUBDIR${n}=wgrbbul
      eval PROCSCRIPT${n}=nullexec
      eval daysavail${n}=justcopyforward
      eval HOURS2ALARM${n}=24



echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
