set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# USAF 16th mesh N. Hem. snow-depth/sea-ice grib1 copy-forward file
# -----------------------------------------------------------------

      eval dsname${n}='"NPR.SNWN.SP.S1200.MESH16"'
      eval TANKSUBDIR${n}=wgrbbul
      eval PROCSCRIPT${n}=nullexec
      eval daysavail${n}=justcopyforward
      eval HOURS2ALARM${n}=48




echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
