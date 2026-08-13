set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------
#

#   elif [ "$dtyp" = 'VIIRS_WEEKLY_GVF' ]; then
#  NESDIS VIIRS weekly realtime 4 km global GVF (Green Vegetation Fraction)
#   (updated daily) (VIIRS_WEEKLY_GVF) (S-NPP)
#  ------------------------------------------------------------------------

      eval ORBITLIST${n}=viirs_greenveg.weeklylist
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
      eval knumber=2

eval DNCF1="$AWSDIR/GVF_GLB_GRIB2/SNPP/"
eval DNCF2="$AWSDIR/GVF_GLB_GRIB2/NOAA-20/"

eval SNCF1="GVF-WKL-GLB_v3r0_npp_s????????_e????????_c???????????????.grib2"
eval SNCF2="GVF-WKL-GLB_v3r0_j01_s????????_e????????_c???????????????.grib2"
     
      eval REMOTEDSNGRP${n}='"concatenate_families \
$SNCF1 \
$SNCF2"'
      eval FTYPE${n}=none
      eval TANKFILE${n}=viirs
      eval EXECUTE${n}=ingest_copy.sh
      eval BUFRTABLE${n}=none
      eval FORGNTABLE${n}=noforgntable
      eval HISTLENMIN${n}=675
      eval HISTLENMAX${n}=700
      eval HOURS2ALARM${n}=30



echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
echo " DONE WITH AWS SERVER "
