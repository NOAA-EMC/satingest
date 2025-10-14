set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------
#

#   if [ "$dtyp" = 'AVHRR_WEEKLY_GVF' ]; then
#  NESDIS AVHRR weekly realtime 0.144-degree global GVF (Green Vegetation
#   Fraction) (updated weekly) (AVHRR_WEEKLY_GVF) (NOAA-18 and NOAA-19)
#  -----------------------------------------------------------------------

      eval ORBITLIST${n}=avhrr_greenveg.weeklylist
      eval REMOTEDSNGRP${n}='"concatenate_families \
PDAFileLinks/PULL/GRIB/NPR.VGWG.NN.D?????.GRIBF \
PDAFileLinks/PULL/GRIB/NPR.VGWG.NP.D?????.GRIBF"'
      eval FTYPE${n}=none
      eval TANKFILE${n}=avhrr
      eval EXECUTE${n}=ingest_copy.sh
      eval BUFRTABLE${n}=none
      eval FORGNTABLE${n}=noforgntable
      eval HISTLENMIN${n}=75
      eval HISTLENMAX${n}=100
      eval HOURS2ALARM${n}=168



echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
