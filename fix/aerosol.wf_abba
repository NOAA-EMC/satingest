set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------
#  Meteosat FIRE DATA FROM NASA (Wildfire Automated Biomass Burning Algorithm)
#  [ GOES removed 06/2020 ]
#  ---------------------------------------

       eval ORBITLIST${n}=wf_abba
       eval REMOTEDSNGRP${n}='"\
PDAFileLinks/PULL/ASCII/f???????????.v65.m??.filt"'
       eval FTYPE${n}=none
       eval TANKFILE${n}=wf_abba
       eval EXECUTE${n}=ingest_copy.sh
       eval BUFRTABLE${n}=none
       eval FORGNTABLE${n}=noforgntable
       eval HISTLENMIN${n}=16800 # lots of small files, need large no. of lines
       eval HISTLENMAX${n}=18000 # lots of small files, need large no. of lines
       eval HOURS2ALARM${n}=6
       eval log${n}=$OUTDIR/ingest_get_wf_abba.out




echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
