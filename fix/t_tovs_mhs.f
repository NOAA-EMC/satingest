set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# The MHS Family (MHS) (NOAA-19, and METOP-1(B),-3(C) satellites)
# NOAA-18 MHS instrument failed 10/2018; removed from processing 3/2019
# ---------------------------------------------------------------------
      eval ORBITLIST${n}=\${ORBITLIST${n}:-atovs1b.mhsx.orbitlist}
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
      eval knumber=2

# STATUS 20260730: THE FILES ARE IN THE PREFIX "METOP-B" for M1 and M3; 
# NOT CORRECTED 


eval DNCF1="$AWSDIR/METOP_MHS_Level_1b_NSS.MHSX/MetOp-B/"
eval DNCF2="$AWSDIR/METOP_MHS_Level_1b_NSS.MHSX/MetOp-B/"

echo " DONE WITH AWS SERVER "

eval SNCF1="NSS.MHSX.M1.D?????.S????.E????.B???????.??"
eval SNCF2="NSS.MHSX.M3.D?????.S????.E????.B???????.??"

      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
$SNCF1 \
$SNCF2"'}
      eval FTYPE${n}=\${FTYPE${n}:-none}
      eval TANKFILE${n}=\${TANKFILE${n}:-b021/xx027}
      eval EXECUTE${n}=\${EXECUTE${n}:-ingest_script_atovs1b.sh}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.021}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-noforgntable}
      eval COMPRESS${n}=\${COMPRESS${n}:-YES}
      eval HISTLENMIN${n}=\${HISTLENMIN${n}:-1300}
      eval HISTLENMAX${n}=\${HISTLENMAX${n}:-1600}
      eval CRITICAL${n}=\${CRITICAL${n}:-YES}




echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
echo " DONE WITH AWS SERVER "
