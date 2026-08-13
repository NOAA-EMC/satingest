set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# The NESDIS AQUA MODIS Water Vapor Imager Wind Family (MODWV)
#  (Terra MODIS water vaper imager winds have been gone since 7/2013)
#  METOP - B/C AVHRR in New Bufr Format. METOP-B (m01); METOP-C (m03)
# -------------------------------------------------------------------
      eval ORBITLIST${n}=\${ORBITLIST${n}:-poeswind.nbavhrr.infrared.list}
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
      eval knumber=4

eval DNCF1="$AWSDIR/NAMV_AVHRR_C04CD_FRAC_NH_BUFR/MetOp-B/"
eval DNCF2="$AWSDIR/NAMV_AVHRR_C04CD_FRAC_SH_BUFR/MetOp-B/"
eval DNCF3="$AWSDIR/NAMV_AVHRR_C04CD_FRAC_NH_BUFR/MetOp-C/"
eval DNCF4="$AWSDIR/NAMV_AVHRR_C04CD_FRAC_SH_BUFR/MetOp-C/"

eval SNCF1="NAMV-AVHRR-C04CD-FRAC-NH_v1r0_m01_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF2="NAMV-AVHRR-C04CD-FRAC-SH_v1r0_m01_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF3="NAMV-AVHRR-C04CD-FRAC-NH_v1r0_m03_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF4="NAMV-AVHRR-C04CD-FRAC-SH_v1r0_m03_s???????????????_e???????????????_c???????????????.bufr"

   eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
$SNCF1 \
$SNCF2 \
$SNCF3 \
$SNCF4"'}

      eval TANKFILE${n}=\${TANKFILE${n}:-b005/xx081}
#      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_transatw}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranmtypsbt}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.005}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-bufr_V10satwind.4qual.bufrtable}
      eval HOURS2ALARM${n}=\${HOURS2ALARM${n}:-9}
      eval HISTLENMIN${n}=\${HISTLENMIN${n}:-2400}
      eval HISTLENMAX${n}=\${HISTLENMAX${n}:-2700}
      eval SUBDATE_CHECK${n}=\${SUBDATE_CHECK${n}:-YES}



echo " Completed $TASK and $TASK.$family "
echo " DONE WITH AWS SERVER "
