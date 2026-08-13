set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------
#
echo "STARTING $JJOB.$family and MACHINE is $MACHINE and n is $n "

#   elif [ "$dtyp" = 'GOES16_ASR_BASE' ]; then
# GOES-16 All Sky Radiance baseline product
#-----------------------------------------------------------------
      eval ORBITLIST${n}=\${ORBITLIST${n}:-goes16asr_baseline.hrlylist}
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
      eval knumber=2

      eval DNCF1="$AWSDIR/ABI_L2_ASRF_BUFR/GOES-18/"
      eval DNCF2="$AWSDIR/ABI_L2_ASRF_BUFR/GOES-19/"
      
eval SNCF1="ABI-L2-ASRF-M6_v2r3_g18_s???????????????_e???????????????_c???????????????.bufr"
eval SNCF2="ABI-L2-ASRF-M6_v2r3_g19_s???????????????_e???????????????_c???????????????.bufr"

      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
$SNCF1 \
$SNCF2"'}
      eval TANKFILE${n}=\${TANKFILE${n}:-b021/xx045}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranmtypsbt}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.021}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-bufrtab.GOES16ASR_NESDIS}
      eval UNCOMPRESS_UNIX${n}=\${UNCOMPRESS_UNIX${n}:-NO}
      eval CRITICAL${n}=\${CRITICAL${n}:-NO}
      eval SUBDATE_CHECK${n}=\${SUBDATE_CHECK${n}:-YES}



echo "COMPLETED $JJOB.$family and MACHINE is $MACHINE and n is $n "

echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
echo " DONE WITH AWS SERVER "
