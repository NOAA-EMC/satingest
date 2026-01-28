set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------

# The AMSR2 Family (AMSR2) (GCOM-W satellite)
# -------------------------------------------
      eval ORBITLIST${n}=\${ORBITLIST${n}:-amsr2_GSI.tmbr.orbitlist}
      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"\
PDAFileLinks/NDE/AMSR2/AMSR2-MBT-GSI_v2r0_GW1_s???????????????_e???????????????_c???????????????.bufr"'}

#      eval MACHINE${n}=\${MACHINE${n}:-ftp.emc.ncep.noaa.gov/ig_test/}
#      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"\
#AMSR2-MBT-GSI_v2r0_GW1_s???????????????_e???????????????_c???????????????.bufr"'}

      eval FTYPE${n}=\${FTYPE${n}:-bufr}
      eval TANKFILE${n}=\${TANKFILE${n}:-b021/xx247}
#      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranjb.sh}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranmtypsbt}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.021}
#      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-AMSR2_GSI_14chan_bufrtab}
#      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-noforgntable}
      eval CRITICAL${n}=\${CRITICAL${n}:-NO}
      eval log${n}=\${log${n}:-$OUTDIR/tranjb_ingest_amsr2_GSI_tmbr.out}
      eval SUBDATE_CHECK${n}=\${SUBDATE_CHECK${n}:-YES}
      eval mod_sec3_desc${n}=\${mod_sec3_desc${n}:-NO}
#      eval ENCODE_BUFRTABLE${n}=\${ENCODE_BUFRTABLE${n}:-YES}



echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
