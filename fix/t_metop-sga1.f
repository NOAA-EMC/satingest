set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# The MWS_Metop-SGA1 satellite}
# --------------------------------------------------------------
      eval PROC_MULT_FILES${n}=\${PROC_MULT_FILES${n}:-YES}
      eval IFILES_MAX_MULT${n}=\${IFILES_MAX_MULT${n}:-180}
      eval ORBITLIST${n}=\${ORBITLIST${n}:-metop-sga1.orbitlist}
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-localdiskcp}
#     eval MACHINE${n}=\${MACHINE${n}:-/lfs/h2/emc/obsproc/noscrub/steve.stegall/DCOMDIR/METOPSGA1/data1/smcd1/Metop-SGA1/}

      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-\
'"W_XX-EUMETSAT-Darmstadt,SAT,SGA1-RO_-1B-BND_C_EUMT_??????????????_G_O_??????????????_??????????????_C_N_???.bin"'}
#-----------------

#-----------------
      eval TANKFILE${n}=\${TANKFILE${n}:-b003/xx012}
#      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranjb.sh}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranmtypsbt}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.003}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-noforgntable}
      eval HISTLENMIN${n}=\${HISTLENMIN${n}:-79700}
      eval HISTLENMAX${n}=\${HISTLENMAX${n}:-80000}
      eval log${n}=\${log${n}:-$OUTDIR/tranjb_ingest_atms.out}
      eval SUBDATE_CHECK${n}=\${SUBDATE_CHECK${n}:-YES}



echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
