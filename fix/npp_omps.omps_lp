set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------



# The OMPS LIMB PROFILER
#  (S-NPP satellite)
# --------------------------------------------------------------
      eval PROC_MULT_FILES${n}=\${PROC_MULT_FILES${n}:-YES}
      eval IFILES_MAX_MULT${n}=\${IFILES_MAX_MULT${n}:-150}
      eval ORBITLIST${n}=\${ORBITLIST${n}:-ompslp.orbitlist}
      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"\
PDAFileLinks/NDE/OMPS/OMPS-LP-EV-EDR_v2r51_npp_s???????????????_e???????????????_c???????????????.bufr"'}
      eval TANKFILE${n}=\${TANKFILE${n}:-b008/xx019}
##      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranmtypsbt}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranjb.sh}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.008}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-noforgntable}
      eval HISTLENMIN${n}=\${HISTLENMIN${n}:-24700}
      eval HISTLENMAX${n}=\${HISTLENMAX${n}:-25000}
      eval log${n}=\${log${n}:-$OUTDIR/tranjb_ingest_ompslp.out}




echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
