set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# The CrIS Full Spectral Radiance Subset 431 Channel Family
#  (CRIS_FSR_431 } 
#  {S-NPP and JPSS-1 (NOAA-20) satellites}
# ---------------------------------------------------------------------

      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-thecloud}
      export iflag_CRIS=${iflag_CRIS:-1}
      eval PROC_MULT_FILES${n}=\${PROC_MULT_FILES${n}:-YES}
      eval IFILES_MAX_MULT${n}=\${IFILES_MAX_MULT${n}:-150}
      eval ORBITLIST${n}=\${ORBITLIST${n}:-cris_fsr_431.orbitlist}

#-----------------
# choose ...
#
# uncomment 3 lines below to process BOTH S-NPP & JPSS-1 (NOAA-20)




      module load awscli
              eval knumber=3
              export CLOUDDIR=( \
nccf-prod-distribution-group-01/NCEP_EMC/JPSS-PG-Results/JPSS-1/NUCAPS_C0431_BUFR/bufr-as-ccap-20230911 \
nccf-prod-distribution-group-01/NCEP_EMC/JPSS-PG-Results/NOAA-21/NUCAPS_C0431_BUFR/bufr-as-ccap-20230911
              \
nccf-prod-distribution-group-01/NCEP_EMC/JPSS-PG-Results/SNPP/NUCAPS_C0431_BUFR/bufr-as-ccap-20220722
              \
              )

              eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
NUCAPS-C0431_v4r0_n21_s???????????????_e???????????????_c???????????????.bufr \
NUCAPS-C0431_v4r0_j01_s???????????????_e???????????????_c???????????????.bufr \
NUCAPS-C0431_v3r1_npp_s???????????????_e???????????????_c???????????????.bufr"'}

      eval TANKFILE${n}=\${TANKFILE${n}:-b021/xx206}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranmtypsbt}
      eval mod_sec3_desc${n}=\${mod_sec3_desc${n}:-NO}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.021}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-noforgntable}
      eval HISTLENMIN${n}=\${HISTLENMIN${n}:-59700}
      eval HISTLENMAX${n}=\${HISTLENMAX${n}:-60000}
      eval log${n}=\${log${n}:-$OUTDIR/tranjb_ingest_cris_fsr_431.out}


echo " Completed $TASK and $TASK.$family "
#
