set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# The NESDIS GOES DMW IR/Long-Wave Wind Family (IRCD_DMW) {Channel 14 (11.2 um)}
#  (GOES-16/17 satellites)
# ------------------------------------------------------------------------------

      eval ORBITLIST${n}=\${ORBITLIST${n}:-goesDMW.infrared.list}
      eval TRANSFER_COMMAND${n}=\${TRANSFER_COMMAND${n}:-syncaws}
      eval knumber=2
      module load awscli


export AWSDIR=nccf-prod-distribution-group-01/NCEP_EMC/results
export DIRAWS1="$AWSDIR/GOES-19/NB_DMWF_C14_BUFR/goes-winds-bufr-ccap-20250117/"
export DIRAWS2="$AWSDIR/GOES-18/NB_DMWF_C14_BUFR/goes-winds-bufr-ccap-20250117/"
              export DIRAWS3=$DIRAWS1
              export DIRAWS4=$DIRAWS2

eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
NB-DMWF-M6C14_v1r0_g19_s???????????????_e???????????????_c???????????????.bufr \
NB-DMWF-M6C14_v1r0_g18_s???????????????_e???????????????_c???????????????.bufr"'}

      eval TANKFILE${n}=\${TANKFILE${n}:-b005/xx030}
      eval EXECUTE${n}=\${EXECUTE${n}:-bufr_tranmtypsbt}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.005}



echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
