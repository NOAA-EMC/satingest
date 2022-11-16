set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


# The AMSU-A Family (AMSUA) (NOAA-15, -18, -19, and METOP-1(B),
#                            -3(C) satellites)
# -------------------------------------------------------------------
      eval ORBITLIST${n}=\${ORBITLIST${n}:-atovs1b.amax.orbitlist}
      eval REMOTEDSNGRP${n}=\${REMOTEDSNGRP${n}:-'"concatenate_families \
PDAFileLinks/PULL/AMSUA/NSS.AMAX.NK.D?????.S????.E????.B???????.?? \
PDAFileLinks/PULL/AMSUA/NSS.AMAX.NN.D?????.S????.E????.B???????.?? \
PDAFileLinks/PULL/AMSUA/NSS.AMAX.NP.D?????.S????.E????.B???????.?? \
PDAFileLinks/PULL/AMSUA/NSS.AMAX.M1.D?????.S????.E????.B???????.?? \
PDAFileLinks/PULL/AMSUA/NSS.AMAX.M3.D?????.S????.E????.B???????.??"'}
      eval FTYPE${n}=\${FTYPE${n}:-none}
      eval TANKFILE${n}=\${TANKFILE${n}:-b021/xx023}
      eval EXECUTE${n}=\${EXECUTE${n}:-ingest_script_atovs1b.sh}
      eval BUFRTABLE${n}=\${BUFRTABLE${n}:-bufrtab.021}
      eval FORGNTABLE${n}=\${FORGNTABLE${n}:-noforgntable}
      eval COMPRESS${n}=\${COMPRESS${n}:-YES}
      eval PROCESS_Ta${n}=\${PROCESS_Ta${n}:-YES} # if YES process antenna
                                                  #  temps into b021/xx123
      eval PROCESS_Tb${n}=\${PROCESS_Tb${n}:-YES} # if YES process brightness
                                                  #  temps into b021/xx023
      eval HISTLENMIN${n}=\${HISTLENMIN${n}:-1300}
      eval HISTLENMAX${n}=\${HISTLENMAX${n}:-1600}
      eval CRITICAL${n}=\${CRITICAL${n}:-YES}



echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
