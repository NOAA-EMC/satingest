set -xa
echo " Starting $TASK and  $TASK.$family "
# ----------------------------------------------------------------------


#  Global geostationary satellite composite mosaic imagery McIDAS files
#   (long-wave IR channels, short-wave IR channels, visible channels,
#   satellite resources metadata) (SAT_COMP_MOSAIC_IMGRY)
#  --------------------------------------------------------------------

      eval ORBITLIST${n}=sat_comp_mosaic_imgry
      eval REMOTEDSNGRP${n}='"concatenate_families \
PDAFileLinks/PULL/MCIDAS/GLOBCOMPLIR.?????????? \
PDAFileLinks/PULL/MCIDAS/GLOBCOMPSIR.?????????? \
PDAFileLinks/PULL/MCIDAS/GLOBCOMPVIS.?????????? \
PDAFileLinks/PULL/MCIDAS/GLOBCOMPSSR.??????????"'
      eval FTYPE${n}=none
      eval TANKFILE${n}=\${TANKFILE${n}:-mcidas}
      eval EXECUTE${n}=copy_to_target
      eval TARGETFILE${n}=same_name2
      eval BUFRTABLE${n}=none
      eval FORGNTABLE${n}=noforgntable
      eval HOURS2ALARM${n}=3
      eval CRITICAL${n}=YES




echo " Completed $TASK and $TASK.$family "
# ----------------------------------------------------------------------
