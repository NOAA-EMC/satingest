#!/bin/sh
#PBS -N satingest_TEST20
#PBS -o /lfs/h2/emc/obsproc/noscrub/Sudhir.Nadiga/SCRIPTDIR/REENGG.3TIER/OUTPUT/satingest_%REDMINE%_%jitask%_%HHMM%.out
#PBS -e /lfs/h2/emc/obsproc/noscrub/Sudhir.Nadiga/SCRIPTDIR/REENGG.3TIER/OUTPUT/satingest_%REDMINE%_%jitask%_%HHMM%.out
#PBS -q dev_transfer
#PBS -l select=1:ncpus=3:mem=68000MB
#PBS -l walltime=00:15:00 
#PBS -A OBSPROC-DEV
cat /etc/motd

set -xa
module purge
module load envvar/1.0
module load PrgEnv-intel/8.1.0
module load intel/19.1.3.304
module load craype/2.7.8
module load cray-mpich/8.1.4

export HOMEsatingest=/lfs/h2/emc/obsproc/noscrub/Sudhir.Nadiga/GITHUBDIR/REBUILD/modify_RBv410_compiled
export VERROOT=/lfs/h2/emc/obsproc/noscrub/Sudhir.Nadiga/GITHUBDIR/REBUILD/modify_RBv4.1.0/versions
.  ${VERROOT}/run.ver

module load netcdf/$netcdf_ver
module load w3nco/$w3nco_ver
module load bufr/$bufr_ver
module load w3emc/$w3emc_ver
module load bacio/$bacio_ver
module load hdf5/$hdf5_ver
module load prod_envir/$prod_envir_ver
module load grib_util/${grib_util_ver}
module load libjpeg/${libjpeg_ver}

export DATAROOT=/lfs/h2/emc/stmp/Sudhir.Nadiga/$$
mkdir -p $DATAROOT
export envir=prod # scripts point to production (/nwprod) files as default
export DEBUG_LEVEL=3
export KEEPDATA=YES
#export DOGZIP=NO
export CLEANUP=NO

export satingest_ver=v4.1.0

echo " REDMINE is %REDMINE% and jitask is %jitask% and HH is %HHMM% "
export DCOMROOT=/lfs/h2/emc/obsproc/noscrub/Sudhir.Nadiga/SCRIPTDIR/REENGG.3TIER/TEMPNEW
mkdir -p $DCOMROOT
export jlogfile=$DCOMROOT/jlogfile_%REDMINE%_%jitask%
export job=%REDMINE%_%jitask%_%HHMM%
export jobid=jobid_%REDMINE%_%jitask%_%HHMM%
export SCREEN=OFF  # DO NOT screen old reports in BUFR_TRANJB
export IFILES_MAX_GET=100000
export MOD_SEC3_DESC=NO
export TASK=%jitask%

module use /lfs/h2/emc/obsproc/noscrub/Sudhir.Nadiga/GITHUBDIR/REBUILD/bufr_tranjb-11-6/modulefiles
module load bufr_tranjb/${bufr_tranjb_ver}
export TRANJBush=$TRANush
export UTILROOT=/apps/ops/prod/nco/core/prod_util.v2.0.12
module list


##################### INSERTED FROM JISATINGEST #######################################
export PS4='+ ${SECONDS}s + '
 if [ -n "$satingest_ver" ]; then
    set +x
    echo
    echo "Current OBSPROC_SATINGEST version is $satingest_ver"
    echo
    set -x
 fi
 
 # Be sure bufr_tranjb module is loaded!
# if [[ $(echo $LOADEDMODULES | egrep -c "(^|:)bufr_tranjb/") -eq 0 ]]; 
#      then echo "bufr_tranjb is required but not loaded! Exiting..."; 
#      exit; 
# fi
 
###################################################################
# Set DEBUG LEVEL - modifies info in execution trace prompt string
###################################################################
export DEBUG_LEVEL=${DEBUG_LEVEL=1}
case $DEBUG_LEVEL in
  1) export PS4='+ ${SECONDS}s + ' ;;       # add seconds (this is typical case)
  0) export PS4='+ ';;                      # don't include seconds (useful if
                                            #  want to diff output from 2 runs)
  2) export PS4='+ ${SECONDS}s:L$LINENO + ';;         # add script line #
  3) export PS4='+ ${SECONDS}s:${0##*/}:L$LINENO + ';;# add script name & line #
  *) echo "Unrecognized DEBUG_LEVEL.  Stay with current PS4 setting";;
 esac
#---
echo "Current OBSPROC_SATINGEST version is $satingest_ver"
echo "OBSPROC_SHARED_BUFR_CWORD version used by this job is $obsproc_shared_bufr_cword_ver"
echo envir is ${envir:?"###FATAL ERROR \"envir\" is not set"}
echo job is ${job:?"###FATAL ERROR \"job\" is not set"}
echo TASK is ${TASK:?"###FATAL ERROR \"TASK\" is not set"}
---
 echo envir is ${envir}
 echo job is ${job}
 echo TASK is ${TASK}
 if [[ "${RUN_ENVIR,,}" == "nco" ]]; then
   export SENDECF=YES
   export SENDBB=${SENDBB:-YES}
 else  # developer
   export SENDECF=NO
   export SENDBB=${SENDBB:-NO}
 fi
---
 if [[ "${RUN_ENVIR,,}" == "nco" ]]; then
   echo DCOMROOT is ${DCOMROOT:?"###FATAL ERROR \"DCOMROOT\" is not set"}
   export USERDIR=${DCOMROOT}/ingest_hist
   export TANKDIR=${DCOMROOT}
   export OUTDIR=${DCOMROOT}/ingest_logs
   if [[ ${envir} == prod ]]; then
     export BBDIR=${BBDIR:-${COMROOT:?}/logs/bb/ingest}
   else
     export BBDIR=${BBDIR:-${COMROOT:?}/logs/bb/ingest}
   fi
 else
   export USERDIR=${USERDIR:\
 -${DCOMROOT:?"###FATAL ERROR \"DCOMROOT\" is not set"}/ingest_hist}
   export TANKDIR=${TANKDIR:\
 -${DCOMROOT:?"###FATAL ERROR \"DCOMROOT\" is not set"}/}
   export OUTDIR=${OUTDIR:\
 -${DCOMROOT:?"###FATAL ERROR \"DCOMROOT\" is not set"}/}
 fi
---
 # Paths for obsproc_satingest items
---
 export HOMEsatingest=${HOMEsatingest:\
 -$PACKAGEROOT/satingest/${satingest_ver:?\
 "###FATAL ERROR \"satingest_ver\" is not set"}}
 export SCRIPTSobsproc_satingest=${SCRIPTSobsproc_satingest:\
 -$HOMEsatingest/scripts}
 export EXECobsproc_satingest=${EXECobsproc_satingest:\
 -$HOMEsatingest/exec}
 export FIXobsproc_satingest=${FIXobsproc_satingest:\
 -$HOMEsatingest/fix}
 export USHobsproc_satingest=${USHobsproc_satingest:\
 -$HOMEsatingest/ush}
# Defaults defined in bufr_tranjb module
 export EXECobsproc_shared_bufr_cword=${EXECobsproc_shared_bufr_cword:-$CWORDX}
 export USHobsproc_shared_bufr_cword=${USHobsproc_shared_bufr_cword:-$CWORDush}
##########################################
 export NHOUR=$UTILROOT/exec/nhour
---
#####################################################################
# Copyng utility scripts for bufr_tranjb no longer required because #
# prod_util module is loaded. Change made, 01/2022                  #
#####################################################################
 [ ! -d $USHbufr ]  &&  USHbufr=$PACKAGEROOT/ush
 [ ! -s $DCNCLD ]  &&  DCNCLD=$OPSROOT/decoders/decod_dcncld/exec/decod_dcncld
########################################
# Check that required variables are set
########################################
set +x
echo
echo TASK is ${TASK:?"###FATAL ERROR \"TASK\" is not set"}
echo
set -x
# Check that TASK is valid
if [  $TASK = 'aerosol'        -o $TASK = 'airs'           -o \
      $TASK = 'avhrr'          -o $TASK = 'radsnd'         -o \
      $TASK = 'npp_atms'       -o $TASK = 'npp_cris_2211'  -o \
      $TASK = 'npp_cris_431'   -o $TASK = 'npp_omps'       -o \
      $TASK = 'ozone14'        -o $TASK = 'ozone_orbit'    -o \
      $TASK = 'poes_sst'       -o $TASK = 'qscatter'       -o \
      $TASK = 'ssmi'           -o $TASK = 'snowday'        -o \
      $TASK = 'snowore'        -o $TASK = 'satwnd'         -o \
      $TASK = 'tovs' ] ; then             
  echo "$TASK is a valid task "
 else
  echo "###FATAL ERROR $TASK IS NOT A VALID TASK; exiting "
  exit
fi
export TASK=$TASK
cat $pgmout
##################### INSERTED FROM JISATINGEST #######################################


$HOMEsatingest/jobs/JISATINGEST
