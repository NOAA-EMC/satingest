#!/bin/ksh
# Run under ksh

####  UNIX Script Documentation Block
#
# Script name:   ingest_transst_poes_navo.sh
#
# RFC contact:  Dong                 org: NP22        date: 2019-10-09
#
# Abstract: This script reads in NAVO files containing sea-surface
#   temperature retrievals, AVHRR brightness temperatures and albedo (in MCSST
#   temporary file binary format) from the NOAA-series and METOP-series POES
#   satellites, converts them into NCEP BUFR format, and writes them into the
#   /dcom tank database on the NCEP WCOSS machines.  Intermediate bufr files
#   are saved to /com for use by downstream physical retrieval job.
#
# Script history log:
# 2014-04-14 Diane Stokes - Taken from ingest_transst_poes.sh and modified to
#     process only NAVO data with one satellite per input file expected and one 
#     bufr file output. Companion text file is renamed to indicate its content
#     of time window start and end.  Satellite info is passed in via namelist.
# 2019-09-18 Jiarui Dong  Added ability to handle METOP-3(C) satellite.
# 2021-07-08 Steve Stegall - Changed to nsat=7, and added line 171 to output 
#     SAID=225, and sat name= NOAA-20. 
# 2021-12-19 Sudhir Nadiga - Modified to use bufr_tranjb module variables.
# 2022-01-18  S. Stegall  Replaced $DATA/ before calling utility scripts and instead 
#      used $UTILROOT/ush/ to properly leverage the prod_util module.
#
# Usage: ingest_transst_poes_navo.sh  <bufrtable>  <raw_file>
#
#   Script parameters:
#                 $1: bufrtable - full path definition for BUFR mnemonic table
#                 $2: raw_file  - full path definition for POES SST/btemp/
#                                 Albedo data file
#
#   Modules and files referenced:
#     scripts    : $UTILROOT/ush/prep_step
#                : $UTILROOT/ush/postmsg 
#                : $USHobsproc_satingest/bufr_tranjb.sh
#     data cards : unit 15, namelist with satellite info
#     executables: $EXECobsproc_satingest/bufr_tranpoessst_navo
#
# Remarks: Invoked by the ush script ingest_translate_orbits.sh.
#
#  Imported variables that must be passed in:
#      DATA                  - path to current working directory
#      DEBUGSCRIPTS          - if set to "ON" or "YES", will run with "set -x"
#                              on (intended for debugging)
#      USHobsproc_satingest  - path to obsproc_satingest ush directory
#                              containing bufr_tranjb.sh
#      EXECobsproc_satingest - path to obsproc_satingest executable directory
#      TANKDIR               - root of directory path to output BUFR database
#                              tank file (e.g., "/dcom/us007003")
#      TANKFILE              - path to directory and tank file in
#                              $TANKDIR/<YYYYMMDD> (see above) to be created/
#                              appended (e.g., "b012/xx012")
#      jlogfile              - path to joblog file
#      TANKPROTECT           - switch to copy (if "YES") the tank file to a
#                              temporary file before updating it (after which
#                              the temporary tank file will be moved over to
#                              the permanent tank file location; otherwise. the
#                              tank file will be updated in situ)
#      COMOUT                - path to directory containing output file
#                              location when SENDCOM (see below) is "YES" 
#                              (e.g., "/com/${NET}/${envir}/${RUN}.${PDY}")
#      SENDCOM               - switch to copy (if "YES") output files to
#                              $COMOUT
#
#   Condition codes:
#     0 - no problem encountered
#   > 0 - some problem encountered
#     Specifically:  99 - Source of data is unknown (not Navy)
#                   191 - <tankfile>  not specified corectly
#
# Attributes:
#   Language: ksh script
#   Machine:  NCEP WCOSS
#
####

if [ $DEBUGSCRIPTS = YES -o $DEBUGSCRIPTS = ON ] ; then
  set -x
fi

host=$(hostname -s)

table=$1
dsname=$2
file=$DATA/$dsname


pgm=bufr_tranpoessst_navo
export pgm

. $UTILROOT/ush/prep_step

if [ ! -s $file ] ; then
   outstring="$dsname does not exist or has zero length"
   len=${#outstring}
   if [ $len -gt $lenerror ] ; then
      lenerror=$len
   fi
   echo $outstring >> $tmperr
else
   export FORT11="$file"
fi
if [ ! -s $table ] ; then
   outstring="$table does not exist or has zero length"
   len=${#outstring}
   if [ $len -gt $lenerror ] ; then
      lenerror=$len
   fi
   echo $outstring  >> $tmperr
else

   cp $table $DATA/table

   export FORT20="$DATA/table"
fi
typsubdir=$(dirname $TANKFILE)
if [ $typsubdir != "." ] ; then
   subtypfil=$(basename $TANKFILE)
   subtyp=`echo $subtypfil | cut -c3-5`
else
   echo "    <tankfile> must be given as : subdirectory/tankfile>" 
   echo "    Last 3 digits of subdirectory are BUFR type." 
   echo "    Last 3 digits of tankfile are BUFR sub-type." 
   echo "    BUFR type and sub-type may be coded as three" 
   echo "    question marks if translation code provides" 
   echo "    BUFR type and sub-type." 
   exit 191
fi

export typsubdir subtypfil subtyp dsname


#  Check that source of data is Navy based on subtype from TANKFILE
#  ----------------------------------------------------------------------------

if [ $subtyp -eq 012 ]; then
   source=navy
else
   msg="Source of data is unknown (not Navy)"
   len=${#outstring}
   if [ $len -gt $lenerror ] ; then
      lenerror=$len
   fi
   echo $msg  >> $tmperr
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
   exit 99
fi


ymdh=$(date -u '+%Y%m%d%H')

if [ $TANKPROTECT = YES ] ; then
   apndstring=_A_P_N_$dsname
else
   apndstring="' '"
fi

cat <<EOF_parm > $DATA/poessst_navo.parm.$host.$$
 &setup
   nsat=7,
 /
 &sat_list
isstsrc(1)=11, isaid(1)=003, csat_name(1)='METOP-1',
isstsrc(2)=12, isaid(2)=004, csat_name(2)='METOP-2',
isstsrc(3)=13, isaid(3)=005, csat_name(3)='METOP-3',
isstsrc(4)=07, isaid(4)=209, csat_name(4)='NOAA-18',
isstsrc(5)=08, isaid(5)=223, csat_name(5)='NOAA-19',
isstsrc(6)=09, isaid(6)=224, csat_name(6)='NPP   ',
isstsrc(7)=10, isaid(7)=225, csat_name(7)='NOAA-20',
 /
EOF_parm

export FORT15=$DATA/poessst_navo.parm.$host.$$
export FORT51="$DATA/$typsubdir.$subtypfil.$dsname.tmpout.$host.$$"
export FORT71="$DATA/timwin.$dsname.tmpout.$host.$$"

echo "$typsubdir $subtypfil $apndstring $ymdh" | \
 $EXECobsproc_satingest/bufr_tranpoessst_navo
retcode=$?

if [ $retcode -eq 4 ]; then
msg="WARNING: BUFR_TRANPOESSST_NAVO ENCOUNTERED UNKNOWN SATELLITE ID --> non-fatal"
   set +x
   echo
   echo "$msg"
   echo
   set -x
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
   retcode=0
fi

if [ $retcode -eq 0 ] ; then

   set +x
   echo " --------------------------------------------- "
   echo " ********** COMPLETED PROGRAM $pgm  **********"
   echo " --------------------------------------------- "
   set -x
   msg="$pgm completed normally"
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"

   rettot=0
   [[ "$SENDCOM" = YES ]] && cp $DATA/timwin.${dsname}.tmpout.$host.$$ $COMOUT

   if [ -s $DATA/$typsubdir.$subtypfil.$dsname.tmpout.$host.$$ ];then
     [[ "$SENDCOM" = YES ]] && \
        cp $DATA/$typsubdir.$subtypfil.$dsname.tmpout.$host.$$\
          $COMOUT
     sh $TRANush $TANKDIR \
        $DATA/$typsubdir.$subtypfil.$dsname.tmpout.$host.$$
     rc=$?
     rettot=$(($rettot+$rc))
   fi
else
   echo "*******************************************************"
   echo "********  ERROR PROGRAM $pgm RETURN CODE $retcode  ********"
   echo "*******************************************************"
   msg="ERROR PROGRAM $pgm RETURN CODE $retcode"
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
fi
     

rettot=$(($rettot+$retcode))
exit $rettot
