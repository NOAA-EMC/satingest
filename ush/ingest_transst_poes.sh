#!/bin/ksh
# Run under ksh

####  UNIX Script Documentation Block
#
# Script name:   ingest_transst_poes.sh
#
# RFC contact:  Keyser               org: NP22        date: 2014-01-17
#
# Abstract: This script reads in NAVO or NESDIS files containing sea-surface
#   temperature retrievals, AVHRR brightness temperatures and albedo (in MCSST
#   temporary file binary format) from the NOAA-series and METOP-series POES
#   satellites, converts them into NCEP BUFR format, and writes them into the
#   /dcom tank database on the NCEP WCOSS machines.
#
# Script history log:
# 2012-11-18 Diane Stokes - Taken from ingest_transst_physstret.sh and modified
#     to exclude physical retrieval generation.  Files for use by physical
#     retrieval code are save to COMOUT.
# 2014-01-17 Diane Stokes/Dennis Keyser - Now includes hostname as well as
#     process id in temporary filenames where only process id was present
#     before.  USH script tranjb renamed to bufr_tranjb.sh and moved from
#     directory path $USHbufr to directory path $USHobsproc_satingest.  Script
#     exit code now takes into account a non-zero exit code from any of the
#     bufr_tranjb.sh executions inside it as well as a non-zero exit code from
#     its bufr_tranpoessst execution (before it only took into account a non-
#     zero return code from the latter) - this improves error diagnosis
#     downstream.  $EXECobsproc_satingest replaces $EXECbufr as the environment
#     variable representing the directory path to the executables.  Add check
#     of variable $SENDCOM to determine whether to send output to $COMOUT.
#     Added information to docblock and new comments.  Updated some existing
#     comments.
# 2021-12-19 Sudhir Nadiga - Modified to use bufr_tranjb module variables.
#
#
# Usage: ingest_transst_poes.sh  <bufrtable>  <raw_file>
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
#     data cards : none
#     executables: $EXECobsproc_satingest/bufr_tranpoessst
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
#     Specifically:  99 - Source of data is unknown (neither Navy nor NESDIS)
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


pgm=bufr_tranpoessst
export pgm
cwd=`pwd`

. $UTILROOT/ush/prep_step
cd $cwd

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

export typsubdir subtypfil subtyp dsname cycle 


#  Determine if source of data is Navy or NESDIS based on subtype from TANKFILE
#  ----------------------------------------------------------------------------

if [ $subtyp -eq 012 ]; then
   source=navy
elif [ $subtyp -eq 011 ]; then
   source=nesd
else
   msg="Source of data is unknown (neither Navy nor NESDIS)"
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

export FORT51="$DATA/$typsubdir.$subtypfil.$dsname.t00z.metopa.tmpout.$host.$$"
export FORT52="$DATA/$typsubdir.$subtypfil.$dsname.t06z.metopa.tmpout.$host.$$"
export FORT53="$DATA/$typsubdir.$subtypfil.$dsname.t12z.metopa.tmpout.$host.$$"
export FORT54="$DATA/$typsubdir.$subtypfil.$dsname.t18z.metopa.tmpout.$host.$$"
export FORT55="$DATA/$typsubdir.$subtypfil.$dsname.t00z.noaa19.tmpout.$host.$$"
export FORT56="$DATA/$typsubdir.$subtypfil.$dsname.t06z.noaa19.tmpout.$host.$$"
export FORT57="$DATA/$typsubdir.$subtypfil.$dsname.t12z.noaa19.tmpout.$host.$$"
export FORT58="$DATA/$typsubdir.$subtypfil.$dsname.t18z.noaa19.tmpout.$host.$$"
export FORT59="$DATA/$typsubdir.$subtypfil.$dsname.t00z.noaa17.tmpout.$host.$$"
export FORT60="$DATA/$typsubdir.$subtypfil.$dsname.t06z.noaa17.tmpout.$host.$$"
export FORT61="$DATA/$typsubdir.$subtypfil.$dsname.t12z.noaa17.tmpout.$host.$$"
export FORT62="$DATA/$typsubdir.$subtypfil.$dsname.t18z.noaa17.tmpout.$host.$$"
export FORT63="$DATA/$typsubdir.$subtypfil.$dsname.t00z.noaa18.tmpout.$host.$$"
export FORT64="$DATA/$typsubdir.$subtypfil.$dsname.t06z.noaa18.tmpout.$host.$$"
export FORT65="$DATA/$typsubdir.$subtypfil.$dsname.t12z.noaa18.tmpout.$host.$$"
export FORT66="$DATA/$typsubdir.$subtypfil.$dsname.t18z.noaa18.tmpout.$host.$$"
export FORT67="$DATA/$typsubdir.$subtypfil.$dsname.t00z.unknown.tmpout.$host.$$"
export FORT68="$DATA/$typsubdir.$subtypfil.$dsname.t06z.unknown.tmpout.$host.$$"
export FORT69="$DATA/$typsubdir.$subtypfil.$dsname.t12z.unknown.tmpout.$host.$$"
export FORT70="$DATA/$typsubdir.$subtypfil.$dsname.t18z.unknown.tmpout.$host.$$"
export FORT71="$DATA/ymd.$dsname.t00z.tmpout.$host.$$"
export FORT72="$DATA/ymd.$dsname.t06z.tmpout.$host.$$"
export FORT73="$DATA/ymd.$dsname.t12z.tmpout.$host.$$"
export FORT74="$DATA/ymd.$dsname.t18z.tmpout.$host.$$"

echo "$typsubdir $subtypfil $apndstring $ymdh" | \
 $EXECobsproc_satingest/bufr_tranpoessst
retcode=$?

if [ $retcode -eq 4 ]; then
msg="WARNING: BUFR_TRANPOESSST ENCOUNTERED UNKNOWN SATELLITE ID --> non-fatal"
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

## stuff related to physical retrievals deleted ##

#  Loop through the four cycles
#  ----------------------------

   rettot=0
   for cycle in t00z t06z t12z t18z ; do
      cyc=$(echo $cycle | cut -c2-3)
      [[ "$SENDCOM" = YES ]] && cp $DATA/ymd.${dsname}.${cycle}.tmpout.$host.$$ \
       $COMOUT
      adate=$(cat $DATA/ymd.${dsname}.${cycle}.tmpout.$host.$$)

#    Ingest METOP-A, NOAA-17, -18, -19 & unknown into AVHRR tank (in this order)
#    ---------------------------------------------------------------------------

      for sat in metopa noaa17 noaa18 noaa19 unknown ; do
         if [ -s \
          $DATA/$typsubdir.$subtypfil.$dsname.$cycle.$sat.tmpout.$host.$$ ];then
            [[ "$SENDCOM" = YES ]] && \
             cp $DATA/$typsubdir.$subtypfil.$dsname.$cycle.$sat.tmpout.$host.$$\
             $COMOUT
            sh $TRANush $TANKDIR \
             $DATA/$typsubdir.$subtypfil.$dsname.$cycle.$sat.tmpout.$host.$$
            rc=$?
            rettot=$(($rettot+$rc))
            if [ $rc -eq 0 ] ; then
               msg="Appending $sat AVHRR data for $adate$cyc cycle to tank \
$typsubdir/$subtypfil"
               echo $msg
               $UTILROOT/ush/postmsg "$jlogfile" "$msg"
            fi
         fi
      done
   done

else
   echo "*******************************************************"
   echo "********  ERROR PROGRAM $pgm RETURN CODE $retcode  ********"
   echo "*******************************************************"
   msg="ERROR PROGRAM $pgm RETURN CODE $retcode"
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
fi
     

rettot=$(($rettot+$retcode))
######exit $retcode
exit $rettot
