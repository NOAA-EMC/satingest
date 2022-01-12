#!/bin/ksh
# Run under ksh

####  UNIX Script Documentation Block
#
# Script name:   ingest_process_days.sh
#
# RFC contact:  Ling      org: NP22        date: 2018-12-06
#
# Abstract: Initiates the transfer of one requested, statically-named, file
#   from a remote unix maxchine.
#
# Script history log:
# 1996-10-03  Bert Katz   Original version for implementation.
# 1996-11-15  Bert Katz   Updated to handle snow field processing.
#     this involved separating the receiving of the file from the processing of
#     the file, with both events recorded separately in the file-processing
#     history file.
# 1996-12-04  Bert Katz   Reorganized file structure to resemble the BUFR
#     tanking system.
# 1997-01-09  Bert Katz   Unified output in "stdout", added debug option
# 1997-01-23  Bert Katz   Corrected handling of first write to the file-
#     processing history file.
# 1997-01-31  Bert Katz   Inserted search of operational script directory ahead
#     of user's script directory to find the processing script to execute, if
#     any extra processing is done.
# 1997-02-12  Bert Katz   Improved "copying forward" so that it will update the
#     new day's directory with the newest version of the file when the file is
#     late and the previous day's directory receives a tardy update.
# 1998-05-12  Bert Katz   Changes for y2k compliance
# 2006-05-12  D. Keyser   Generalized name of remote machine (no longer
#     hardwired to CEMSCS) and changed to account for all remote machines now
#     being unix (since MVS CEMSCS machine was replaced with unix DDS machine).
#     Improved documentation and comments; more appropriate messages posted to
#     joblog.
# 2006-09-29  D. Keyser   If transfer fails on first attempt (for whatever
#     reason) now sleeps 30 sec and tries a second time, if this also fails
#     script gives up (allows transfer to bypass possible momentary ftp
#     glitches)
# 2008-01-24  D. Keyser   Checks value of new imported script variable
#     "MTYPSBT" - if "YES", now executes program bufr_tranmtypsbt to: 1) change
#     each BUFR message in output statically-named BUFR file to have BUFR type
#     and subtype as determined by input script variable "tankfile"
#     (bttt/xxsss, where ttt=BUFR type and sss=BUFR subtype); and 2) encode
#     the BUFR menmonic table bufrtab.ttt (where ttt is obtained from input
#     script variable "tankfile") into the top of the output statically-named
#     BUFR file (in place of any dictionary messages that may have previously
#     been there), output (stdout and stderr) from bufr_tranmtypsbt is written
#     into directory specified by imported script variable "OUTDIR"
# 2013-01-07  D. Keyser   Modified to run on WCOSS. Specifically, replaces CCS
#     script variables XLFUNIT_n with FORTn (where n is the unit number
#     connected to the filename defined by the variable FORTn) - needed because
#     ifort uses FORTn.  Also, since WCOSS default script (sh) is bash, all
#     child scripts are executed under ksh (the CCS default) rather than under
#     sh (some script commands tranferred from CCS version do not work under
#     bash).  This script is now set to run under ksh shell as the default.
# 2014-01-03  Diane Stokes/D. Keyser   Renamed to add suffix .sh qualifier.
#     Now includes hostname as well as process id in temporary filenames where
#     only process id was present before.  Changed all "date" commands to
#     "date -u" since WCOSS should always present date in UTC.  All references
#     to "GMT" changed to "UTC".  Removed all references to script variables
#     $CNVRT2F77 and $ATTRIBUTES and logic which executed cnvblk since latter
#     is now obsolete.  $USHobsproc_satingest replaces $USHbufr as the
#     environment variable representing the directory path to the ush scripts.
#     $EXECobsproc_satingest replaces $EXECbufr as the environment variable
#     representing the directory path to the executables.  Added information to
#     docblock and new comments.  Updated some existing comments.
# 2018-12-06  Y. Ling  Updated to run on phase 3 machine.
#
#
# Usage: ingest_process_days.sh
#
#   Script parameters: None
#
#   Modules and files referenced:
#     scripts    : $USHobsproc_satingest/ingest_get.sh
#                  $UTILROOT/ush/postmsg
#                  $UTILROOT/ush/prep_step
#                  $USHobsproc_satingest/$PROCSCRIPT
#                  $USERDIR/$PROCSCRIPT
#     data cards : none
#     executables: $EXECobsproc_satingest/bufr_tranmtypsbt
#                  $NDATE
#
# Remarks: Invoked by the script ingest_process_onetype_newdays.sh.
#
#   Imported Variables that must be passed in:
#      DATA                  - path to current working directory
#      dsname                - name of the file to be transferred from the
#                              remote unix machine
#      dsname_local          - base name of the file to be transferred from the
#                              remote unix machine (excluding any directory)
#      jlogfile              - path to joblog file
#      DEBUGSCRIPTS          - if set to "ON" or "YES", will run with "set -x"
#                              on (intended for debugging)
#      dsname_hist           - the leading qualifier(s) in the file name
#                              containing file-processing history (e.g.,
#                              ${dsname_hist}.history)
#      TANKDIR               - root of directory path to output file (e.g.,
#                              "/dcom/us007003")
#      TANKSUBDIR            - directory in $TANKDIR/<YYYYMMDD> containing
#                              output file (e.g., "wgrdbul")
#      TANKFILE              - name of output file in
#                              $TANKDIR/<YYYYMMDD>/$TANKSUBDIR
#                              (e.g., "PRD.AEROSOL.FIELD.KM100")
#      datecurr              - current date in form YYYYMMDD
#      datenext              - initially current date in form YYYYMMDD
#      timecurr              - current time in form HHMM (UTC)
#      timemade              - HHMM (UTC) when the file(s) to be requested from
#                              the remote unix machine will be available
#      ndayarch              - maximum number of days that input files
#                              previously transferred from the remote unix
#                              machine will be kept in current date directory
#                              $TANKDIR/$datecurr/$TANKSUBDIR in the event no
#                              more recent files become available for
#                              transferring
#      EXECobsproc_satingest - path to obsproc_satingest executable directory
#      USHobsproc_satingest  - path to obsproc_satingest ush directory
#      USERDIR               - path to directory containing file-processing
#                              history file (e.g., "$TANKDIR/ingest_hist");
#                              also can be alternate path to directory
#                              containing $PROCSCRIPT if it is not found in
#                              $USHobsproc_satingest
#      OUTDIR                - path to directory containing output listing
#                              (usually same as $TANKDIR)
#      MACHINE               - name of remote unix machine to be used in
#                              transfer requests
#      MTYPSBT               - if set to "YES", the program
#                              $EXECobsproc_satingest/bufr_tranmtypsbt will be
#                              executed in order to encode the external BUFR
#                              table into the beginning of the statically-named
#                              file and to change the BUFR type and subtype
#                              internally in each non-dictionary BUFR message
#      tankfile              - updated BUFR type (TTT) and subtype (SSS) in
#                              each non-dictionary BUFR message created from
#                              $EXECobsproc_satingest/bufr_tranmtypsbt when
#                              MTYPSBT is 'YES' (in form bTTT/xxSSS,
#                              e.g., "b008/xx011")
#      COPYFORWARD           - if set to "YES", the first request for a file on
#                              a given day will result in the most recent
#                              available file being "copied forward" from the
#                              older directory to the current day's directory,
#                              if the file being remotely provided changes
#                              later that day, the new copy will be retrieved
#                              and overwrite the file that was "copied forward"
#      PROCSCRIPT            - the name of the script to perform processing on
#                              the file (normally not set, meaning no further
#                              processing is required)
#      HISTLENMIN            - minimum length (in lines) of the file-processing
#                              history file
#      HISTLENMAX            - maximum length (in lines) of the file-processing
#                              history file
#
# Condition codes:
#     0 - no problem encountered
#   > 0 - some problem encountered
#     Specifically:   1 - Transfer of file failed in ingest_get.sh
#
# Attributes:
#   Language: ksh script
#   Machine:  NCEP WCOSS
#
####

set -au

echo
echo "#######################################################################"
echo "                   START INGEST_PROCESS_DAYS                           "
echo "#######################################################################"
echo

if [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ] ; then
   set -x
fi

host=$(hostname -s)

if [ -s $USERDIR/${dsname_hist}.history ] ; then
   dsnfound=$(grep -c "$dsname (for $datecurr) RECEIVED" \
$USERDIR/${dsname_hist}.history)
else
   dsnfound=0
fi

if [ $dsnfound -eq 0 ] ; then
   newday="$dsname"
   
###itries_max=1
   itries_max=2
   set +x
   echo
   echo "Time is now $(date -u)."
   echo
   [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x

   transerror=99
   itries=1
   while [ $transerror -gt 0 -a $itries -le $itries_max ]; do
      if [ $itries -gt 1 ]; then
         msg="TRANSFER OF $dsname FAILED!!!! - SLEEP 30 SEC AND TRY AGAIN."
         $UTILROOT/ush/postmsg "$jlogfile" "$msg"
         sleep 30
      fi
      ksh $USHobsproc_satingest/ingest_get.sh $MACHINE $dsname_local "$newday"\
       2>&1
      transerror=$?
      itries=`expr $itries + 1`
   done
   itries=`expr $itries - 1`

   set +x
   echo
   echo "Time is now $(date -u)."
   echo
   [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x

   if [ $transerror -ne 0 ] ; then
      msg="Exiting with rc = $transerror - TRANSFER OF $dsname FAILED AFTER \
$itries TRIES!!!!"
      $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      set +x
      echo
      echo $msg
      echo "Time is now $(date -u)."
      echo
      exit $transerror
   fi
   msg="TRANSFER OF $dsname successful on try no. ${itries}."
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
   mv $dsname_local ${dsname_local}_temp
   if [ $MTYPSBT = YES ] ; then
#==============================================================================
#              Execute program $EXECobsproc_satingest/bufr_tranmtypsbt
#==============================================================================
      export encode_bufrtable=YES # encodes bufrtab.ttt into top of output file
      outfile=$OUTDIR/bufr_tranmtypsbt.out
      tmpout=$DATA/bufr_tranmtypsbt.tempout.$host.$$
      errlog=$OUTDIR/bufr_tranmtypsbt.errlog
      tmperr=$DATA/bufr_tranmtypsbt.temperr.$host.$$
      outstring="bufr_tranmtypsbt will run on file $dsname_local"
      lenstring=${#outstring}
      dashes="---------------------------------------------------------------"
      if [ -s $outfile ] ; then
         lenoutfile=$(cat $outfile | wc -l)
      else
         lenoutfile=0
      fi
      if [ -s $errlog ] ; then
         lenerrlog=$(cat $errlog | wc -l)
      else
         lenerrlog=0
      fi
      datestring="bufr_tranmtypsbt will run on \
$(date -u '+%Y/%m/%d at %H:%M:%S') UTC"
      lendatestring=${#datestring}
      [ $lendatestring -gt $lenstring ]  &&  lenstring=$lendatestring
      dashstring=$(echo $dashes$dashes | cut -c1-$lenstring)
      echo $dashstring >> $tmpout
      echo $outstring  >> $tmpout
      echo $datestring >> $tmpout
      echo $dashstring >> $tmpout
      cp $tmpout $tmperr
      TANKFILE_save=$TANKFILE
      export TANKFILE=$tankfile
      mv $dsname_local ${dsname_local}_temp
      export FORT11="${dsname_local}_temp"
      pgm=bufr_tranmtypsbt
      msg="$pgm has BEGUN"
      $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      set +u

#  Note - must use "$UTILROOT/ush/prep_step" here not ". $UTILROOT/ush/prep_step" because the
#         latter would unset the FORT* variables that have previously been
#         been set.  These may still be used in subsequent programs in this
#         script.
#######   . $UTILROOT/ush/prep_step
      $UTILROOT/ush/prep_step
      set -u
      export FORT51="$dsname_local"
      $EXECobsproc_satingest/bufr_tranmtypsbt >> $tmpout 2>&1
      ier=$?
      datestring="bufr_tranmtypsbt : rc = $ier on \
$(date -u '+%Y/%m/%d at %H:%M:%S') UTC"
      lendatestring=${#datestring}
      outstring="bufr_tranmtypsbt has run on file $dsname_local"
      len=${#outstring}
      [ $len -gt $lenstring ]  &&  lenstring=$len
      [ $lendatestring -gt $lenstring ]  &&  lenstring=$lendatestring
      dashstring=$(echo $dashes$dashes | cut -c1-$lenstring)
      [ $ier -ne 0 ]  &&  echo $dashstring >> $tmperr
      echo $dashstring >> $tmpout
      [ $ier -ne 0 ]  &&  echo $outstring >> $tmperr
      echo $outstring >> $tmpout
      if [ $ier -ne 0 ] ; then
         echo $datestring >> $tmperr
         echo $dashstring >> $tmperr
      fi
      echo $datestring >> $tmpout
      echo $dashstring >> $tmpout
      if [ $lenoutfile -gt 130000 ] ; then
         head -n 120000 $outfile > $outfile.head.$host.$$
         cat $tmpout $outfile.head.$host.$$ > $outfile
         rm $outfile.head.$host.$$
      else
         cat $tmpout $outfile > $outfile.temp.$host.$$
         mv $outfile.temp.$host.$$ $outfile
      fi
      cat $tmpout
      rm $tmpout
      TANKFILE=$TANKFILE_save
      if [ $ier -eq 0 ] ; then
         rm ${dsname_local}_temp
      else
         msg="Exiting with rc = $ier - BUFR_TRANMTYPSBT FAILED"
         $UTILROOT/ush/postmsg "$jlogfile" "$msg"
         set +x
         echo
         echo $msg
         echo "Time is now $(date -u)."
         echo
         set +x
         if [ $lenerrlog -gt 4000 ] ; then
            head -n 3000 $errlog > $errlog.head.$host.$$
            cat $tmperr $errlog.head.$host.$$ > $errlog
            rm $errlog.head.$host.$$
         else
            cat $tmperr $errlog > $errlog.temp.$host.$$
            mv $errlog.temp.$host.$$ $errlog
         fi
         rm $tmperr
         exit $ier
      fi
#==============================================================================
   else
      mv ${dsname_local}_temp $dsname_local
   fi 
   cd $TANKDIR/$datecurr/$TANKSUBDIR
   if [ ! -s $TANKFILE -a $COPYFORWARD != YES ] ; then
      idayarch=0
      found=0
      ymdhback=$(echo -e "$datecurr*100 \n quit" | bc)
      while [ $idayarch -lt $ndayarch -a $found -eq 0 ] ; do
         idayarch=$(($idayarch+1))
         ymdhback=$($NDATE -24 $ymdhback)
         dateback=$(echo -e "$ymdhback/100 \n quit" | bc)
         if [ -s $TANKDIR/$dateback/$TANKSUBDIR/$TANKFILE ] ; then
            cp $TANKDIR/$dateback/$TANKSUBDIR/$TANKFILE .
            found=1
         fi
      done
   fi
   if [ -s $TANKFILE ] ; then
      cmp -s $TANKFILE $DATA/$dsname_local
      diffrc=$?
      if [ $diffrc -eq 0 ] ; then
         if [ $COPYFORWARD != YES ] ; then
            rm $TANKFILE
         fi
         exit 0
      fi
   fi
   cp $DATA/$dsname_local $TANKFILE
   echo "$dsname (for $datecurr) RECEIVED AT \
`date -u +%Y/%m/%d' '%H:%M:%S' UTC'`" >> $USERDIR/${dsname_hist}.history
   if [ $dsname_local = $TANKFILE ]; then
      msg="$dsname copied from remote unix machine to \
$TANKDIR/$datecurr/$TANKSUBDIR directory"
   else
      msg="$dsname copied from remote unix machine to \
$TANKDIR/$datecurr/$TANKSUBDIR/$TANKFILE"
   fi
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
   if [ $COPYFORWARD = YES -a $timecurr -lt $timemade ] ; then
      cp $TANKFILE $TANKDIR/$datenext/$TANKSUBDIR
      if [ $PROCSCRIPT = nullexec ] ; then
         echo "$dsname (for $datenext) COPIED AGAIN (from $dateback) AT \
`date -u +%Y/%m/%d' '%H:%M:%S' UTC'`" >> $USERDIR/${dsname_hist}.history
         msg="$dsname copied again to $TANKDIR/$datenext/$TANKSUBDIR/$TANKFILE"
         $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      fi
   fi
fi
if [ -s $USERDIR/${dsname_hist}.history ] ; then
   procfound=$(grep -c "$dsname (for $datecurr) PROCESSED" \
$USERDIR/${dsname_hist}.history)
else
   procfound=0
fi
if [ $procfound -eq 0 ] ; then
   if [ $PROCSCRIPT != nullexec ] ; then
      cd $TANKDIR/$datecurr/$TANKSUBDIR
      if [ -s $USHobsproc_satingest/$PROCSCRIPT ] ; then
         sh $USHobsproc_satingest/$PROCSCRIPT
         procrc=$?
      elif [ -s $USERDIR/$PROCSCRIPT ] ; then
         sh $USERDIR/$PROCSCRIPT $TANKFILE $datecurr 
         procrc=$?
      fi
      if [ $procrc -eq 0 ] ; then
         echo "$dsname (for $datecurr) PROCESSED AT \
`date -u +%Y/%m/%d' '%H:%M:%S' UTC'`" >> $USERDIR/${dsname_hist}.history
      fi
      if [ $COPYFORWARD = YES -a $timecurr -lt $timemade ] ; then
         ymdhback=$ymdhcurr
         dateback=$datecurr
         ymdhcurr=$ymdhnext
         datecurr=$datenext
         cd $TANKDIR/$datecurr/$TANKSUBDIR
         if [ -s $USHobsproc_satingest/$PROCSCRIPT ] ; then
            sh $USHobsproc_satingest/$PROCSCRIPT 
            retcode=$?
         elif [ -s $USERDIR/$PROCSCRIPT ] ; then
            sh $USERDIR/$PROCSCRIPT $TANKFILE $dateback
            retcode=$?
         fi
         cd $TANKDIR/$dateback/$TANKSUBDIR
         if [ $retcode -eq 0 ] ; then
            echo "$dsname (for $datecurr) COPIED and PROCESSED AGAIN (from \
$dateback) AT `date -u +%Y/%m/%d' '%H:%M:%S' UTC'`" >> \
             $USERDIR/${dsname_hist}.history
         fi
      fi
   fi
fi

histlen=`wc -l < $USERDIR/${dsname_hist}.history`
if [ $histlen -gt $HISTLENMAX ] ; then 
   histcut=$(($histlen-$HISTLENMIN))
   awk ' BEGIN   { nlines = 0 }
                 { 
                   nlines = nlines + 1;
                   if(nlines>'$histcut') print $0
                 } 
       ' $USERDIR/${dsname_hist}.history > $USERDIR/${dsname_hist}.newhist
   mv $USERDIR/${dsname_hist}.newhist $USERDIR/${dsname_hist}.history
fi

