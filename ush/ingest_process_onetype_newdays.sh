#!/bin/ksh
# Run under ksh

####  UNIX Script Documentation Block
#
# Script name:   ingest_process_onetype_newdays.sh
#
# RFC contact:  Ling      org: NP22        date: 2018-12-06
#
# Abstract: Determines whether the current day and time are consistent with the
#   availability of an updated, statically-named, file from a remote unix
#   machine.  If the current day is on the schedule, or if the file does not
#   yet exist on the local machine (NCEP CCS), a file transfer is initiated.
#
# Script history log:
# 1996-10-03  Bert Katz   Original version for implementation.
# 1996-12-04  Bert Katz   Directory structure reorganized to resemble the BUFR
#     tanking system.
# 1997-01-09  Bert Katz   Unified output in "stdout", corrected the date-
#     stamping procedure, added a facility for "copying forward" the previous
#     day's file to the current day's directory after 00Z, added debug option.
# 1997-01-23  Bert Katz   Corrected handling of first write to the file-
#     processing history file.
# 1997-01-31  Bert Katz   Inserted search of operational script directory ahead
#     of user's script directory to find the processing script to execute, if
#     any extra processing is done.
# 1997-02-12  Bert Katz   Improved "copying forward" so that it will update the
#     new day's directory with the newest version of the file when the file is
#     late and the previous day's directory receives a tardy update.
# 1997-11-26  Bert Katz   Added the ability to "copy forward" a file into the
#     next day's directory without performing any file transfers.  in addition,
#     "copy forward" can be performed more than once if more current files
#     become available for "copying forward".
# 2006-05-12  D. Keyser   Generalized name of remote machine (no longer
#     hardwired to CEMSCS) and changed to account for all remote machines now
#     being unix (since MVS CEMSCS machine was replaced with unix DDS machine).
#     Improved documentation and comments.
# 2012-10-26  D. Keyser   Modified to run on WCOSS. Specifically, since WCOSS
#     default script (sh) is bash, all child scripts are executed under ksh
#     (the CCS default) rather than under sh (some script commands tranferred
#     from CCS version do not work under bash).  This script is now set to run
#     under ksh shell as the default.
# 2014-01-03  Diane Stokes/D. Keyser   Renamed to add suffix .sh qualifier.
#     Now includes hostname as well as process id in temporary filenames where
#     only process id was present before.  Changed all "date" commands to
#     "date -u" since WCOSS should always present date in UTC.  All references
#     to "GMT" changed to "UTC".  Imported variable $timetype now checked for
#     value of "UTC" rather than "GMT" ($timetype redefined to be "UTC" when
#     time type is Greenwich).  $USHobsproc_satingest replaces $USHbufr as the
#     environment variable representing the directory path to the ush scripts.
#     Added information to docblock and new comments.  Updated some existing
#     comments.
# 2017-11-10  D. Keyser   Fixed a minor comment typo.
# 2018-12-06  Y. Ling   Updated to run on phase 3 machine.
# 2022-01-18  S. Stegall  Replaced $DATA/ before calling utility scripts and instead 
#      used $UTILROOT/ush/ to properly leverage the prod_util module.
#
#
# Usage: ingest_process_onetype_newdays.sh
#
#   Script parameters: none
#
#   Modules and files referenced:
#     scripts    : $USHobsproc_satingest/ingest_process_days.sh
#                  $UTILROOT/ush/postmsg
#                  $USHobsproc_satingest/$PROCSCRIPT
#                  $USERDIR/$PROCSCRIPT
#     data cards : none
#     executables: $NDATE
#
# Remarks: Invoked by the model script existday.sh.ecf.
#
#   Imported Variables that must be passed in:
#      DATA                   - path to current working directory
#      dsname                 - name of the file to be transferred from the
#                               remote unix machine
#      jlogfile               - path to joblog file
#      DEBUGSCRIPTS           - if set to "ON" or "YES", will run with "set -x"
#                               on (intended for debugging)
#      dsname_hist            - the leading qualifier(s) in the file name
#                               containing file-processing history (e.g.,
#                               ${dsname_hist}.history)
#      TANKDIR                - root of directory path to output file (e.g.,
#                               "/dcom/us007003")
#      TANKSUBDIR             - directory in $TANKDIR/<YYYYMMDD> containing
#                               output file (e.g., "wgrdbul")
#      TANKFILE               - name of output file in
#                               $TANKDIR/<YYYYMMDD>/$TANKSUBDIR
#                               (e.g., "PRD.AEROSOL.FIELD.KM100")
#      timetype               - if set to "LOCAL", all times for processing
#                               will be assumed to be in local time, if set to
#                               "UTC", all times for processing will be assumed
#                               to be in UTC, if set to "USER_SPEC", the
#                               calling program is specifying a "hardwired"
#                               time for processing using the script variable
#                               $user_spec_timedatecurr (see below)
#      user_spec_timedatecurr - hardwired time which will be used as the time
#                               in all processing, it is specified in the form
#                               "yyyymmdd doy dow HHMM", where "yyyy" is year,
#                               "mm" is month, "dd" is day of month, "doy" is
#                               day of year, "dow" is day of week (1=Monday,
#                               ..., 7=Sunday), "HH" is hour and "MM" is
#                               minute.  This is only invoked if timetype is
#                               set to "USER_SPEC" (see above)
#      timemade               - HHMM (UTC) when the file(s) to be requested
#                               from the remote unix machine will be available
#      daysavail              - the schedule for the availablity of the file;
#                               The following options are available:
#                                (1) Some subset of
#                                    "Sun Mon Tue Wed Thu Fri Sat"
#                                (2) "DAY OF MONTH = n"
#                                (3) "DAY OF MONTH = n MODULO k"
#                                (4) "DAY OF YEAR = n"
#                                (5) "DAY OF YEAR = n MODULO k"
#                               Note: If set to "justcopy" or "justcopyforward"
#                                     the file is NEVER transferred from the
#                                     remote unix machine, it is ALWAYS simply
#                                    copied from the previous day
#      dayafter               - if the file will always be available on the
#                               calendar day after the date with which it
#                               should be date-stamped, then "YES" should be
#                               specified, otherwise should be "NO"
#      ndayarch               - maximum number of days that input files
#                               previously transferred from the remote unix
#                               machine will be kept in current date directory
#                               $TANKDIR/$datecurr/$TANKSUBDIR in the event no
#                               more recent files become available for
#                               transferring
#      USHobsproc_satingest   - path to obsproc_satingest ush directory
#      USERDIR                - path to directory containing file-processing
#                               history file (e.g., "$TANKDIR/ingest_hist");
#                               also can be alternate path to directory
#                               containing $PROCSCRIPT if it is not found in
#                               $USHobsproc_satingest
#      MACHINE                - name of remote unix machine to be used in
#                               transfer requests
#      COPYFORWARD            - if set to "YES", the first request for a file
#                               on a given day will result in the most recent
#                               available file being "copied forward" from the
#                               older directory to the current day's directory,
#                               if the file being remotely provided changes
#                               later that day, the new copy will be retrieved
#                               and overwrite the file that was
#                               "copied forward"
#      PROCSCRIPT             - the name of the script to perform processing on
#                               the file (normally not set, meaning no further
#                               processing is required)
#
# Condition codes:
#     0 - no problem encountered
#   > 0 - some problem encountered
#
# Attributes:
#   Language: ksh script
#   Machine:  NCEP WCOSS
#
####

set -au

echo
echo "#######################################################################"
echo "                START INGEST_PROCESS_ONETYPE_NEWDAYS                   "
echo "#######################################################################"
echo

if [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ] ; then
   set -x
fi

set +A WEEKDAYS SUN MON TUE WED THU FRI SAT

dsname_local=`basename $dsname`

if [ $timetype = LOCAL ] ; then
   timedatecurr=$(date '+%Y%m%d %j %w %H%M')
elif [ $timetype = UTC ] ; then
   timedatecurr=$(date -u '+%Y%m%d %j %w %H%M')
else
   timedatecurr=$user_spec_timedatecurr
fi
echo $timedatecurr | read datecurr doycurr dowcurr timecurr

#  DAK:  Following typeset added so that possible leading zero in variable
#        doycurr will not be treated as octal value.  If doycurr is numerically
#        equal to 1, always retain 1 character regardless of value so that the
#        arithmetic expression doycurr=$(($doycurr-1)) will be interpreted as
#        '0' and not as undefined.
#  ----------------------------------------------------------------------------

if [ $doycurr -eq 1 ] ; then
   typeset -Z1 doycurr
else
   typeset -LZ7 doycurr
fi

domcurr=$(($datecurr%100))
ymdhcurr=$(echo -e "100*$datecurr+$timecurr/100 \n quit" | bc)

if [ ! -d $TANKDIR/$datecurr/$TANKSUBDIR ] ; then
   mkdir -m 775 -p $TANKDIR/$datecurr/$TANKSUBDIR
fi

if [ $COPYFORWARD = YES ] ; then
   idayarch=0
   found=0
   dsncopied=0

# Don't copy forward if new file already received from remote machine for today

   if [ -s $USERDIR/${dsname_hist}.history ] ; then
      dsncopied=$(grep -c "RECEIVED FROM REMOTE MACHINE, $dsname (for \
$datecurr) COPIED" $USERDIR/${dsname_hist}.history)
#VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
      if [ $dsncopied -eq 0 ]; then
         dsncopied=$(grep -c "$dsname (for $datecurr) RECEIVED" \
$USERDIR/${dsname_hist}.history)
      fi
#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
   fi
   if [ $dsncopied -gt 0 ]; then
      msg="earlier processing has ALREADY RECEIVED $dsname from remote \
machine and COPIED file to today"
      set +x
      echo
      echo $msg
      echo
      [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
      $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      found=1
   fi

# Don't copy forward if new file already created and written for today

   if [ $dsncopied -eq 0 ]; then
      if [ -s $USERDIR/${dsname_hist}.history ] ; then
         dsncopied=$(grep -c "$dsname (for $datecurr) CREATED and WRITTEN" \
$USERDIR/${dsname_hist}.history)
      fi
      if [ $dsncopied -gt 0 ]; then
         msg="earlier processing has ALREADY CREATED and WRITTEN $dsname to \
today"
         set +x
         echo
         echo $msg
         echo
         [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
         $UTILROOT/ush/postmsg "$jlogfile" "$msg"
         found=1
      fi
   fi

   ymdhback=$ymdhcurr
   while [ $idayarch -lt $ndayarch -a $found -eq 0 ] ; do
      idayarch=$(($idayarch+1))
      ymdhback=$($NDATE -24 $ymdhback)
      dateback=$(echo -e "$ymdhback/100 \n quit" | bc)
      dsncopied=0
      if [ -s $USERDIR/${dsname_hist}.history ] ; then
         if [ $PROCSCRIPT = nullexec ] ; then
            dsncopied=$(grep -c "$dsname_local (for $datecurr) COPIED (from \
$dateback)" $USERDIR/${dsname_hist}.history)
         else
            dsncopied=$(grep -c "$dsname_local (for $datecurr) COPIED and \
PROCESSED (from $dateback)" $USERDIR/${dsname_hist}.history)
         fi
      fi
      if [ $dsncopied -eq 0 ] ; then
         if [ -s $TANKDIR/$dateback/$TANKSUBDIR/$TANKFILE ] ; then
            found=1
         else    
            found=0
         fi
         if [ $found -eq 1 ] ; then
            cp $TANKDIR/$dateback/$TANKSUBDIR/$TANKFILE \
             $TANKDIR/$datecurr/$TANKSUBDIR
            if [ $PROCSCRIPT != nullexec ] ; then
               cd $TANKDIR/$datecurr/$TANKSUBDIR
               if [ -s $USHobsproc_satingest/$PROCSCRIPT ] ; then
                  sh $USHobsproc_satingest/$PROCSCRIPT 
                  retcode=$?
               elif [ -s $USERDIR/$PROCSCRIPT ] ; then
                  sh $USERDIR/$PROCSCRIPT $TANKFILE $dateback
                  retcode=$?
               fi
               if [ $retcode -eq 0 ] ; then
                  echo "$dsname_local (for $datecurr) COPIED and PROCESSED \
(from $dateback) AT `date -u +%Y/%m/%d' '%H:%M:%S' UTC'`" >> \
                   $USERDIR/${dsname_hist}.history
                  msg="$dsname_local copied and processed from \
$TANKDIR/$dateback/$TANKSUBDIR to $TANKDIR/$datecurr/$TANKSUBDIR"
                  set +x
                  echo
                  echo $msg
                  echo
                  [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
                  $UTILROOT/ush/postmsg "$jlogfile" "$msg"
               fi
            else
               echo "$dsname_local (for $datecurr) COPIED (from $dateback) AT \
`date -u +%Y/%m/%d' '%H:%M:%S' UTC'`" >> $USERDIR/${dsname_hist}.history
               msg="$dsname_local copied from $TANKDIR/$dateback/$TANKSUBDIR \
to $TANKDIR/$datecurr/$TANKSUBDIR"
               set +x
               echo
               echo $msg
               echo
               [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
               $UTILROOT/ush/postmsg "$jlogfile" "$msg"
            fi
         fi
      else
         if [ "$daysavail" = justcopy -o "$daysavail" = justcopyforward ] ; then
            msg="earlier processing has ALREADY COPIED $dsname_local forward \
to today"
         else
            msg="earlier processing has ALREADY COPIED $dsname_local forward \
to today, check to see if new file is available for today on remote machine"
         fi
         set +x
         echo
         echo $msg
         echo
         [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
         $UTILROOT/ush/postmsg "$jlogfile" "$msg"
         found=1
      fi
   done
   if [ $found -eq 0 ] ; then
      msg="$dsname_local NOT copied forward to today as it was not found for \
any previous day --> non-fatal"
      set +x
      echo
      echo $msg
      echo
      [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
      $UTILROOT/ush/postmsg "$jlogfile" "$msg"
   fi
fi

if [ "$daysavail" = justcopy -o "$daysavail" = justcopyforward ] ; then
   exit 0
fi

if [ $dayafter = YES ] ; then
   ymdhcurr=$($NDATE -24 $ymdhcurr)
   dowcurr=$(($dowcurr-1))
   domcurr=$(($domcurr-1))
   doycurr=$(($doycurr-1))
fi
if [ $timecurr -lt $timemade ] ; then
   ymdhnext=$ymdhcurr
   datenext=$datecurr
   ymdhcurr=$($NDATE -24 $ymdhcurr)
   dowcurr=$(($dowcurr-1))
   domcurr=$(($domcurr-1))
   doycurr=$(($doycurr-1))
fi
datecurr=$(echo -e "$ymdhcurr/100 \n quit" | bc)
cd $TANKDIR
if [ ! -d $datecurr/$TANKSUBDIR ] ; then
   mkdir -m 775 -p $datecurr/$TANKSUBDIR
fi
cd $datecurr/$TANKSUBDIR

daysavail=$(echo $daysavail | tr [a-z] [A-Z])
dayofmonth=$(echo $daysavail | grep -c "MONTH")
dayofyear=$(echo $daysavail | grep -c "YEAR")
if [ $dayofmonth -eq 0 -a $dayofyear -eq 0 ] ; then
   if [ $dowcurr -lt 0 ] ; then
      dowcurr=$(($dowcurr+7))
   fi
   daycurr=${WEEKDAYS[$dowcurr]}
   set +x
   echo
   echo "Data for $dsname is available on the following days :"
   echo $daysavail
   echo
   [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
   dayfound=$(echo $daysavail | grep -c $daycurr)
   if [ -s $TANKFILE -a $dayfound -eq 0 ] ; then
      set +x
      echo
      echo "Since today is $daycurr, file transfers will not proceed."
      echo
      exit 
   elif [ $dayfound -ne 0 ] ; then
      set +x
      echo
      echo "Since today is $daycurr, file transfers will proceed."
      echo
   else
      set +x
      echo
      echo "Since today is $daycurr, but $dsname does not exist, file \
transfers will proceed."
      echo
   fi
   [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
else
   if [ $dayofmonth -eq 1 ] ; then
      if [ $domcurr -lt 1 ] ; then
         domcurr=$(($datecurr%100))
      fi
      daycurr=$domcurr
      label="day of month"
   elif [ $dayofyear -eq 1 ] ; then
      if [ $doycurr -lt 1 ] ; then
         yrmod4=$(echo -e "$ymdhcurr/1000000%4 \n quit" | bc)
         if [ $yrmod4 -eq 0 ] ; then
            yrmod100=$(echo -e "$ymdhcurr/1000000%100 \n quit" | bc)
            yrmod400=$(echo -e "$ymdhcurr/1000000%400 \n quit" | bc)
            if [ $yrmod100 -ne 0 ] ; then
               doycurr=$(($doycurr+366))
            elif [ $yrmod400 -eq 0 ] ; then
               doycurr=$(($doycurr+366))
            else
               doycurr=$(($doycurr+365))
            fi
         else
            doycurr=$(($doycurr+365))
         fi
      fi
      daycurr=$doycurr
      label="day of year"
   fi
   set +x
   echo
   echo "Data for $dsname is available on the following days :"
   echo $daysavail
   echo $daysavail | awk ' { n=split($0,arr,"=")
                             m=split(arr[2],opermod,"MODULO")
                             if(m==1) print opermod[1]," 0"
                             else print opermod[1],opermod[2]
                           }' | read dayproc divisor
   echo
   [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
   if [ $divisor -ne 0 ] ; then
      quot=$(($daycurr/$divisor))
      remain=$(($daycurr-$quot*$divisor))
      if [ -s $TANKFILE -a $remain -ne $dayproc ] ; then
         set +x
         echo
         echo "Since $label $daycurr != $dayproc modulo $divisor, file \
transfers will not proceed."
         echo
         exit
      elif [ $remain -eq $dayproc ] ; then
         set +x
         echo
         echo "Since $label $daycurr = $dayproc modulo $divisor, file \
transfers will proceed."
         echo
      else
         set +x
         echo
         echo "Since $label $daycurr != $dayproc modulo $divisor, but $dsname \
does not exist, file transfers will proceed."
         echo
      fi
      [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
   else
      if [ -s $TANKFILE -a $daycurr -ne $dayproc ] ; then
         set +x
         echo
      echo "Since $label $daycurr != $dayproc, file transfers will not proceed."
         echo
         exit
      elif [ $daycurr -eq $dayproc ] ; then
         set +x
         echo
         echo "Since $label $daycurr = $dayproc, file transfers will proceed."
         echo
      else
         set +x
         echo
         echo "Since $label $daycurr != $dayproc, but $dsname does not exist, \
file transfers will proceed."
         echo
      fi
      [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
   fi
fi  
ksh $USHobsproc_satingest/ingest_process_days.sh $MACHINE
