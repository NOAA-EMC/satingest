#!/bin/ksh
# Run under ksh

####  UNIX Script Documentation Block
#
# Script name:   ingest_check_lapsed_data.sh
#
# RFC contact:  Ling      org: NP22        date: 2018-12-06
#
# Abstract: Checks for lapses in the transfer and processing of data files from
#   a remote unix machine.  It uses the appropriate file-processing history
#   files to determine the last time a data file was successfully received and/
#   or processed.  Depending upon the value of HOURS2ALARM, a return code is
#   issued indicating whether a significant break has occurred in file transfer
#   or processing.
#
# Script history log:
# 1998-05-12  Bert Katz   Original version for implementation.
# 1999-11-17  Bert Katz   Added capability for HOURS2ALARM to be a list of
#        numbers.  The list may be any length, but only that part of the the
#        list which corresponds (one-to-one) with the file families in
#        REMOTEDSNGRP will be used.  If the list of numbers in HOURS2ALARM
#        contains only one number, that number will be used as the number of
#        hours before a break in transfer/processing is indicated for ALL file
#        families in REMOTEDSNGRP.  If the list of numbers in HOURS2ALARM
#        contains fewer numbers than the number of file families in
#        REMOTEDSNGRP, then the last number in HOURS2ALARM will be used for all
#        remaining file families in REMOTEDSNGRP which lack a corresponding
#        number in HOURS2ALARM.  If the list of numbers in HOURS2ALARM is in
#        one-to-one correspondence with the list of file families in
#        REMOTEDSNGRP, then each file family in REMOTEDSNGRP is paired with a
#        unique number of hours indicating a break in #transfer/processing.
# 2005-10-11  D. Keyser   Includes the NESDIS 4km (96'th mesh) IMS snow cover
#        data, file imssnow96.grb, as a type for which a lapsed date warning
#        will not be posted to the production joblog file.
# 2006-03-30  P. O'Reilly Modified to create flag files for use with Big
#        Brother monitoring to alert the lapse in data.
# 2006-04-25  P. O'Reilly Modified to add logic to distinguish between two
#        files in the snow job that are identically named, to help distinguish
#        them in Big Brother monitoring.
# 2006-06-14  P. O'Reilly Modified to add logic to distinguish between files
#        that are of CRITICAL or non-CRITICAL nature for Big Brother monitoring.
# 2006-08-12  D. Keyser   Improved documentation and comments, more appropriate
#        messages posted to joblog.  Changed to account for all remote machines
#        now being unix (since MVS CEMSCS machine was replaced with unix DDS
#        machine).
# 2008-01-31  D. Keyser   Now converts each embedded "." character in file
#        group name (normally delineates qualifiers in filename) to "\."
#        (escapes "." so it will be treated as a period and not as a special
#        character in the grep from the file-processing history files to check
#        for lapses in the transfer and processing of data files from the
#        remote machine).  Now converts each embedded "*" character in file
#        group name (wildcard matching any string of 1 or more characters) to
#        ".*" so it will be treated the same way in the grep.  Now converts
#        each "?" character in file group name (wildcard matching exactly 1
#        character) to "." so it will be treated the same way in the grep.  In
#        grep, includes a blank space after converted file group name if it
#        originally contained any "*" or "?" characters - this will clearly
#        define the end of the file name.  Otherwise, do not include the blank,
#        as the end of the filename is a wildcard string extending beyond the
#        file group name here.
# 2010-07-06  D. Keyser   Will not expand filenames which include substitution
#        characters "?" and "*" in case files being sftp'd are on same CCS
#        machine as that in which the job is running.
# 2012-10-18  D. Keyser   Modified to run on WCOSS.   This script is now set to
#        run under ksh shell as the default.
# 2014-01-03  D. Keyser   Renamed to add suffix .sh qualifier.  Added
#        information to docblock and new comments.  Updated some existing
#        comments.
# 2015-04-21  D. Stokes   Removed special check for old 96th mesh IMS ascii 
#        filename pattern which has been obsolete since the upgrade to IMSv3.
# 2017-11-10  D. Keyser   Updated definition of REMOTEDSNGRP in Docblock.
# 2018-12-06  Y. Ling   Updated to run on phase 3 machines.
# 2022-01-18  S. Stegall  Replaced $DATA/ before calling utility scripts and instead 
#        used $UTILROOT/ush/ to properly leverage the prod_util module.
#
#
# Usage: ingest_check_lapsed_data.sh  <history_file>
#
#   Script parameters:
#                 $1: history_file - complete path to file-processing history
#                                    file
#
#   Modules and files referenced:
#     scripts    : $UTILROOT/ush/postmsg
#     data cards : none
#     executables: $NHOUR
#
# Remarks: Invoked by the model scripts existore.sh.ecf and existday.sh.ecf.
#
#   Imported Variables that must be passed in:
#      DATA         - path to current working directory
#      dsname       - file name
#      REMOTEDSNGRP - the leading portion of the name of a family of files from
#                     the remote unix machine (normally this is defined as the
#                     complete path to these files, however if REMOTEDIRGRP is
#                     set, it is then defined as only the files themselves)
#      HOURS2ALARM  - the number of hours that will pass before a return code
#                     is issued to trigger an alarm mechanism
#      jlogfile     - path to joblog file
#      SENDBB       - switch to send messages (if "YES") to Big Brother
#      BBDIR        - Directory for Big Brother status files (if SENDBB=YES)
#      CRITICAL     - switch to flag data red in Big Brother (if "YES") when
#                     there is a lapse in the data
#
#   Imported Variables that can be passed in:
#      DEBUGSCRIPTS - if set to "ON" or "YES", will run with "set -x" on
#                     (intended for debugging)
#                     (default = 'OFF')
#      dsname_hist  - the leading qualifier(s) in the file name containing
#                     file-processing history
#                     (default = 'null')
#
#   Condition codes:
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
echo "                   START CHECK_LAPSED_DATA                             "
echo "#######################################################################"
echo

DEBUGSCRIPTS=${DEBUGSCRIPTS:-OFF}
if [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ] ; then
   set -x
fi

if [ $# -lt 1 ] ; then
   echo "Arguments to ingest_check_lapsed_data.sh are:"
   echo "  (1) file-processing_history_file"
   exit 0
fi

if [[ $SENDBB == 'YES' ]] ; then
  BBDIR=${BBDIR:-$DATA}
  [[ -d $BBDIR ]] || mkdir -m 775 -p $BBDIR
  if [ $? -ne 0 ]; then
    $UTILROOT/ush/err_exit "Could not create BigBrother status file directory: $BBDIR"
  fi
fi

dsname_hist=${dsname_hist:-null}
if [ $dsname_hist.history = $(basename $1) ] ; then
   ftype=file
   filenames=$dsname
else
   ftype=filegroup
   filenames="$REMOTEDSNGRP"
fi

mcount=0
if [ -s $1 ] ; then
   for RECPRO in RECEIVED PROCESSED ; do
      ncount=0
      filenames_save="$filenames"

      echo "$filenames_save" | read firstword restofstring
      while [[ "$firstword" != "" ]] ; do
         filenames_save="$restofstring"
         eval fname=\$firstword

#  Unset $hourwait to undo typesets on it from previous loop (i.e., start fresh)
#  -----------------------------------------------------------------------------

         unset hourwait        # Added this on WCOSS to undo typesets
######## typeset +LZ hourwait  # Direct undoing typesets does not work on WCOSS
######## typeset +Z1 hourwait  # Direct undoing typesets does not work on WCOSS
         if [ "$fname" = concatenate_families -o \
              "$fname" = correlate_familes -o \
              "$fname" = concatenate_fams -o "$fname" = correlate_fams -o \
              "$fname" = concatenate -o "$fname" = correlate -o \
              "$fname" = concat_families -o "$fname" = correl_families -o \
              "$fname" = concat_fams -o "$fname" = correl_fams -o \
              "$fname" = concat -o "$fname" = correl ] ; then
            echo "$filenames_save" | read firstword restofstring
            continue
         fi

#  Prepare file group name for grep'ing in file-processing history file
#  --------------------------------------------------------------------

#  Convert each "." character in file group name (normally delineates
#   qualifiers in filename) to "\." (escapes "." so it will be treated as a
#   period and not as a special character later in the grep)
#  ------------------------------------------------------------------------

         fname_grep1=$(echo "$fname" | sed "s/\./\\\./g")

#  Convert each embedded "*" character in file group name (wildcard matching
#   any string of 1 or more characters) to ".*" (it will then also be treated
#   as a wildcard matching any string later in the grep)
#  ---------------------------------------------------------------------------

         fname_grep2=$(echo $fname_grep1 | sed "s/*/.*/g")

#  Convert each "?" character in file group name (wildcard matching exactly 1
#   character) to "." (it will then also be treated as a wildcard matching
#   exactly one character later in the grep)
#  --------------------------------------------------------------------------

         fname_grep3=$(echo $fname_grep2 | sed "s/?/./g")

#  In grep, include a blank space after converted file group name if it
#   originally contained any "*" or "?" characters - this will clearly define
#   the end of the file name.  Otherwise, do not include the blank, as the
#   end of the filename is a wildcard string extending beyond the file group
#   name here.
#  --------------------------------------------------------------------------

         echo "$fname" | grep -Fe "*" -Fe "?"
         iret=$?
         if [ $iret -eq 0 ]; then
            blank=" "
         else
            blank=""
         fi

         grep $RECPRO $1 | grep "$fname_grep3$blank" | tail -n1 | awk '
            { 
              n=split($0,ymdarr,"/");
              m=split(ymdarr[n-2],yeararr," ");
              l=split(ymdarr[n],dayarr," ");
              k=split($0,hmsarr,":");
              j=split(hmsarr[k-2],hourarr," ");
              print yeararr[m],ymdarr[n-1],dayarr[1],hourarr[j],hmsarr[k-1]
            } ' | read lyear lmonth lday lhour lminute
         retcode=$?
         if [ $retcode -ne 0 ] ; then
            echo "$filenames_save" | read firstword restofstring
            continue
         fi

#  Do not use "typeset -LZ2 lmonth lday lhour lminute cmonth cday chour cminute"
#  because:  1) the expressions in the first and third lines below DO NOT
#               interpret values with leading zeroes as octal
#            2) if you do, then $lhour, $lminute, $chour and $cminute are
#               undefined when their value is 00
#  -----------------------------------------------------------------------------

# the following 2 changes must be made on WCOSS, else you get the error:
#       (standard_in) 1: illegal character: \
#       (standard_in) 1: syntax error
#
   lymdh=$(echo -e "$lyear*1000000+$lmonth*10000+$lday*100+$lhour \n quit" | bc)
         date -u '+%Y %m %d %H %M' | read cyear cmonth cday chour cminute 
   cymdh=$(echo -e "$cyear*1000000+$cmonth*10000+$cday*100+$chour \n quit" | bc)
         hourwait=$($NHOUR $cymdh $lymdh)
         hourwait_orig=$hourwait

#  The following will remove leading zeroes from $hourwait unless $hourwait is
#  numerically equal to 0, in which case it will force it to equal the string
#  '0' rather than '00'
#  ---------------------------------------------------------------------------

         if [ $hourwait -eq 0 ] ; then
            typeset -Z1 hourwait
         else
            typeset -LZ hourwait
         fi

#  The following line modified on later machines so that the shell does not
#  interpret the values for $cminute and $lminute as octal when there are
#  leading zeros
#  (Note: Cannot use "typeset -LZ2 cminute lminute" with original line below
#         for reasons noted in previous comment, reason 2)
#  -------------------------------------------------------------------------

###      minwait=$(($cminute-$lminute))
         minwait=$(expr $cminute - $lminute)
         if [ $minwait -lt 0 ] ; then 

#  Unset $hourwait to undo earlier typesets on it until its new value is known
#  ---------------------------------------------------------------------------

            unset hourwait       # Added this on WCOSS to undo typesets
########    typeset +LZ hourwait # Direct undoing typesets doesn't work on WCOSS
########    typeset +Z1 hourwait # Direct undoing typesets doesn't work on WCOSS

#  The following line modified on later machines so that the shell does not
#  interpret the value for $hourwait as octal when there are leading zeros
#  (Note: Cannot use "typeset -LZ hourwait" with original line below because
#         $hourwait on the right hand side is then undefined when its value
#         is 0)
#  -------------------------------------------------------------------------

###         hourwait=$(($hourwait_orig-1))
            hourwait=$(expr $hourwait_orig - 1)

#  Since hourwait above is generated by an expression, no need to use "typeset"
#   as all leading zeroes from $hourwait are automatically removed (and a value
#   of "0" will remain "0")
#  ----------------------------------------------------------------------------

         fi
         ncount=$(($ncount+1))
         [ $RECPRO = RECEIVED ]  &&  mcount=1
         icount=0
         for hourlimit in $HOURS2ALARM ; do
            icount=$(($icount+1))
            if [ $icount -eq $ncount ] ; then
               break
            fi
         done
         if [ $hourwait -gt $hourlimit ] ; then
            if [ $RECPRO = PROCESSED ] ; then
               $UTILROOT/ush/postmsg "$jlogfile" "REMOTE FILE TRANSFER INGEST: No \
data from $fname processed in the last $hourwait hrs."
              echo "Processing has failed for $hourwait hours on $ftype $fname."
              echo "Processing has failed for $hourwait hours on $ftype $fname."
              echo "Processing has failed for $hourwait hours on $ftype $fname."
            elif [ $RECPRO = RECEIVED ] ; then
               $UTILROOT/ush/postmsg "$jlogfile" "REMOTE FILE TRANSFER INGEST: No \
data from $fname received in the last $hourwait hrs."
             echo "No new files received for $hourwait hours for $ftype $fname."
             echo "No new files received for $hourwait hours for $ftype $fname."
             echo "No new files received for $hourwait hours for $ftype $fname."
            fi
            if [ $SENDBB = 'YES' ]; then
               if [ $CRITICAL = 'YES' ] ; then
	          echo 2 $hourwait > $BBDIR/`basename "$fname"`
	       else
                  echo 1 $hourwait > $BBDIR/`basename "$fname"`
               fi
            fi
         else
            if [ $RECPRO = PROCESSED ] ; then
               $UTILROOT/ush/postmsg "$jlogfile" "REMOTE FILE TRANSFER INGEST: Data \
from $fname processed in the last $hourwait hrs."
            elif [ $RECPRO = RECEIVED ] ; then
               $UTILROOT/ush/postmsg "$jlogfile" "REMOTE FILE TRANSFER INGEST: Data \
from $fname received in the last $hourwait hrs."
            fi
            if [ $SENDBB = 'YES' ]; then
               echo 0 $hourwait > $BBDIR/`basename "$fname"`
            fi
         fi
         echo "$filenames_save" | read firstword restofstring
      done
   done
   if [ $mcount -eq 0 ] ; then
      if [ "$fname" != imssnow.grb -a "$fname" != imssnow96.grb -a \
           "$fname" != snowdepth.grb ] ; then
         oldest_date=`grep " AT " $1 | grep "$fname_grep3$blank" | head -n1 | \
          awk -F" AT " '{print $2}' | cat`
         $UTILROOT/ush/postmsg "$jlogfile" "REMOTE INGEST: No data from $fname \
received since before ${oldest_date}."
      fi
   fi
else
   exit 0
fi
