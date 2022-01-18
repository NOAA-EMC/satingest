#!/bin/ksh
# Run under ksh

####  UNIX Script Documentation Block
#
# Script name:   ingest_process_onetype_neworbits.sh
#
# RFC contact:  Keyser      org: NP22        date: 2017-11-10
#
# Abstract: Determines the existence of yet-to-be processed files in one family
#   of time-stamped files from a remote unix machine. Processing is initiated
#   if it is determined that any such files exist. 
#
# Script history log:
# 1996-10-03  Bert Katz   Original version for implementation.
# 1996-11-29  Bert Katz   Unified the output file.
# 1997-01-09  Bert Katz   Unified output goes to "stdout", refined neworbit
#     determination, added debug option.
# 1997-03-13  Bert Katz   Added safeguards to keep simultaneous executions from
#     stepping on each others' toes.
# 1997-07-10  Bert Katz   Corrected the code which determines the varying part
#     of the filename for the case in which only one file exists on the CEMSCS.
# 1997-10-14  Bert Katz   Modified to handle multiple input file families.
# 1997-12-17  Bert Katz   Changed return code 199 to 254 so as not to conflict
#     with return code from process_orbits.  Made construction of the "newlist"
#     file more reliable by starting a new "newlist" file with the first file
#     group and then concatenating the remaining file groups (if any) onto the
#     "newlist" file.
# 1998-05-08  Bert Katz   Simplified script by removing array notation.
# 1998-07-20  Bert Katz   Added option to concatenate or correlate multiple
#     input file families.  Simplified sorting procedure by eliminating the
#     determination of the varying portion of the filename: it took more time
#     than it saved.  Added a sort for the newlist file: necessary for multiple
#     input file families.
# 1999-05-11  Bert Katz   Changed criteria for exiting so that non-existence of
#     files for a single query no longer causes processing to terminate.
#     Processing will only terminate if all queries fail to find files to
#     process.  This will allow processing to continue upon concatenated
#     families of of files.
# 2003-05-30  L Sager     Added scripting to drop any new orbits which have
#     been migrated from CEMSCS.
# 2006-02-23  D. Keyser   Modified to remove processing which used ftp to check
#     for and remove migrated AIRS files (including center f-o-v AIRS, warmest
#     f-o-v AIRS, and AMSR-E), one at a time.  This is no longer needed because
#     script ingest_cemscsquery now removes migrated files (prior to this) in a
#     more efficient manner.  This change will allow the parent AIRS ingest job
#     to run much faster, especially when there are a lot of migrated files to
#     filter out.  Improved script Docblock.
# 2006-05-12  D. Keyser   Combines/generalizes this script (previously for
#     CEMSCS machine only) and script ingest_unixproc_onetype_neworbits (for
#     unix machines only).  Changed to account for all remote machines now
#     being unix (since MVS CEMSCS machine was replaced with unix DDS machine).
#     Improved documentation and comments, more appropriate messages posted to
#     joblog.
# 2007-05-14  D. Keyser    Now uses imported script variable IFILES_MAX_GET to
#     determine the maximum number of new files on the remote machine for which
#     transferring and processing will occur, anything above this results in no
#     file processing (had been unlimited except for special case of ingest
#     processing executed from RUC2A dump jobs where it was hardwired to "8" -
#     this special case has now been removed)
# 2010-07-06  D. Keyser   Will not expand filenames which include substitution
#     characters "?" and "*" in case files being sftp'd are on same CCS machine
#     as that in which the job is running.
# 2012-06-26  D. Keyser   Always sorts newlist file, not just for multiple
#     input file families as before. The "ls" command on some new remote
#     machines does not return the listing of files in alphabetical order
#     (e.g., trmmrt.gsfc.nasa.gov).
# 2012-10-18  D. Keyser   Modified to run on WCOSS. Specifically, since WCOSS
#     default script (sh) is bash, all child scripts are executed under ksh
#     (the CCS default) rather than under sh (some script commands tranferred
#     from CCS version do not work under bash). This script is now set to run
#     under ksh shell as the default.
# 2014-01-14  Diane Stokes/D. Keyser   Renamed to add suffix .sh qualifier.
#     Now includes hostname as well as process id in temporary filenames where
#     only process id was present before.  Changed all "date" commands to
#     "date -u" since WCOSS should always present date in UTC.  Now accounts
#     for rare cases where a particular group of files in a concatenated family
#     was not found on the remote unix machine due to a connection/timeout
#     issue in the query.  These files will now not be removed from the
#     housekeeping file containing the list of files on the remote machine file
#     listing coming out of this run.  This prevents a future run, where
#     connection is ok in query, from considering these as new or repeat files.
#     $USHobsproc_satingest replaces $USHbufr as the environment variable
#     representing the directory path to the ush scripts.  Added information to
#     docblock and new comments.  Updated some existing comments.
# 2015-11-17  D. Keyser   If environment variable PROC_MULT_FILES is imported
#     as "YES" (i.e., concatenate all input files pulled from remote server
#     prior to processing), resets it to "NO" (process each input file one at a
#     time) and prints dignostic to both jlogfile and stdout if either:
#        1) the value of imported environment variable IFILES_MAX_MULT (the
#     maximum number of new files on the remote machine for which the files
#     will be concatenated prior to processing, defaults to 100 but can be
#     user-specified) is less than the number of new files found on the remote
#     server for the family being processed (prevents the creation of a
#     concatenated file that might be so large that it greatly slows down
#     processing)
#                  -- or --
#        2) the imported environment variable FTYPE (input file type for the
#     family being processed) is neither "bufr" nor "ncepbufr" (some non-BUFR
#     files go through additional processing that precludes their being
#     concatenated prior to such processing).
# 2016-06-10  D. Stokes   If more than $IFILES_MAX_GET new files are found,
#     transfer and process the first $IFILES_MAX_GET new files, leaving the rest
#     to be processed another time.  (Previously, the script would exit without
#     processing any of the new files if $IFILES_MAX_GET was exceeded.)
# 2017-11-10  D. Keyser   Updated definition of REMOTEDSNGRP in Docblock.
# 2022-01-18  S. Stegall  Replaced $DATA/ before calling utility scripts and instead 
#      used $UTILROOT/ush/ to properly leverage the prod_util module.
#
# Usage: ingest_process_onetype_neworbits.sh
#
#   Script parameters: none
#
#   Modules and files referenced:
#     scripts    : $USHobsproc_satingest/ingest_query.sh
#                  $USHobsproc_satingest/ingest_process_orbits.sh
#                  $UTILROOT/ush/postmsg
#     data cards : none
#     executables: none
#
# Remarks: Invoked by the model script existore.sh.ecf.
#
#   Imported Variables that must be passed in:
#      DATA                 - path to current working directory
#      jlogfile             - path to joblog file
#      REMOTEDSNGRP         - the leading portion of the name of a family of
#                             files from the remote unix machine (normally this
#                             is defined as the complete path to these files,
#                             however if REMOTEDIRGRP is set (i.e., anything
#                             other than '.'), it is then defined as only the
#                             files themselves)
#      ORBITLIST            - complete path to the housekeeping file which
#                             contains a listing of all files in the "family"
#                             seen on the remote unix maxchine by the last run
#                             (this file will be updated via the addition of
#                             any new files on the remote unix machine found by
#                             this run and via the deletion of any old files
#                             on the remote unix machine not found by this
#                             run); also complete file path to the leading
#                             qualifier(s) in the housekeeping file
#                             containing a listing of all files in the "family"
#                             now seen on the remote unix maxchine by this run
#                             ($ORBITLIST.newlist.$host.$$); also complete file
#                             path to the leading qualifier(s) in the file name
#                             containing a listing of all new files in the
#                             "family" found on the remote unix machine by this
#                             run ($ORBITLIST.neworbits.$host.$$); also
#                             complete file path to the leading qualifier(s)
#                             in the file name containing a listing of all old
#                             files in the "family" no longer found on the
#                             remote unix machine by this run
#                             ($ORBITLIST.oldorbits.$host.$$)
#      DEBUGSCRIPTS         - if set to "ON" or "YES", will run with "set -x"
#                             on (intended for debugging)
#      USHobsproc_satingest - path to obsproc_satingest ush directory
#      MACHINE              - name of remote unix machine to be used in
#                             transfer requests
#      IFILES_MAX_GET       - the maximum number of new files on the remote
#                             machine for which transfers for a file family
#                             will occur. if greater than $IFILES_MAX_GET new
#                             files are found on the remote machine for a
#                             particular file family, only the first
#                             $IFILES_MAX_GET files will be transferred and 
#                             processed for that family.
#
# Condition codes:
#     0 - no problem encountered
#   > 0 - some problem encountered
#     Specifically:   1 - Query failed to produce any files to process
#                   111 - All files to be processed in a particular group
#                         (family) were unprocessable
#                         (condition code coming out of
#                          ingest_process_orbits.sh)
#                   199 - One or more files were untransferable in last 5 (or
#                         possibly some other number) runs of this job
#                         (condition code coming out of
#                          ingest_process_orbits.sh)
#                   220 - No action specified for multiple file families
#                   222 - All files to be processed were already processed
#                         (condition code coming out of
#                          ingest_process_orbits.sh)
#                   230 - No files submitted for processing
#                         (condition code coming out of
#                          ingest_process_orbits.sh)
#                   254 - Failure in sort of either $ORBITLIST or
#                          $ORBITLIST.newlist.$host.$$ housekeeping files (see
#                          above for definitions of these files)
#
# Attributes:
#   Language: ksh script
#   Machine:  NCEP WCOSS
#
####

set -au

echo
echo "#######################################################################"
echo "                  START INGEST_PROCESS_ONETYPE_NEWORBITS               "
echo "#######################################################################"
echo

echo
echo "Processing file group $REMOTEDSNGRP"
echo "Start time is $(date -u)."
echo

if [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ] ; then
   set -x
fi

host=$(hostname -s)

if [ -s $ORBITLIST.newlist.$host.$$ ] ; then
   rm $ORBITLIST.newlist.$host.$$
fi
nfam=0
REMDSNGRP=""
unset CONCATCORREL

REMOTEDSNGRP_save="$REMOTEDSNGRP"


#  Loop through each DSN family listed in $REMOTEDSNGRP to get a list of new
#  files to process
#  -------------------------------------------------------------------------

echo "$REMOTEDSNGRP_save" | read firstword restofstring
while [[ "$firstword" != "" ]] ; do
   REMOTEDSNGRP_save="$restofstring"
   eval DIRDSNFAM=\$firstword

#  See if the words concatenate_families or correlate_families are in the list
#  of groups
#  ---------------------------------------------------------------------------

   CONCORTEST=$(echo "$DIRDSNFAM" | tr [A-Z] [a-z])
   if [ $CONCORTEST = concatenate_families -o \
      $CONCORTEST = correlate_families ] ; then
      CONCATCORREL=$CONCORTEST
      echo "$REMOTEDSNGRP_save" | read firstword restofstring
      continue
   elif [ $CONCORTEST = concat -o $CONCORTEST = concatenate -o \
          $CONCORTEST = concat_fams -o $CONCORTEST = concat_families -o \
           $CONCORTEST = concatenate_fams ] ; then
      CONCATCORREL=concatenate_families
      echo "$REMOTEDSNGRP_save" | read firstword restofstring
      continue
   elif [ $CONCORTEST = correl -o $CONCORTEST = correlate -o \
          $CONCORTEST = correl_fams -o $CONCORTEST = correl_families -o \
           $CONCORTEST = correlate_fams ] ; then
      CONCATCORREL=correlate_families
      echo "$REMOTEDSNGRP_save" | read firstword restofstring
      continue
   fi
   nfam=$(($nfam+1))
   REMDSNGRP="${REMDSNGRP}$DIRDSNFAM "
   DSNFAM=$(basename "$DIRDSNFAM")

#  Get listing of datasets (beginning with $DSNFAM) available on remote unix
#  machine $MACHINE - dataset listing is returned in file
#  $DSNFAM.newlist.$host.$$
#  ---------------------------------------------------------------------------

   ksh $USHobsproc_satingest/ingest_query.sh $MACHINE $DSNFAM.newlist.$host.$$\
    "$DIRDSNFAM"
   transerror=$?

   set +x
   echo
   echo "Time is now $(date -u)."
   echo
   [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x

   if [ $transerror -ne 0 ] || [ ! -s $DSNFAM.newlist.$host.$$ ]; then
      set +x
      echo
      echo "No $DIRDSNFAM files located on remote unix machine $MACHINE."
      echo
      [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x

#  Possibly, this particular group of files in the concatenated family (usually
#  from a single satellite) was not found on the remote unix machine due to a
#  connection/timeout issue in the query. See if this group of files is in the
#  remote machine file listing from the last run. If so, and if the file-
#  processing history file contains processing info for them, assume a
#  connection/timeout did occur in the query for this run. Grep these files out
#  of the remote machine file listing from the last run for later cat'ing onto
#  the remote machine file listing from this run. This prevents future runs
#  from seeing this group of files on the remote machine again and thinking
#  they are new or repeat files.
#  (Note: The condition that at least one file in this group of files must be
#         in the file-processing history file to satisfy the connection/timeout
#         scenario allows for a situation where a particular group of files in
#         the concatenated family actually does suddenly stop being posted to
#         the remote unix machine. Here, once they age off the file-processing
#         history file they will no longer be restored onto the remote machine
#         file listing and will never again be considered in future runs
#         (unless they once again appear on the remote unix machine.)
#  ----------------------------------------------------------------------------

# Change all "?" values to "." in $DSNFAM so grep works properly
      dsnfam=$(echo $DSNFAM | sed "s/?/./g")
      grep $dsnfam $ORBITLIST > restore.listing_${nfam}.$host.$$
      grep -q $dsnfam $ORBITLIST.history
      errgrep=$?
      if [ -s restore.listing_${nfam}.$host.$$ -a $errgrep -eq 0 ]; then
         msg="***WARNING: $DIRDSNFAM files not located due to possible \
connection/timeout issue in query - restore them in file-listing history,"
         set +x
         echo
         echo $msg
         echo
         [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
         $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      else
         > restore.listing_${nfam}.$host.$$
      fi
   else
      > restore.listing_${nfam}.$host.$$
   fi

#  Save list of multiple families of files to same file
#  ----------------------------------------------------

   if [ -s $ORBITLIST.newlist.$host.$$ ] ; then
      cat $DSNFAM.newlist.$host.$$ >> $ORBITLIST.newlist.$host.$$
      rm $DSNFAM.newlist.$host.$$
   else
      mv $DSNFAM.newlist.$host.$$ $ORBITLIST.newlist.$host.$$
   fi

   echo "$REMOTEDSNGRP_save" | read firstword restofstring
done
if [ ! -s $ORBITLIST.newlist.$host.$$ ] ; then
   set +x
   echo
   echo "Exiting - query failed to produce any files to process."
   echo "Ending time for ingest_process_onetype_neworbits.sh is $(date -u)."
   echo
   exit 1
fi

#  Before sorting the list of files seen on the remote unix maxchine by this
#  run, check to see if any group of files from a concatenated family need to
#  be added back on to the list (see above why they might hve been incorrectly
#  removed from this list and why this resoration is needed)
# ----------------------------------------------------------------------------

ifam=0
while [[ "$ifam" < "$nfam" ]] ; do
   ifam=$(($ifam+1))
   cat restore.listing_${ifam}.$host.$$ >> $ORBITLIST.newlist.$host.$$
   rm restore.listing_${ifam}.$host.$$
done

#  Sort the list of files now seen on the remote unix maxchine by this run
#  -----------------------------------------------------------------------

sort -d -o $ORBITLIST.tempsort.$host.$$ $ORBITLIST.newlist.$host.$$
if [ $? -eq 0 ] ; then
   awk ' { print $1 } ' $ORBITLIST.tempsort.$host.$$ > \
    $ORBITLIST.newlist.$host.$$
   rm $ORBITLIST.tempsort.$host.$$
else
   set +x
   echo
   echo "Exiting - failure in sort of list of files now seen on the remote \
unix maxchine by this run."
   echo "Ending time for ingest_process_onetype_neworbits.sh is $(date -u)."
   exit 254
fi

#  Sort the list of files seen on the remote unix maxchine by the last run
#  -----------------------------------------------------------------------

if [ -s $ORBITLIST ] ; then
   sort -d -o $ORBITLIST.tempsort.$host.$$ $ORBITLIST
   if [ $? -eq 0 ] ; then
      awk ' { print $1 } ' $ORBITLIST.tempsort.$host.$$ > $ORBITLIST
      rm $ORBITLIST.tempsort.$host.$$
   else
      set +
      echo
      echo "Exiting - failure in sort of list of files seen on the remote \
unix maxchine by the last run."
      echo "Ending time for ingest_process_onetype_neworbits.sh is $(date -u)."
      echo
      exit 254
   fi

#  Determine if this run found are any new files on the remote unix maxchine
#  that now need to be processed
#  -------------------------------------------------------------------------

   comm -13 $ORBITLIST $ORBITLIST.newlist.$host.$$ > \
    $ORBITLIST.neworbits.$host.$$

#  Determine if any old files were removed from the remote unix machine since
#  the last run
#  --------------------------------------------------------------------------

   comm -23 $ORBITLIST $ORBITLIST.newlist.$host.$$ > \
    $ORBITLIST.oldorbits.$host.$$

   if [ -s $ORBITLIST.oldorbits.$host.$$ ] ; then
      set +x
      echo
      echo "The following time-stamped files have been deleted on remote unix \
machine $MACHINE since the last run :"
      cat $ORBITLIST.oldorbits.$host.$$
      echo
      [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x

#  Update the housekeeping file containing the list of files on the remote unix
#  machine: remove the old files no longer present on the unix machine since
#  the last run
#  ----------------------------------------------------------------------------

      comm -12 $ORBITLIST $ORBITLIST.newlist.$host.$$ > \
       $ORBITLIST.remaining.$host.$$
      mv $ORBITLIST.remaining.$host.$$ $ORBITLIST
   fi
   rm $ORBITLIST.newlist.$host.$$ $ORBITLIST.oldorbits.$host.$$

   if [ ! -s $ORBITLIST.neworbits.$host.$$ ] ; then
      msg=" No new time-stamped files found on remote unix machine $MACHINE \
since the last run - no further processing is done."
      set +x
      echo
      echo $msg
      echo
      [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
      $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      rm $ORBITLIST.neworbits.$host.$$
      set +x
      echo
      echo "Ending time for ingest_process_onetype_neworbits.sh is $(date -u)."
      echo
      exit 0
   fi
else 
   mv $ORBITLIST.newlist.$host.$$ $ORBITLIST.neworbits.$host.$$
fi

#  Process each new file found on the unix machine found since the last run
#  ------------------------------------------------------------------------

#  Update the housekeeping file containing the list of files on the remote unix
#  machine: add the new files now found on the unix machine since the last run
#  ----------------------------------------------------------------------------

set +x
echo
echo "Time is now $(date -u)."
echo
echo "The following time-stamped files have been added on remote unix machine \
$MACHINE since the last run and will require processing :"
cat $ORBITLIST.neworbits.$host.$$
echo
[ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x

num_files=$(wc -l < $ORBITLIST.neworbits.$host.$$)

#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

#  Identify any new files which are likely repeats and would not be processed
#   - adjust value of num_files to not include these (thus will not be included
#   when num_files is compared to values for IFILES_MAX_GET or IFILES_MAX_MULT
#   to make processing decisions)
#  ----------------------------------------------------------------------------

grep PROCESSED $ORBITLIST.history > orbitlist.history_PROCESSED.$host.$$
cut -d" " -f1  orbitlist.history_PROCESSED.$host.$$ > \
 orbitlist.history_PROCESSED_cut.$host.$$
sort -d $ORBITLIST.neworbits.$host.$$ > neworbits_sort.$host.$$
sort -d orbitlist.history_PROCESSED_cut.$host.$$ > \
 orbitlist.history_PROCESSED_cut_sort.$host.$$
awk '{$1=$1};1' neworbits_sort.$host.$$ > neworbits_sort_R.$host.$$
awk '{$1=$1};1' orbitlist.history_PROCESSED_cut_sort.$host.$$ > \
 orbitlist.history_PROCESSED_cut_sort_R.$host.$$
comm -12 neworbits_sort_R.$host.$$ \
 orbitlist.history_PROCESSED_cut_sort_R.$host.$$ > likely_repeats.$host.$$
if [ -s likely_repeats.$host.$$ ]; then
   set +x
   echo
   echo "Correction: The following time-stamped files added on remote unix \
machine $MACHINE since the last run are likely repeats which"
   echo "            would not be included in the processing :"
   cat likely_repeats.$host.$$
   echo
fi
[ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
num_files_repeat=$(wc -l < likely_repeats.$host.$$)
num_files=`expr $num_files - $num_files_repeat`
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

if [ $num_files -gt $IFILES_MAX_GET ]; then
   msg="***WARNING: $num_files new files exceeds the limit of $IFILES_MAX_GET\
 for this family - Process only the first $IFILES_MAX_GET files"
   set +x
   echo
   echo $msg
   echo
   [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
   mv $ORBITLIST.neworbits.$host.$$ $ORBITLIST.neworbits.$host.$$.full_list
   head --lines=$IFILES_MAX_GET $ORBITLIST.neworbits.$host.$$.full_list > $ORBITLIST.neworbits.$host.$$
   rm $ORBITLIST.neworbits.$host.$$.full_list
   num_files=$IFILES_MAX_GET
fi

if [ $PROC_MULT_FILES = YES ]; then
   if [ $num_files -gt $IFILES_MAX_MULT ]; then
      msg="***WARNING: The number of new files for this family, $num_files, \
exceeds the limit of $IFILES_MAX_MULT - input files will be processed 1 by 1 \
rather than concatenated"
      $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      export PROC_MULT_FILES=NO
      set +x
      echo
      echo $msg
      echo
   elif [ $FTYPE != bufr -a $FTYPE != ncepbufr ]; then
      msg="***WARNING: FTYPE is imported as $FTYPE, it must be bufr or ncepbufr \
for input files to be concatenated - input files will be processed 1 by 1"
      $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      export PROC_MULT_FILES=NO
      set +x
      echo
      echo $msg
      echo

   fi
fi

if [ $nfam -gt 1 ] ; then
   CONCATCORREL=${CONCATCORREL:-correlate_families}
else
   CONCATCORREL=${CONCATCORREL:-concatenate_families}
fi
if [ $CONCATCORREL = correlate_families ] ; then
   awk -v REMDSNFAMS="$REMDSNGRP" '
    BEGIN { 
            nfamily=split(REMDSNFAMS,remdsnfams," ");
            for(i=1;i<=nfamily;i=i+1) 
            {
              lenfam[i]=length(remdsnfams[i]);
              numfam[i]=0
            }
          }
          {
            for(i=1;i<=nfamily;i=i+1) 
              if(index($1,remdsnfams[i])!=0) 
              { 
                numfam[i]=numfam[i]+1;
                tail[i,numfam[i]]=substr($1,lenfam[i]+1,length($1)-lenfam[i]);
                lines[i,numfam[i]]=$0
              }
          }
    END   { 
            for(i=1;i<=numfam[1];i=i+1)
            {
              icount=1;
              prtline=lines[1,i];
              for(j=2;j<=nfamily;j=j+1)
              {
                for(k=1;k<=numfam[j];k=k+1)
                {
                  if(tail[j,k]==tail[1,i])
                  {
                    icount=icount+1;
                    prtline=prtline " " lines[j,k]
                  }
                }
              }
              if(icount==nfamily) print prtline
            }
          }' $ORBITLIST.neworbits.$host.$$ | \
              ksh $USHobsproc_satingest/ingest_process_orbits.sh 9>> $ORBITLIST
   orberror=$?
elif [ $CONCATCORREL = concatenate_families ] ; then 
   ksh $USHobsproc_satingest/ingest_process_orbits.sh < \
    $ORBITLIST.neworbits.$host.$$ 9>> $ORBITLIST
   orberror=$?
else
   set +x
   echo
   echo "Exiting - No action specified for multiple file families."
   echo "Ending time for ingest_process_onetype_neworbits.sh is $(date -u)."
   echo
   exit 220
fi
rm $ORBITLIST.neworbits.$host.$$
if [ $orberror -ne 0 ] ; then
   set +x
   echo
   echo "Ending time for ingest_process_onetype_neworbits.sh is $(date -u)."
   echo
   exit $orberror
fi
set +x
echo
echo "Ending time for ingest_process_onetype_neworbits.sh is $(date -u)."
echo

