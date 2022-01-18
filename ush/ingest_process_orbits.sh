#!/bin/ksh
# Run under ksh

####  UNIX Script Documentation Block
#
# Script name:   ingest_process_orbits.sh
#
# RFC contact:  Y. Ling/D. Keyser    org: NP22        date: 2016-06-10
#
# Abstract: Initiates the transfer and processing of time-stamped files from a
#   remote unix machine.  
#
# Script history log:
# 1996-10-03  Bert Katz   Original version for implementation.
# 1996-11-29  Bert Katz   Unified the output file.
# 1997-01-09  Bert Katz   Unified output goes to "stdout", difficulty in
#     reading exactly one file is now considered to be a benign problem, debug
#     option added.
# 1997-04-07  Bert Katz   Moved error handling up from script
#     "translate_orbits", and added an option to execute another script instead
#     of "translate_orbits", (such as "tranjb") if desired.  Provided a summary
#     of file processing activities, as well as a graceful exit if ftp transfer
#     of a file fails 5 consecutive times 
# 1997-10-14  Bert Katz   Modified to handle multiple input file families.
# 1997-12-15  Bert Katz   Inability to transfer one of a related N-tuple of
#     files from N file families will cause the rest of the N-tuple to be
#     skipped so that transfer of the next N-tuple  files will begin.
# 1998-12-02  Bert Katz   Modified to use Y2K compliant version of tranjb.
# 2001-05-15  L. Sager    Added option to unix uncompress data under control of
#     keyword UNCOMPRESS_UNIX in the event that the file transferred from
#     NESDIS is unix-compressed.
# 2006-02-08  D. Keyser   Replaced call to obsolete program GRABBUFR with call
#     to program CWORDSH (via ush script cwordsh) which has been augmented to
#     perform all functions which GRABBUFR had done (and more).
# 2006-05-12  D. Keyser   Combines/generalizes this script (previously for
#     CEMSCS machine only) and script ingest_unixproc_orbits (for unix machines
#     only).  Changed to account for all remote machines now being unix (since
#     MVS CEMSCS machine was replaced with unix DDS machine).  Improved
#     documentation and comments.  Now exits immediately with rc=230 and posts
#     message to joblog if no files submitted for processing (used to exit
#     immediately with rc=0 and did not post a message to joblog).
# 2007-05-14  D. Keyser    Now uses imported script variable ITRIES_MAX_GET to
#     determine maximum number of failed attempts to get a file from remote
#     machine via ftp before giving up (had been hardwired to "2"); minor
#     script changes regarding the posting of messages to the joblog file
# 2008-01-31  D. Keyser    "copy_to_target" date processing now more
#     generalized {handles date qualifier Dyyddd in any qualifier in file name
#     vs. only in qualifier 5 before, handles files with date qualifier in the
#     form mmddyyyy (new autosnow files)}, streamlines yyddd to yyyymmdd
#     conversion
# 2011-01-05  D. Keyser   Modified to post a message to the joblog file after
#     the successful execution of cwordsh in the event that cwordsh is unable
#     to block 1 or more input BUFR messages.  Export DX_SKIP=YES prior to
#     calls to cwordsh so that dictionary messages will not be copied by that
#     program (see cwordsh for more information on DX_SKIP).
# 2012-11-14  D. Keyser   Modified to run on WCOSS. Specifically, since WCOSS
#     default script (sh) is bash, all child scripts are executed under ksh
#     (the CCS default) rather than under sh (some script commands tranferred
#     from CCS version do not work under bash). This script is now set to run
#     under ksh shell as the default. Script cwordsh now unblocks files rather
#     than blocks files.
# 2012-11-14  S. Melchior Modified to replace uncompress with gunzip since 
#     uncompress is not installed on WCOSS.
# 2014-01-17  Diane Stokes/D. Keyser   Renamed to add suffix .sh qualifier.
#     Now includes hostname as well as process id in temporary file names where
#     only process id was present before.  Changed all "date" commands to
#     "date -u" since WCOSS should always present date in UTC.  All references
#     to "GMT" changed to "UTC".  USH script cwordsh renamed to bufr_cword.sh
#     and moved from directory path USHbufr to directory path
#     USHobsproc_shared_bufr_cword.  USH script tranjb renamed to
#     bufr_tranjb.sh and moved from directory path USHbufr to directory path
#     USHobsproc_satingest.  Removed all references to script variables
#     CNVRT2F77 and ATTRIBUTES and logic which executed cnvblk since latter
#     is now obsolete.  USHobsproc_satingest replaces USHbufr as the
#     environment variable representing the directory path to the ush scripts.
#     Added information to docblock and new comments.  Updated some existing
#     comments.
# 2014-07-30  D. Keyser   If script variable TARGETFILE is imported as
#     "same_name" for case when script variable EXECUTE is imported as
#     "copy_to_target", then files pulled from the remote machine are simply
#     copied to a directory path identified as "$TANKDIR/$TANKFILE" with no
#     change in the file name.
# 2015-07-03  D. Keyser
#          - If script variable TARGETFILE is imported as "same_name2" for case
#     when script variable EXECUTE is imported as "copy_to_target", then files
#     pulled from the remote machine are simply copied to a directory path
#     identified as "$TANKDIR/<YYYYMMDD>/$TANKFILE" with no change in the file
#     name.  <YYYYMMDD> is obtained from qualifier in file name.
#          - "copy_to_target" date processing now more generalized {handles
#     date qualifier of form yyyymmdd either delimited by periods (".") or by
#     underscores ("_") (date processing occurs when TARGETFILE is imported as
#     either "same_name2" or as the name of the output file).
#     These changes allow new blended global biomass burning emmission product
#     and burned area product files to be ingested.
# 2015-08-07  D. Keyser
#          - Improved docblock description for variable FTYPE, and updated its
#     definition to reflect that it no longer controls whether or not file
#     uncompression will be performed if UNCOMPRESS_UNIX is set to YES.
#          - Updated definition of script variable UNCOMPRESS_UNIX to reflect
#     that it no longer has a dependency on FTYPE value, and that files will be
#     uncompressed only if they contain 1 of the 8 specific file name suffixes
#     noted in the gzip man page.
#          - File uncompression (when UNCOMPRESS_UNIX is imported as YES) now
#     occurs regardless of value of FTYPE.  Before it could only occur when
#     FTYPE was imported as bufr.  Allows incoming compressed files from all
#     sources, and in all formats, to be uncompressed (if desired) prior to
#     downstream processing.
#          - If uncompression is unsuccessful (when UNCOMPRESS_UNIX is imported
#     as YES), a more detailed message is posted to the jlogfile (i.e., that
#     processing continues with original file since since it is likely not
#     compressed).  The same message is now also posted to stdout.
#          - The construction of the uncompressed file name in variable dsname
#     passed to the executing script or program, needed in order to match the
#     file name output from the gunzip command which uncompresses the file when
#     UNCOMPRESS_UNIX is imported as YES, now works for all 8 file name
#     suffixes noted in the gzip man page (the suffix is removed from the
#     uncompressed file name).  Before, the construction of the uncompressed
#     file name in variable dsname it did not work properly for file names with
#     suffixes ".gz" and "-gz".  Opens up file uncompression to more types of
#     input compressed files.
#               - If for some (highly rare) reason the construction of the
#     uncompressed file name in variable dsname fails, a message will now be
#     posted to the jlogfile and to stdout noting that subsequent processing of
#     the file must be skipped.  All the usual bells and whistles follow
#     (message posted to $ORBITLIST.history, non-processed family file counter
#     bumped by 1, etc.). Steps are made to actually stop any additional
#     attempts to process file.
#          - If CWORDSH processing fails for a file, a more detailed message is
#     posted to the jlogfile noting that subsequent processing of the file must
#     be skipped.  The same message is now also posted to stdout.  All the
#     usual bells and whistles follow (message posted to $ORBITLIST.history,
#     non-processed family file counter bumped by 1, etc.).
# 2015-11-09  D. Keyser
#          - HISTLENMIN and HISTLENMAX now used to also define minimum/maximum
#     length (in lines) of the new "${ORBITLIST}_copy.history" files, created
#     when script variable EXECUTE is imported as "copy_to_target" and script
#     variable TARGETFILE is imported as "same_name2", as well as the minimum/
#     maximum length of the existing "$USERDIR/$TARGETFILE.history" created
#     when script variable EXECUTE is imported as "copy_to_target" and script
#     variable TARGETFILE is imported as the name of the output file.
# 2015-11-16  D. Keyser
#          - For case when script variable script variable EXECUTE is imported
#     as "copy_to_target" and script variable TARGETFILE is imported as either
#     "same_name2" or as the name of the output file, the date processing
#     (i.e., pulling <YYYYMMDD> from qualifier in file name) is further
#     generalized to handle cases where the qualifier (coming after either a
#     "_" or "." delimiter) is yyyymmdd suffixed with any number of
#     alphanumeric characters. This change allows new global geostationary
#     satellite composite mosaic imagery files which have date qualifier in
#     form <yyyymmddhh> to be properly ingested.
# 2015-11-17  D. Keyser
#          - If new environment variable PROC_MULT_FILES is imported as "YES"
#     for the particular family, each new BUFR file pulled from the remote
#     server is concatenated with previous new files pulled over, in order to
#     create a single BUFR file that is then sent on for subsequent processing.
#     If PROC_MULT_FILES s imported as "NO" (the default) then, as before, each
#     individual file pulled from the remote server is processed separately.
#          - In order to facilitate the new logic above, a large portion of
#     this script, which deals with file processing, has been moved into a new
#     ush script ingest_process_orbits_subscript.sh which is sourced by
#     ingest_process_orbits.sh at different locations in the logic based on the
#     value of PROC_MULT_FILES.
#          - An error in the execution of gunzip for compressed files (when
#     UNCOMPRESS_UNIX=YES) now results in the file being skipped rather than
#     continuing to be processed under the assumption that the file is likely
#     just not compressed.  Although this may be the case, it is also possible
#     that the file may be corrupted (e.g., file was being added at the time it
#     was being pulled) and lead to undesirable results downstream. This change
#     will allow the file to potentially be uncompressed by gunzip and
#     processed the next time the job runs.
#          - Fixed two minor bugs in the script that had no affect on output.
# 2016-06-10  Y. Ling/D. Keyser   Augmented UNCOMPRESS_UNIX such that when it
#     is YES, it can uncompress files that have been compressed via bzip2.
#     Such files are identified via their file name suffix of either ".bz2" or
#     ".bz".  UNCOMPRESS_UNIX=YES can continue to uncompress files that have
#     been compressed via gzip - these continue to be identified via their file
#     name suffix (8 possibilites noted in the gzip man page). If
#     UNCOMPRESS_UNIX=YES and an input file has a suffix that does not match
#     one of the expected patterns for either gzip or bzip2 compression, then
#     the file is skipped.  This is also the case if the execution of gunzip
#     or bunzip2 fails.
# 2022-01-18  S. Stegall  Replaced $DATA/ before calling utility scripts and instead 
#      used $UTILROOT/ush/ to properly leverage the prod_util module.
#
#
# Usage: ingest_process_orbits.sh
#
#   Script parameters: none
#
#   Modules and files referenced by this script:
#     scripts    : $USHobsproc_satingest/ingest_get.sh
#                  $USHobsproc_satingest/ingest_process_orbits_subscript.sh
#                                                                     (sourced)
#                  $CWORDush
#                  $UTILROOT/ush/postmsg
#     data cards : none
#     executables: $CWORDX (def: $EXECobsproc_shared_bufr_cword/bufr_cword)
#                  (invoked by $CWORDush)
#   Modules and files referenced by sourced script
#   ingest_process_orbits_subscript.sh:
#     scripts      $UTILROOT/ush/date2jday.sh
#                  $USHobsproc_satingest/ingest_translate_orbits.sh
#                  $USHobsproc_satingest/bufr_tranjb.sh
#                  $UTILROOT/ush/postmsg
#     data cards : none
#     executables: none
#
# Remarks: Invoked by the script ingest_process_onetype_neworbits.sh.
#
#
# The following refers to both this script and sourced script
# ingest_process_orbits_subscript.sh:
#
#   Imported Variables that must be passed in:
#      DATA                 - path to current working directory
#      jlogfile             - path to joblog file
#      ORBITLIST            - complete path to the leading qualifier(s) in the
#                             file-processing history file ($ORBITLIST.history)
#                             (this file will be updated to add information on
#                             the processing of any new files on the remote
#                             unix machine found by this run); also complete
#                             path to the leading qualifier(s) in the history
#                             file listing any new files on the remote unix
#                             machine found by this run that have already been
#                             processed by an earlier run and are considered
#                             "repeats" ($ORBITLIST.repeats)
#      DEBUGSCRIPTS         - if imported as "ON" or "YES", will run with
#                             "set -x" on (intended for debugging)
#      EXECUTE              - the name of a program or script used to either
#                             translate file into BUFR, or to append BUFR
#                             messages to the tank file - in the former case,
#                             $EXECUTE is not invoked here but rather child
#                             script ingest_translate_orbits.sh is executed
#                             (it will invoke $EXECUTE), in the latter case
#                             $EXECUTE is always ush script
#                             $USHobsproc_satingest/bufr_tranjb.sh and it will
#                             be invoked here;
#                             an exception occurs when $EXECUTE is imported as
#                             "copy_to_target" in which case a file is just
#                             simply copied from the current working directory
#                             to either:
#                                - the same file name in directory path
#                                  $TANKDIR/$TANKFILE (when $TARGETFILE is
#                                  imported as "same_name")
#                                    -- or --
#                                - the same file name in directory path
#                                  $TANKDIR/<YYYYMMDD>/$TANKFILE (when
#                                  $TARGETFILE is imported as "same_name2")
#                                  (<YYYYMMDD> is obtained from qualifier in
#                                   file name)
#                                    -- or --
#                                - file name $TARGETFILE in directory path
#                                  $TANKDIR/<YYYYMMDD>/$TANKFILE (when
#                                  $TARGETFILE is imported as anything other
#                                  than "same_name" or "same_name2")
#                                (see TANKDIR, TANKFILE and TARGETFILE below)
#      TANKDIR              - root of directory path to output file or BUFR
#                             database tank file (e.g., "/dcom/us007003")
#      TANKFILE             - either:
#                                - sub-directory in path $TANKDIR containing
#                                  output file simply copied from the current
#                                  working directory with the same file name
#                                  (e.g., file is $TANKDIR/$TANKFILE/<filename>)
#                                  (when $TARGETFILE is imported as "same_name")
#                                    -- or --
#                                - sub-directory in path $TANKDIR/<YYYYMMDD>
#                                  containing output file simply copied from the
#                                  current working directory with the same file
#                                  name (e.g., file is
#                                  $TANKDIR/<YYYYMMDD>/$TANKFILE/<filename>)
#                                  (when $TARGETFILE is imported as "same_name2")
#                                    -- or --
#                                - sub-directory in path $TANKDIR/<YYYYMMDD>
#                                  containing output file simply copied from the
#                                  current working directory with file name
#                                  $TARGETFILE
#                                  (e.g., file is
#                                   $TANKDIR/<YYYYMMDD>/$TANKFILE/$TARGETFILE)
#                                  {when $TARGETFILE is imported as a file name
#                                   (anything other than "same_name" or
#                                    "same_name2")}
#                                (invoked only when $EXECUTE is imported as
#                                 "copy_to_target")
#                                (see EXECUTE and TANKDIR above, and TARGETFILE
#                                 below)
#      TARGETFILE           - either:
#                                - imported as "same_name" in which case a file
#                                  is just simply copied from the current
#                                  working directory to the same file name in
#                                  directory path $TANKDIR/$TANKFILE
#                                    -- or --
#                                - imported as "same_name2" in which case a file
#                                  is just simply copied from the current
#                                  working directory to the same file name in
#                                  directory path $TANKDIR/<YYYYMMDD>/$TANKFILE
#                                    -- or --
#                                - the name of the output file when simply
#                                  copied from the current working directory to
#                                  directory path $TANKDIR/<YYYYMMDD>/$TANKFILE
#                                (invoked only when $EXECUTE is imported as
#                                 "copy_to_target")
#                                (see EXECUTE, TANKDIR and TANKFILE above)
#      USHobsproc_satingest - path to obsproc_satingest ush directory
#      utilscript           - path to utility ush script directory containing
#                             date2jday.sh
#      ITRIES_MAX_GET       - the maximum number of failed attempts to transfer
#                             a file from the remote machine before giving up
#      USERDIR              - path to directory containing file-processing
#                             history file (e.g., "$TANKDIR/ingest_hist")
#      MACHINE              - name of remote unix machine to be used in
#                             transfer requests
#      FTYPE                - file type used to determine if some preprocessing
#                             on the file transferred from the remote unix
#                             machine is necessary - possible values are:
#                                bufr     - execute $CWORDSH to unblock file
#                                           and strip off any extraneous
#                                           characters from BUFR messages prior
#                                           to either:
#                                             1) executing any translation
#                                                programs or scripts which
#                                                themselves execute
#                                                $USHobsproc_satingest/bufr_tranjb.sh
#                                                after all other processing is
#                                                complete
#                                                     -- or --
#                                             2) executing
#                                                $USHobsproc_satingest/bufr_tranjb.sh
#                                                without any prior execution of
#                                                translation programs or
#                                                scripts
#                                           Note: In this case $CWORDSH is
#                                                 NEVER executed inside
#                                                 $USHobsproc_satingest/bufr_tranjb.sh.
#                                ncepbufr - execute $CWORDush to unblock file
#                                           and strip off any extraneous
#                                           characters from BUFR messages
#                                           within execution of
#                                           $USHobsproc_satingest/bufr_tranjb.sh
#                                           Note: This should only be set if
#                                                 EXECUTE is exported as
#                                                 bufr_tranjb.sh (i.e., there
#                                                 is no prior execution of a
#                                                 translation program or
#                                                 script).
#                                none     - do nothing
#      UNCOMPRESS_UNIX      - if imported as "YES", files from remote unix
#                             machine are assumed to be unix-compressed and are
#                             uncompressed using either "gunzip" or "bunzip2".
#                             (No dependency on value for FTYPE.)
#                             Note: A file will be uncompressed only if its
#                                   name ends in one of the following strings:
#                                   ".gz", "-gz", ".z", "-z", "_z", ".Z", "-Z"
#                                    or "_Z" for "gunzip"; or ".bz2" or ".bz"
#                                    for "bunzip2".
#                                    In this case, $dsname is updated
#                                    to remove the string from the end of the
#                                    file name (since the actual file name will
#                                    be updated in the same way).
#                                   If a file's name does not end in one of the 
#                                   above strings, it will not be uncompressed.
#                                   This will be noted, and the file will be
#                                   skipped.  It will also be skipped if
#                                   uncompression fails.
#      DELAFTPROC           - if imported as "YES", files from the remote unix
#                             machine are not kept after processing.
#      HISTLENMIN           - minimum length (in lines) of the file keeping
#                             track of the file-processing history
#      HISTLENMAX           - maximum length (in lines) of the file keeping
#                             track of the file-processing history
#
#   Imported Variables that can be passed in:
#      CWORDush             - path to ush script bufr_cword.sh
#                             (default =
#                             '$USHobsproc_shared_bufr_cword/bufr_cword.sh')
#      USHobsproc_shared_bufr_cword
#                           - path to obsproc_shared ush directory containing
#                             bufr_cword.sh
#                             (invoked only if $CWORDush is not imported)
#
# Condition codes:
#     0 - no problem encountered
#   > 0 - some problem encountered
#     Specifically: 111 - All files to be processed in a particular group
#                         (family) were unprocessable
#                   199 - One or more files were untransferable in last 5 (or
#                         possibly some other number) runs of this job
#                   222 - All files to be processed were already processed
#                   230 - No files submitted for processing
#
# Attributes:
#   Language: ksh script
#   Machine:  NCEP WCOSS
#
####

set -au

echo
echo "#######################################################################"
echo "                   START INGEST_PROCESS_ORBITS                         "
echo "#######################################################################"
echo

if [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ] ; then
   set -x
fi

host=$(hostname -s)

CWORDush=${CWORDush:-$USHobsproc_shared_bufr_cword/bufr_cword.sh}

orbitcount=0
procorbcount=0
repeatcount=0
noproccount=0
noxfercount=0
unproccount=0
unxfercount=0
toterr_mult_files=99999

read line
rc=$?
if [ $rc -ne 0 ] ; then
   msg="Exiting with rc = 230 - no files submitted for processing --> non-fatal"
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
   set +x
   echo
   echo $msg
   echo
   exit 230
fi

typeset -Z2 mm dd

> $DATA/dsname_cat
> $DATA/orbitlist_fname_cat
> $DATA/orbitlist_path_cat

while [ $rc -eq 0 ] ; do
   set -A FILEINFO $line
   iword=0
   dsnamelist=""
   toterr=0
   yy=-99
   ddd=999
   yyyy=9999
   mm=99
   dd=99
   while [ $iword -lt ${#FILEINFO[*]} ] ; do
      neworbit=${FILEINFO[$iword]}
      dsname_full=$neworbit
      dsname=$(basename $neworbit)
      iword=$(($iword+1))
      orbitnotp_max=5
      orbitcount=$(($orbitcount+1))
      if [ -s $ORBITLIST.history ] ; then
         orbitdone=$(grep -c "$neworbit PROCESSED" $ORBITLIST.history)
         orbitnotp=$(grep -c "COULD NOT PROCESS $neworbit" $ORBITLIST.history)
      else
         orbitdone=0
         orbitnotp=0
      fi
      if [ $orbitdone -eq 0 -a $orbitnotp -lt $orbitnotp_max ] ; then

         set +x
         echo
         echo "Time is now $(date -u)."
         echo
         [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x

         transerror=99
         itries=1
         while [ $transerror -gt 0 -a $itries -le $ITRIES_MAX_GET ]; do
            if [ $itries -gt 1 ]; then
               msg="TRANSFER OF $dsname_full FAILED!!!! - SLEEP 30 SEC AND \
TRY AGAIN."
               $UTILROOT/ush/postmsg "$jlogfile" "$msg"
               sleep 30
            fi
            ksh $USHobsproc_satingest/ingest_get.sh $MACHINE $DATA/$dsname \
             "$neworbit" 2>&1
            transerror=$?
            itries=`expr $itries + 1`
         done
         itries=`expr $itries - 1`

         set +x
         echo
         echo "Time is now $(date -u)."
         echo
         [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x

         if [ $transerror -eq 0 ] ; then
         echo "$dsname_full RECEIVED AT $(date -u +%Y/%m/%d' '%H:%M:%S' UTC')" \
          >> $ORBITLIST.history
            msg="TRANSFER OF $dsname_full successful on try no. ${itries}."
            $UTILROOT/ush/postmsg "$jlogfile" "$msg"
            fdcperr=0
            if [ $UNCOMPRESS_UNIX = YES ] ; then
               dsnuerr=1
               uncerror=1
               nchar=`echo ${#dsname}`
               nchar_m1=`expr $nchar - 1`
               nchar_m2=`expr $nchar - 2`
               nchar_m3=`expr $nchar - 3`
               for char_del in . - _; do
                  if [ `echo $dsname | cut -c$nchar_m1-$nchar` = \
                   ${char_del}Z -o `echo $dsname | cut -c$nchar_m1-$nchar` \
                   = ${char_del}z ]; then
                     eval dsname_t=`echo $dsname | cut -c1-\`expr $nchar - 2\``
                     dsnuerr=0
                     break
                  fi
                  if [ `echo $dsname | cut -c$nchar_m2-$nchar` = \
                   ${char_del}gz ]; then
                     if [ ${char_del}gz != _gz ]; then
                        eval dsname_t=`echo $dsname | cut -c1-\`expr $nchar - 3\``
                        dsnuerr=0
                        break
                     fi
                  fi
               done
               if [ $dsnuerr -eq 0 ]; then
                  gunzip $DATA/$dsname
                  uncerror=$?
                  if [ $uncerror -eq 0 ] ; then
                     msg="UNIX UNCOMPRESS (gunzip) OF $dsname_full successful."
                     $UTILROOT/ush/postmsg "$jlogfile" "$msg"
                     dsname=$dsname_t
                  fi
               else
                  if [ `echo $dsname | cut -c$nchar_m2-$nchar` = .bz ]; then
                     eval dsname_t=`echo $dsname | cut -c1-\`expr $nchar - 3\``
                     dsnuerr=0
                  elif [ `echo $dsname | cut -c$nchar_m3-$nchar` = .bz2 ]; then
                     eval dsname_t=`echo $dsname | cut -c1-\`expr $nchar - 4\``
                     dsnuerr=0
                  fi
                  if [ $dsnuerr -eq 0 ]; then
                     bunzip2 $DATA/$dsname
                     uncerror=$?
                     if [ $uncerror -eq 0 ] ; then
                        msg="UNIX UNCOMPRESS (bunzip2) OF $dsname_full \
successful."
                        $UTILROOT/ush/postmsg "$jlogfile" "$msg"
                        dsname=$dsname_t
                     fi
                  fi
               fi
               if [ $uncerror -ne 0 ] ; then
                  msg="UNIX UNCOMPRESS (gunzip or bunzip2) OF $dsname_full \
UNSUCCESSFUL, original file may just not be compressed but it may also be \
corrupt so skip processing of this file --> non-fatal"
                  $UTILROOT/ush/postmsg "$jlogfile" "$msg"
                  noproccount=$(($noproccount+1))
                  echo "COULD NOT PROCESS $neworbit AT \
$(date -u +%Y/%m/%d' '%H:%M:%S' UTC')" >> $ORBITLIST.history
                  if [ $DELAFTPROC = YES ] ; then
                     rm $DATA/$dsname
                  fi
                  set +x
                  echo
                  echo $msg
                  echo
                  [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
               fi
               toterr=$(($toterr+$uncerror))
            fi
            if [ $FTYPE = bufr ] ; then
               DX_SKIP=YES # tell $CWORDush to not copy any dictionary msgs
#	       if [ $cword = 'yes' ] ; then 
                 $CWORDush unblk $DATA/$dsname $DATA/tmporbit |tee \
                  $DATA/bufr_cword.out.$host.$$
#              else
#	         echo "cword is set to $cword "
#              fi
               fdcperr=$?
               if [ $fdcperr -eq 0 -a -s $DATA/tmporbit ] ; then
                  mv $DATA/tmporbit $DATA/$dsname
                  msg=`grep "***WARNING" $DATA/bufr_cword.out.$host.$$`
                  err_grep=$?
                  msg=`echo $msg, input file: $dsname_full.`
                  if [ $err_grep -eq 0 ]; then
                     $UTILROOT/ush/postmsg "$jlogfile" "$msg"
                  fi
                  msg="BUFR_CWORD processing successful for $dsname_full."
                  $UTILROOT/ush/postmsg "$jlogfile" "$msg"
               else
                  msg="BUFR_CWORD processing UNSUCCESSFUL or INCOMPLETE for \
$dsname_full, skip processing of this file --> non-fatal"
                  $UTILROOT/ush/postmsg "$jlogfile" "$msg"
                  noproccount=$(($noproccount+1))
                  echo "COULD NOT PROCESS $neworbit AT \
$(date -u +%Y/%m/%d' '%H:%M:%S' UTC')" >> $ORBITLIST.history
                  if [ $DELAFTPROC = YES ] ; then
                     rm $DATA/$dsname
                  fi
                  set +x
                  echo
                  echo $msg
                  echo
                  [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
               fi
               rm $DATA/bufr_cword.out.$host.$$
            fi 
            toterr=$(($toterr+$fdcperr))
         else
            msg="TRANSFER OF $dsname_full FAILED AFTER $itries TRIES!!!!"
            $UTILROOT/ush/postmsg "$jlogfile" "$msg"
            noxfercount=$(($noxfercount+1))
            echo "COULD NOT TRANSFER $dsname_full AT \
$(date -u +%Y/%m/%d' '%H:%M:%S' UTC')" >> $ORBITLIST.history
            orbitnotr=$(grep "$dsname_full" $ORBITLIST.history | \
             tail -n$orbitnotp_max | grep -c "COULD NOT TRANSFER $dsname_full")
            if [ $orbitnotr -ge $orbitnotp_max ] ; then
               unxfercount=$(($unxfercount+1))
               unproccount=$(($unproccount+1))
               echo "$dsname_full DECLARED UNPROCESSABLE AT \
$(date -u +%Y/%m/%d' '%H:%M:%S' UTC')" >> $ORBITLIST.history
               print -u9 $dsname_full
            fi
         fi
         toterr=$(($toterr+$transerror))
         dsnamelist="$dsnamelist $dsname"
      elif [ $orbitdone -gt 0 ] ; then
         repeatcount=$(($repeatcount+1))
         echo "$dsname_full REPEATED AT $(date -u +%Y/%m/%d' '%H:%M:%S' UTC')" \
          >> $ORBITLIST.repeats
         print -u9 $dsname_full
         toterr=$(($toterr+1))
      elif [ $orbitnotp -ge $orbitnotp_max ] ; then
         unproccount=$(($unproccount+1))
         echo "$dsname_full DECLARED UNPROCESSABLE AT \
$(date -u +%Y/%m/%d' '%H:%M:%S' UTC')" >> $ORBITLIST.history
         print -u9 $dsname_full
         toterr=$(($toterr+1))
      fi
      if [ $toterr -gt 0 ] ; then
         break
      fi
   done

   if [ $PROC_MULT_FILES != YES ]; then
      [ $toterr -eq 0 ] && \
       . $USHobsproc_satingest/ingest_process_orbits_subscript.sh
   else
      if [ $toterr -eq 0 ]; then
         echo $FILEINFO >> $DATA/orbitlist_path_cat
         echo $dsname >> $DATA/orbitlist_fname_cat
         cat $DATA/$dsname >> $DATA/dsname_cat
         toterr_mult_files=0
      fi
   fi
   read line
   rc=$?

done

if [ $PROC_MULT_FILES = YES ]; then
   dsname=dsname_cat
   dsnamelist=dsname_cat
   toterr=$toterr_mult_files
   [ $toterr -eq 0 ] && \
    . $USHobsproc_satingest/ingest_process_orbits_subscript.sh
fi

if [ -s $ORBITLIST.history ] ; then
   histlen=$(wc -l < $ORBITLIST.history)
   if [ $histlen -gt $HISTLENMAX ] ; then 
      histcut=$(($histlen-$HISTLENMIN))
      awk ' BEGIN   { nlines = 0 }
                    { 
                      nlines = nlines + 1;
                      if(nlines>'$histcut') print $0
                    } 
          ' ${ORBITLIST}.history > ${ORBITLIST}.newhist.$host.$$
      mv ${ORBITLIST}.newhist.$host.$$ ${ORBITLIST}.history
   fi
fi
if [ -s ${ORBITLIST}_copy.history ] ; then
   copy_histlen=$(wc -l < ${ORBITLIST}_copy.history)
   if [ $copy_histlen -gt $HISTLENMAX ] ; then
      copy_histcut=$(($copy_histlen-$HISTLENMIN))
      awk ' BEGIN   { nlines = 0 }
                    {
                      nlines = nlines + 1;
                      if(nlines>'$copy_histcut') print $0
                    }
          ' ${ORBITLIST}_copy.history > ${ORBITLIST}_copy.newhist.$host.$$
      mv ${ORBITLIST}_copy.newhist.$host.$$ ${ORBITLIST}_copy.history
   fi
fi
if [ -s ${ORBITLIST}.repeats ] ; then
   reptlen=$(wc -l < ${ORBITLIST}.repeats)
   if [ $reptlen -gt $HISTLENMAX ] ; then 
      reptcut=$(($reptlen-$HISTLENMIN))
      awk ' BEGIN   { nlines = 0 }
                    { 
                      nlines = nlines + 1;
                      if(nlines>'$reptcut') print $0 
                    } 
          ' ${ORBITLIST}.repeats > ${ORBITLIST}.newrept.$host.$$
      mv ${ORBITLIST}.newrept.$host.$$ ${ORBITLIST}.repeats
   fi
fi
if [ -s $USERDIR/$TARGETFILE.history ] ; then
   target_histlen=$(wc -l < $USERDIR/$TARGETFILE.history)
   if [ $target_histlen -gt $HISTLENMAX ] ; then
      target_histcut=$(($target_histlen-$HISTLENMIN))
      awk ' BEGIN   { nlines = 0 }
                    {
                      nlines = nlines + 1;
                      if(nlines>'$target_histcut') print $0
                    }
          ' $USERDIR/$TARGETFILE.history > $USERDIR/$TARGETFILE.newhist.$host.$$
      mv $USERDIR/$TARGETFILE.newhist.$host.$$ $USERDIR/$TARGETFILE.history
   fi
fi

set +x
echo
echo "Exiting - summary of processing : "
echo
echo "$orbitcount files were submitted for processing."
echo
if [ $procorbcount -gt 0 ] ; then
   echo "$procorbcount files were successfully processed."
fi
if [ $noproccount -gt 0 ] ; then
   msg="$noproccount files were unsuccessfully processed --> non-fatal"
   echo $msg
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
fi
if [ $noxfercount -gt 0 ] ; then
   msg="$noxfercount files were unsuccessfully transferred --> non-fatal"
   echo $msg
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
fi
if [ $unproccount -gt 0 ] ; then
   msg="$unproccount files were declared unprocessable --> non-fatal"
   echo $msg
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
fi
if [ $repeatcount -gt 0 ] ; then
   msg="$repeatcount files had been previously processed --> non-fatal"
   echo $msg
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
fi
echo
[ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x

if [ $unxfercount -gt 0 ] ; then
   msg="Exiting with rc = 199 - $unxfercount files were untransferable in last \
$orbitnotp_max runs of this job --> non-fatal"
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
   set +x
   echo
   echo $msg
   echo
   exit 199
fi
if [ $orbitcount -gt 1 ] ; then
   if [ $orbitcount -eq $repeatcount ] ; then
      msg="Exiting with rc = 222 - all files to be processed were already \
processed --> non-fatal"
      $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      set +x
      echo
      echo $msg
      echo
      exit 222
   fi
   if [ $orbitcount -eq $noproccount ] ; then
      if [ -n "$igroup" ]; then
         msg="Exiting with rc = 111 - all group $igroup files to be processed \
were unprocessable --> non-fatal" 
      else
         msg="Exiting with rc = 111 - all files to be processed were \
unprocessable --> non-fatal" 
      fi
      $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      set +x
      echo
      echo $msg
      echo
      exit 111
   fi
fi
exit 0
