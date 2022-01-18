#!/bin/ksh
# Run under ksh

####  UNIX Script Documentation Block
#
# Script name:   existday.sh.ecf
#
# RFC contact:  Keyser      org: NP22        date: 2017-12-27
#
# Abstract: Entry point for a series of scripts that will handle the transfer
#   of statically-named files from a remote unix machine.  The contents of
#   these files are assumed to be updated at most once per day.
#
# Script history log:
# 1996-10-02  Bert Katz   Original version for implementation
#        (existdyc.sh.sms).
# 1996-12-04  Bert Katz   Reorganized file structure to resemble the BUFR
#        tanking system.
# 1997-01-10  Bert Katz   Sent unified output file to "stdout".  Added check
#        for lapsed processing.  Added a debug option.
# 1997-11-03  Bert Katz   Emergency fix to allow all file streams to complete
#        processing after a failure in one file stream.
# 1997-11-13  Bert Katz   Changed handling of lapses in processing so that such
#        errors are recorded in jlogfile, but are not displayed as fatal
#        errors.
# 1997-12-17  Bert Katz   Changed handling of file-transfer failures so that
#        such errors are recorded in jlogfile, but are not displayed as fatal
#        errors.
# 1998-05-12  Bert Katz   Changed use of check_lapsed_days to
#        check_lapsed_data, which is y2k compliant.
# 2005-11-02  Bert Katz   Modified to allow for different tasks to set
#        different times-of-creation for the NESDIS files being acquired
# 2006-06-14  P. OReilly   Modified the script to add variable "CRITICAL" which
#        is used to set flags for Big Brother monitoring in script
#        ingest_check_lapsed_data.
# 2006-08-12  D. Keyser   Renamed from existdyc.sh.sms.  Generalized name of
#        remote machine (no longer hardwired to CEMSCS) and changed to account
#        for all remote machines now being unix (since MVS CEMSCS machine was
#        replaced with unix DDS machine).  Improved docblock and comments.
#        Accepts imported value for path to ingest_qmgr.sh, $utilscript,
#        defaults to /nwprod/util/ush.  If execution of ingest_qmgr.sh returns
#        status code of 99, now posts a message to the joblog file that another
#        job with this same name is in the system and this ingest job will
#        continue but not ingest any satellite data (before no message was
#        posted).
# 2008-01-24  D. Keyser   Added new script variable MTYPSBT.
# 2012-10-26  D. Keyser   Modified the script to add new value TRANSFER_COMMAND
#        which determines the type of transfer to be done (e.g., 'ftp', 'sftp',
#        'lftp', 'wget').  Different file streams can be processed using
#        different values of TRANSFER_COMMAND (if not found for a particular
#        file stream, the global value is used).  WCOSS/Eddy currently does not
#        support ftp, but it does support wget (later WCOSS/Tide does support
#        ftp for some queues).  Modified to run on WCOSS.  Specifically, since
#        WCOSS default script (sh) is bash, all child scripts are executed
#        under ksh (the CCS default) rather than under sh (some script commands
#        tranferred from CCS version do not work under bash).  This script is
#        now set to run under ksh shell as the default. Renamed to
#        existday.sh.ecf since now scheduled by ecFlow.
# 2014-01-22  Diane Stokes/D. Keyser   Removed all references to script
#        variables $CNVRT2F77 and $ATTRIBUTES since these are no longer used
#        now that cnvblk is obsolete.  $USHobsproc_satingest replaces $USHbufr
#        as the environment variable representing the directory path to the ush
#        scripts.  $EXECobsproc_satingest replaces $EXECbufr as the environment
#        variable representing the directory path to the executables.  Create
#        output directories $USERDIR, $TANKDIR and $OUTDIR if they do not
#        already exist.  Now points to directory path $USHobsproc_satingest
#        rather than directory path $utilscript to execute ingest_qmgr.sh
#        (moved out of utililty ush location).  Added suffix ".sh" to ush
#        scripts ingest_check_lapsed_data and ingest_process_onetype_newdays
#        both of which this script executes.  Aborts with condition code 98 if
#        environment variable "timetype" imported with value "LOCAL" - this is
#        not supported on WCOSS where the local time is hardwired to the UTC
#        time.  The default for $timetype default is now "UTC" rather than
#        "GMT".  Added new comments.  Updated information in docblock and some
#        existing comments.
# 2014-09-04  D. Keyser   Modified the script so that different file streams
#        can be processed using different values of wget_network_timeout
#        (connection or read timeout in seconds).  If not found for a
#        particular file stream, the global value is used.
# 2016-01-08  D. Keyser   Corrected definition of variable ndayarch in
#        in the remarks section of the docblock.
# 2017-12-27  D. Keyser   Modified to hardwire environment variable
#        $REMOTEDIRGRP to ".", its default value in child script ingest_get.sh.
#        This variable is only invoked when ingest_get.sh is executed downstream
#        from model script existore.sh.ecf. It is set here to avoid an "unset
#        variable" error in ingest_get.sh when that script is executed
#        downstream from this model script (existday.sh.ecf).
# 2022-01-18  S. Stegall  Replaced $DATA/ before calling utility scripts and instead 
         used $UTILROOT/ush/ to properly leverage the prod_util module.
#
# Usage: existday.sh.ecf
#
#   Script parameters: None
#
#   Modules and files referenced:
#     scripts    : $USHobsproc_satingest/ingest_qmgr.sh
#                  $USHobsproc_satingest/ingest_process_onetype_newdays.sh
#                  $USHobsproc_satingest/ingest_check_lapsed_data.sh
#                  $UTILROOT/ush/prep_step
#                  $UTILROOT/ush/postmsg
#     data cards : None
#     executables: None
#
# Remarks:
#
#   Condition codes:
#       0 - no problem encountered
#     > 0 - some problem encountered
#       Specifically:  98 - Environment variable "timetype" imported with
#                           value "LOCAL" - this is not supported on WCOSS
#                           where the local time is hardwired to the UTC time
#                      99 - Another job with this name is in the system, let
#                           that job run in lieu of this job
#       In addition, this script will complete with condition code ZERO but
#        will stamp out an ABNORMAL COMPLETION diagnostic if any one of the
#        files returns a status code from child script
#        ingest_process_onetype_newdays.sh that is > 0 - these include:
#                             -- currently none !!
#
#
#   The following script variables are user controllable.  They should be
#   specified by the user in the script that invokes
#   $USHobsproc_satingest/ingest_process_onetype_newdays.sh (except where there 
#   are default values noted).
#
#     USHobsproc_satingest : path to obsproc_satingest ush directory.
#     USERDIR : path to directory containing housekeeping and file-processing
#       history files.
#     DATA : path to directory where files from the remote unix machine are to
#       be put.
#     TANKDIR : path to directory where files are put for operational use.
#     OUTDIR : path to directory where output listing is to be put.
#     EXECobsproc_satingest : path to obsproc_satingest executable directory.
#     HISTLENMIN, HISTLENMAX : minimum and maximum length of the file keeping
#       track of the file-processing history. Each line of the file corresponds
#       to one file successfully processed.
#     daysavail : the schedule for the availablity of the file.
#       Default is "Sun Mon Tue Wed Thu Fri Sat".
#       The following options are available :
#       (1) Some subset of "Sun Mon Tue Wed Thu Fri Sat"
#       (2) "DAY OF MONTH = n"
#       (3) "DAY OF MONTH = n MODULO k"
#       (4) "DAY OF YEAR = n"
#       (5) "DAY OF YEAR = n MODULO k"
#       One may also specify a different schedule for availability for one or
#       more files as daysavail1, daysavail2,. . . (see below). Defaults to
#       "Sun Mon Tue Wed Thu Fri Sat".
#       Note: If set to "justcopy" or "justcopyforward" the file is NEVER
#             transferred from the remote unix machine, it is ALWAYS simply
#             copied from the previous day.
#     dayafter : If the file will always be available on the calendar day after
#       the date with which it should be date-stamped, then "YES" should be
#       specified.  One may also specify a different value for this for one or
#       more files as dayafter1, dayafter2,. . . (see below). Defaults to "NO".
#     HOURS2ALARM : in case file transfers or processing come to a halt, this
#       is the number of hours that will pass before a return code will be
#       issued to trigger an alarm mechanism.  One may also specify a different
#       number of hours for one or more files as HOURS2ALARM1, HOURS2ALARM2,
#       . . . (see below).  Defaults to "30".
#     CRITICAL : if data type is critical to NCEP production, variable is set
#       to "YES" and data will flag red in Big Brother when there is a lapse in
#       the data.  If data type is not critical, variable is set to default
#       value of "NO". One may specify different values for one or more files
#       such as CRITICAL1, CRITICAL2, . . . (see below).  As said, defaults to
#       "NO".
#     MACHINE : name of remote unix machine to be used in transfer requests.
#       Defaults to "none".
#     TRANSFER_COMMAND : type of transfer to be done (e.g., 'ftp', 'sftp',
#       'lftp', 'wget').  One may also specify a different transfer command for
#       one or more files as TRANSFER_COMMAND1, TRANSFER_COMMAND2,. . . (see
#       below).  Defaults to "ftp".
#            Note: Currently should use 'sftp' only for connections to same
#                  WCOSS machine as that in which this script is running. Note:
#                  'sftp' will not work for pulling files from other WCOSS
#                  machine.
#     DEBUGSCRIPTS : if set to "ON" or "YES", all scripts will run with
#       "set -x" on. Intended for debugging.  Defaults to "OFF".
#     ndayarch : maximum number of days that input files previously transferred
#       from the remote unix machine will be kept in current date directory
#       $TANKDIR/<yyyymmdd_current>/$TANKSUBDIR in the event no more recent
#       files become available for transferring.  Defaults to "10".
#     COPYFORWARD : if set to "YES", the first request for a file on a given
#       day will result in the most recent available file being "copied
#       forward" from the older directory to the current day's directory.  If
#       the file being remotely provided changes later that day, the new copy
#       will be retrieved and overwrite #the file that was "copied forward".
#       Defaults to "YES".
#     timemade : the 24-hour clock time when the files to be requested from the
#       remote unix machine will be available.  One may also specify a
#       different value for timemade for one or more files as timemade1,
#       timemade2,. . . (see below).  Defaults to "0000".
#     timetype : if set to "LOCAL", all times for processing will be assumed to
#       be in local time.  If set to "UTC", all times for processing will be
#       assumed to be in UTC.  If set to "USER_SPEC", the calling program is
#       specifying a "hardwired" time for processing using the script variable
#       user_spec_timedatecurr (see next below).  One may also specify a
#       different value for timetype for one or more files as timetype1,
#       timetype2,. . .  (see below).  Defaults to "UTC".
#       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#       ===> WARNING: ON WCOSS THE "LOCAL" TIME IS THE "UTC" TIME -
#                     SCRIPT MODIFIED TO ABORT WITH THIS INFO IF
#                     timetype IS IMPORTED AS "LOCAL" !!!!!
#       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#     user_spec_timedatecurr : hardwired time which will be used as the time
#       in all processing.  It is specified in the form "yyyymmdd doy dow HHMM",
#       where "yyyy" is year, "mm" is month, "dd" is day of month, "doy" is day
#       of year, "dow" is day of week (1=Monday, ..., 7=Sunday), "HH" is hour
#       and "MM" is minute.  This is only invoked if timetype is set to
#       "USER_SPEC" (see first above).  One may also specify a different value
#       for user_spec_timedatecurr for one or more files as
#       user_spec_timedatecurr1, user_spec_timedatecurr2,. . .  (see below).
#       Defaults to "`date -u '+%Y%m%d %j %w %H%M'" (current UTC time).
#     MTYPSBT : if set to "YES", the program
#       $EXECobsproc_satingest/bufr_tranmtypsbt will be executed by the child
#       script ingest_process_days.sh in order to encode the external BUFR
#       table into the beginning of the statically-named file and to change the
#       BUFR type and subtype internally in each non- dictionary BUFR message.
#       One may also specify a different value for MTYPSBT for one or more
#       files as MTYPSBT1, MTYPSBT2,. . .  (see below).
#       Defaults to "NO"
#     wget_network_timeout: connection or read timeout in seconds (applies only
#       when $TRANSFER_COMMAND or $TRANSFER_COMMANDn is "wget").  Applies to
#       ingest_get.sh.  One may also specify a different connection or read
#       timeout value for one or more files as wget_network_timeout1,
#       wget_network_timeout2,. . . (see below).  Defaults to "120".
#     wget_tries: number of times to try to connect (no retries for fatal
#       errors) (applies only when $TRANSFER_COMMAND or $TRANSFER_COMMANDn is
#      "wget").
#       Defaults to "2" in ingest_get.sh.
#     lftp_dns_timeout: time limit for DSN queries in seconds (applies only
#       when $TRANSFER_COMMAND or $TRANSFER_COMMANDn is "lftp").
#       Defaults to "120" in ingest_get.sh.
#     lftp_recon_int_base: base minimal time between reconnects in seconds
#       (applies only when $TRANSFER_COMMAND or $TRANSFER_COMMANDn is "lftp").
#       Defaults to "120" in ingest_get.sh.
#     lftp_max_tries: number of times to try to connect (applies only when
#       $TRANSFER_COMMAND or $TRANSFER_COMMANDn is "lftp").
#       Defaults to "2" in ingest_get.sh.
#     lftp_recon_int_mult: multiplier by which lftp_recon_int_base (see above)
#       is multiplied each time a new attempt to reconnect occurs (applies only
#       when $TRANSFER_COMMAND or $TRANSFER_COMMANDn is "lftp").
#       Defaults to "1" in ingest_get.sh.
#
#
#
#       The following environmental variables allow multiple files on the
#       remote unix machine to be processed with one invocation of
#       $USHobsproc_satingest/ingest_process_onetype_newdays.sh :
#     
#     nfiles : The number of files from the remote unix machine that are being
#       processed.
#
#       In the definitions that follow, the suffixed "n" or "N" means that a
#       list of variables such as : daysavail1, daysavail2, . . . ,
#       daysavail$nfiles should be defined for use by the script
#       $USHobsproc_satingest/ingest_process_onetype_newdays.sh (some values
#       below do have default values).
#
#     daysavailN : the schedule for the availablity of the file.
#       Default is "Sun Mon Tue Wed Thu Fri Sat".
#       The following options are available :
#       (1) Some subset of "Sun Mon Tue Wed Thu Fri Sat"
#       (2) "DAY OF MONTH = n"
#       (3) "DAY OF MONTH = n MODULO k"
#       (4) "DAY OF YEAR = n"
#       (5) "DAY OF YEAR = n MODULO k"
#       For a particular file "N", if $daysavailN does not exist, then the
#       global default value "$daysavail" is used (see above).
#       Note: If set to "justcopy" or "justcopyforward" the file is NEVER
#             transferred from the remote unix machine, it is ALWAYS simply
#             copied from the previous day.
#     dayafterN : If the file will always be available on the calendar day
#       after the date with which it should be date-stamped, then "YES" should
#       be specified.  For a particular file "N", if $dayafterN does not exist,
#       then the global default value "$dayafter" is used (see above).
#     TRANSFER_COMMANDn : type of transfer to be done (e.g., 'ftp', 'sftp',
#       'lftp', 'wget').  For a particular file "n", if $TRANSFER_COMMANDn
#       does not exist, then the global default value "TRANSFER_COMMAND" is
#       used (see above).
#     PROCSCRIPTn : the name of the script to process the file.  If PROCSCRIPTn
#       is not set, no further processing is required.
#     dsnameN : The name of the file to be transferred from the remote unix
#       machine.
#     dsname_histN : The leading qualifier(s) in the file name in $USERDIR
#       containing file-processing history (file path is thus
#       $USERDIR/$dsname_hist.history).  Default is $dsnameN (see above).
#     TANKSUBDIRn : sub-directory holding file TANKFILEn (for a particular file
#       "n").
#     TANKFILEn : the name of the file when copied to sub-directory TANKSUBDIRn
#       (for a particular file "n").  Default is $dsnameN (see above).
#     HOURS2ALARMn : in case file transfers or processing come to a halt, this
#       is the number of hours that will pass before a return code will be
#       issued to trigger an alarm mechanism.  For a particular file "n", if
#       $HOURS2ALARMn does not exist, then the global default value
#       "$HOURS2ALARM" is used (see above).
#     CRITICALn : if data type is critical to NCEP production, variable is set
#       to "YES" and data will flag red in Big Brother when there is a lapse in
#       the data.  If data type is not critical, variable is set to default
#       value of "NO". For a particular file "n", if $CRITICALn does not exist,
#       then the global default value "$CRITICAL" is used (see above).
#     timemadeN : the 24-hour clock time when the files to be requested from
#       the remote unix machine will be available.  For a particular file "N",
#       if $timemadeN does not exist, then the global default value "$timemade"
#       is used (see above).
#     timetypeN : if set to "LOCAL", all times for processing will be assumed
#       to be in local time.  If set to "UTC", all times for processing will be
#       assumed to be in UTC.  If set to "USER_SPEC", the calling program is
#       specifying a "hardwired" time for processing using the script variable
#       user_spec_timedatecurrN (for a particular file "N", see next below).
#       For a particular file "N", if $timetypeN does not exist, then the
#       global default value "$timetype" is used (see above).
#       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#       ===> WARNING: ON WCOSS THE "LOCAL" TIME IS THE "UTC" TIME -
#                     SCRIPT MODIFIED TO ABORT WITH THIS INFO IF
#                     timetype IS IMPORTED AS "LOCAL" !!!!!
#       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#     user_spec_timedatecurrN : hardwired time which will be used as the time
#       in all processing.  It is specified in the form "yyyymmdd doy dow HHMM",
#       where "yyyy" is year, "mm" is month, "dd" is day of month, "doy" is day
#       of year, "dow" is day of week (1=Monday, ..., 7=Sunday), "HH" is hour
#       and "MM" is minute.  This is only invoked if timetypeN is set to
#       "USER_SPEC" (for a particular file "N", see first above).  For a
#       particular file "N", if $user_spec_timedatecurrN does not exist, then
#       the global default value "$user_spec_timedatecurr" is used (see above).
#     MTYPSBTn : if set to "YES", the program
#       $EXECobsproc_satingest/bufr_tranmtypsbt will be executed by the child
#       script ingest_process_days.sh in order to encode the external BUFR
#       table into the beginning of the statically-named file and to change the
#       BUFR type and subtype internally in each non- dictionary BUFR message
#       (for a particular file "n").  For a particular file "n", if $MTYPSBTn
#       does not exist, then the global default value "$MTYPSBT" is used (see
#       above).
#     wget_network_timeoutN: connection or read timeout in seconds (applies
#       only when $TRANSFER_COMMAND or $TRANSFER_COMMANDn is "wget").  Applies
#       to ingest_get.sh.  For a particular file "N", if wget_network_timeoutN
#       does not exist, then the global default value $wget_network_timeout
#       used (see above).
#
#     It should be noted that, for each file, the environmental variables with
#       a suffixed "n" or "N" (i.e., daysavail1, dayafter1, TRANSFER_COMMAND1,
#       PROCSCRIPT1, dsname1, dsname_hist1, TANKSUBDIR1, TANKFILE1,
#       HOURS2ALARM1, CRITICAL1, timemade1, timetype1, user_spec_timedatecurr1,
#       MTYPSBT1, wget_network_timeout1) are assigned to variables without the
#       "n" or "N" (i.e., daysavail, dayafter, TRANSFER_COMMAND, PROCSCRIPT,
#       dsname, dsname_hist, TANKSUBDIR, TANKFILE, HOURS2ALARM, CRITICAL,
#       timemade, timetype, user_spec_timedatecurr, MTYPSBT,
#       wget_network_timeout) and exported for use by the script
#       $USHobsproc_satingest/ingest_process_onetype_newdays.sh.
#
# Attributes:
#   Language: ksh script
#   Machine:  NCEP WCOSS
#
####

#####################################################################
#
# START FLOW OF CONTROL
#
#####################################################################

########################################
set -aux
msg="$DATATYPE PROCESSING FROM STATICALLY-NAMED FILES HAS BEGUN"
$UTILROOT/ush/postmsg "$jlogfile" "$msg"

########################################

ksh $USHobsproc_satingest/ingest_qmgr.sh
errsc=$?



if [ $errsc -eq 99 ]; then
   msg="Another job with this name is in the system, this ingest job will \
continue but not ingest any satellite data"
   echo $msg
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
   exit $errsc
fi


pwd
ls -ltr


 
##########################################

set +x
echo " "
echo "############################################################"
echo "         INGEST DATA FROM REMOTE UNIX MACHINE               "
echo "               (STATICALLY-NAMED FILES)                     "
echo "############################################################"
echo " "
set -x

set +u
[ -n "$USERDIR" -a ! -d "$USERDIR" ] && mkdir -p $USERDIR
[ -n "$TANKDIR" -a ! -d "$TANKDIR" ] && mkdir -p $TANKDIR
[ -n "$OUTDIR"  -a ! -d "$OUTDIR"  ] && mkdir -p $OUTDIR
set -u

pgm='ingest_process_onetype_newdays.sh'
set +u
. $UTILROOT/ush/prep_step
set -u

MACHINE=${MACHINE:-none}
ndayarch=${ndayarch:-10}
DEBUGSCRIPTS=${DEBUGSCRIPTS:-OFF}
COPYFORWARD=${COPYFORWARD:-YES}
REMOTEDIRGRP="." # hardwired to avoid "unset variable" error in ingest_get.sh
                 #  (not invoked from this model script)

daysavail_save=${daysavail:-"Sun Mon Tue Wed Thu Fri Sat"}
dayafter_save=${dayafter:-NO}
TRANSFER_COMMAND_save=${TRANSFER_COMMAND:-ftp}
HOURS2ALARM_save=${HOURS2ALARM:-30}
CRITICAL_save=${CRITICAL:-NO}
timemade_save=${timemade:-0000}
timetype_save=${timetype:-UTC}
user_spec_timedatecurr_save=\
${user_spec_timedatecurr:-`date -u '+%Y%m%d %j %w %H%M'`}
MTYPSBT_save=${MTYPSBT:-NO}
wget_network_timeout_save=${wget_network_timeout:-120}

ifiles=0
errsave=0
lapseret=0

#  Process each data file
#  ----------------------

while [ $ifiles -lt $nfiles ] ; do

  ifiles=$(($ifiles+1)) 

  msg="$pgm files $ifiles has begun."
  $UTILROOT/ush/postmsg "$jlogfile" "$msg"

  set +x
  echo
  echo "####################################################################"
  echo "####################################################################"
  echo "                PROCESSING FOR FILE $ifiles HAS BEGUN               "
  echo "####################################################################"
  echo "####################################################################"
  echo
  set -x

  eval daysavail=\${daysavail$ifiles:-$daysavail_save}
  eval dayafter=\${dayafter$ifiles:-$dayafter_save}
  eval TRANSFER_COMMAND=\${TRANSFER_COMMAND$ifiles:-$TRANSFER_COMMAND_save}
  eval PROCSCRIPT=\${PROCSCRIPT$ifiles:-nullexec}
  eval dsname=\$dsname$ifiles
  eval dsname_hist=\${dsname_hist$ifiles:-\$dsname$ifiles}
  eval TANKSUBDIR=\$TANKSUBDIR$ifiles
  eval TANKFILE=\${TANKFILE$ifiles:-\$dsname$ifiles}
  eval HOURS2ALARM=\${HOURS2ALARM$ifiles:-$HOURS2ALARM_save}
  eval CRITICAL=\${CRITICAL$ifiles:-$CRITICAL_save}
  eval timemade=\${timemade$ifiles:-$timemade_save}
  eval timetype=\${timetype$ifiles:-$timetype_save}
  eval user_spec_timedatecurr=\
\${user_spec_timedatecurr$ifiles:-$user_spec_timedatecurr_save}
  eval MTYPSBT=\${MTYPSBT$ifiles:-$MTYPSBT_save}
  eval wget_network_timeout=\${wget_network_timeout$ifiles:-$wget_network_timeout_save}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if [ $timetype = LOCAL ]; then
     set +x
     echo
     echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
     echo " ===> WARNING: Environment variable "timetype" imported with value "
     echo "               LOCAL - this is not supported on WCOSS where the    "
     echo "               local time is hardwired to the UTC time.  Abort     "
     echo "               with condition code 98.                             "
     echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
     echo
     set -x
     exit 98
  fi
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ksh $USHobsproc_satingest/ingest_process_onetype_newdays.sh
  err=$?

  echo "error from ingest_process_onetype_newdays.sh is " $err
  if [ $err -gt $errsave ] ; then
    errsave=$err
  fi

  ksh $USHobsproc_satingest/ingest_check_lapsed_data.sh \
   $USERDIR/$dsname_hist.history

done

err=$errsave

if [ $err -ne 0 ] # can this ever happen???
then

#####################################################################
# ABNORMAL RUN (NON FATAL)
set +x
echo " "
echo " ****** PROCESSING COMPLETED ABNORMALLY (R.C.=$err)"
echo " ****** PROCESSING COMPLETED ABNORMALLY (R.C.=$err)"
echo " ****** PROCESSING COMPLETED ABNORMALLY (R.C.=$err)"
echo " ****** PROCESSING COMPLETED ABNORMALLY (R.C.=$err)"
echo " "
set -x
#####################################################################

#################################


   msg="$DATATYPE PROCESSING FROM STATICALLY-NAMED FILES HAS COMPLETED \
ABNORMALLY WITH R.C.=$err  --> non-fatal"
   echo $msg
   echo $err
###$UTILROOT/ush/err_chk

else

#####################################################################
# GOOD RUN
set +x
echo " "
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " "
set -x
#####################################################################

#################################


   msg="$DATATYPE PROCESSING FROM STATICALLY-NAMED FILES HAS COMPLETED \
NORMALLY."
   echo $msg

fi
$UTILROOT/ush/postmsg "$jlogfile" "$msg"

############## END OF SCRIPT #######################
