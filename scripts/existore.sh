#!/bin/ksh
# Run under ksh

####  UNIX Script Documentation Block
#
# Script name:   existore.sh.ecf
#
# RFC contact:  Keyser      org: NP22        date: 2017-11-10
#
# Abstract: Entry point for a series of scripts that will handle the
#   processing/ingest of time-stamped files from a remote unix machine.
#
# Script history log:
# 1996-10-02  Bert Katz   Original version for implementation (existoru.sh.sms
#        and existorc.sh.sms).
# 1996-11-29  Bert Katz   Added check for interrupted processing and unified
#        the output file.
# 1997-01-10  Bert Katz   Sent unified output file to "stdout" and added a
#        debug option.
# 1997-06-06  Bert Katz   Added the ability to get different families of files
#        from different UNIX machines.
# 1997-06-20  Bert Katz   Added an option to use a BUFR table to unpack
#       "foreign" input BUFR files.
# 1997-10-21  Bert Katz   Added a new directory to locate unchanging input
#        files.  Also added an option to use a BUFR table to unpack "foreign"
#        input BUFR files.
# 1997-11-03  Bert Katz   Emergency fix to allow all file streams to complete
#        processing after a failure in one file stream.
# 1997-11-13  Bert Katz   Changed handling of lapses in processing so that such
#        errors are recorded in jlogfile, but are not displayed as fatal
#        errors.  In addition, modified the script so that different file
#        streams can be processed using different values of CNVRT2F77 and
#        ASSIGNFTYPE.
# 1998-01-06  Bert Katz   Changed handling of file transfer problems so that
#        such errors are recorded in jlogfile, but are not displayed as fatal
#        errors.
# 1998-05-12  Bert Katz   Changed use of check_lapsed_orbits to
#        check_lapsed_data, which is y2k compliant 
# 1998-09-15  Bert Katz   Added the ability to give different families of files
#        different file attributes.
# 1998-12-02  Bert Katz   Established "nobufrtable" as default for variable
#        BUFRTABLE.
# 1998-12-23  Bert Katz   Changed handling of repeated file processing so that
#        such errors are recorded in jlogfile, but are not displayed as fatal
#        errors.
# 2000-11-21  D. Keyser   Modified the script so that different file streams
#        can be processed using different values of HISTLENMIN and HISTLENMAX.
#        If not found for a particular file stream, the global value is used.
# 2001-03-21  D. Keyser   Modified the script to add new value COMPRESS
#        (different file streams can be processed using different values of
#        COMPRESS - if not found for a particular file stream, the global
#        value is used).
# 2001-03-21  L. Sager    Modified the script to add new value UNCOMPRESS_UNIX
#        (different file streams can be processed using different values of
#        UNCOMPRESS_UNIX - if not found for a particular file stream, the
#        global value is used).
# 2001-05-14  D. Keyser   Modified the script to add new value ATTRIBUTES
#        (different file streams can be processed using different values of
#        ATTRIBUTES, if not found for a particular file stream, the global
#        value is used).
# 2004-05-10  D. Keyser   Modified the script to add new values PROCESS_Ta and
#        PROCESS_Tb (different file streams can be processed using different
#        values of PROCESS_Ta and PROCESS_Tb - if not found for a particular
#        file stream, the global value is used).
# 2005-04-29  D. Keyser   Modified the script to add new values PROCESS_Ta
#        and PROCESS_Tb (different file streams can be processed using
#        different values of PROCESS_Ta and PROCESS_Tb, if not found for a
#        particular file stream, the global value is used).
# 2006-04-19 P. OReilly   Modified the script to accept unique values for the
#        value HOURS2ALARM from job scripts.
# 2006-06-14  P. OReilly  Modified the script to add variable "CRITICAL" which
#        is used to set flags for Big Brother monitoring in script
#        ingest_check_lapsed_data.
# 2006-08-12  D. Keyser   Combines/generalizes previous scripts existorc.sh.sms
#        (for CEMSCS machine only) and existoru.sh.sms (for unix machines
#        only) and changed to account for all remote machines now being unix
#        (since MVS CEMSCS machine was replaced with unix DDS machine).
#        Improved Docblock and comments.  Accepts imported value for path to
#        ingest_qmgr.sh, $utilscript, defaults to /nwprod/util/ush.  If
#        execution of ingest_qmgr.sh returns status code of 99, now posts a
#        message to the joblog file that another job with this same name is in
#        the system and this ingest job will continue but not ingest any
#        satellite data (before no message was posted).
# 2007-05-14  D. Keyser   Now stops with FATAL error for case where no eligible
#        executable or script is found in one or more groups (families) (return
#        code 244).  Modified the script to add new values ITRIES_MAX_QUERY and
#        ITRIES_MAX_GET to set the maximum number of failed attempts to ftp
#        query and ftp get before giving up, respectively (both default to "2")
#        and to add the new value IFILES_MAX_GET to set the maximum number of
#        new files for which ftp transfers for a file family will occur
#        (defaults to "999").  Different file streams can be processed using
#        different values of ITRIES_MAX_QUERY, ITRIES_MAX_GET and
#        IFILES_MAX_GET (if not found for a particular file stream, the global
#        value is used).  Minor docblock changes.
# 2008-01-02  D. Keyser   Modified the script to add new value COPYFILES which,
#        when "YES", attempts to copy individual files pulled over from the
#        remote unix machine to directory $TANKDIR/<YYYYMMDD>/wbufbul after
#        translating them into NCEP BUFR and then standardizing them into WMO
#        BUFR (defaults to "NO").  Different file streams can be processed
#        using different values of COPYFILES (if not found for a particular
#        file stream, the global value is used).
# 2009-05-04  D. Keyser   Modified the script to add new value mod_sec3_desc to
#        alert the  program bufr_tranmtypsbt (if it is run) to either overwrite
#        (=YES) or not overwrite (=NO) the top-level sequence descriptor in
#        Sec. 3 of every BUFR message with the read-in value from the external
#        BUFR mnemonic table (defaults to "YES").  Different file streams can
#        be processed using different values of mod_sec3_desc (if not found for
#        a particular file stream, the global value is used).
# 2012-10-18  D. Keyser   Modified the script to add new value TRANSFER_COMMAND
#        which determines the type of transfer to be done (e.g., 'ftp', 'sftp',
#        'lftp', 'wget').  Different file streams can be processed using
#        different values of TRANSFER_COMMAND (if not found for a particular
#        file stream, the global value is used).  WCOSS/Eddy currently does not
#        support ftp, but it does support wget (later WCOSS/Tide does support
#        ftp for some queues).  Modified to run on WCOSS. Specifically, since
#        WCOSS default script (sh) is bash, all child scripts are executed
#        under ksh (the CCS default) rather than under sh (some script commands
#        tranferred from CCS version do not work under bash).  This script is
#        now set to run under ksh shell as the default.  Script cwordsh now
#        unblocks files rather than blocks files.  Renamed to existore.sh.ecf
#        since now scheduled by ecFlow.  
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
#        scripts ingest_check_lapsed_data and ingest_process_onetype_neworbits
#        both of which this script executes.  USH script cwordsh renamed to
#        bufr_cword.sh and moved from directory path $USHbufr to directory path
#        $USHobsproc_shared_bufr_cword.  USH script tranjb renamed to
#        bufr_tranjb.sh and moved from directory path $USHbufr to directory
#        path $USHobsproc_satingest.  Aborts with condition code 98 if
#        environment variable "timetype" imported with value "LOCAL" - this is
#        not supported on WCOSS where the local time is hardwired to the UTC
#        time.  The default for $timetype default is now "UTC" rather than
#        "GMT".  Added new comments.  Updated information in docblock and some
#        existing comments.
# 2014-07-30  D. Keyser   Updated definitions of script variables TARGETFILE,
#        TARGETFILEn, TANKFILEn and EXECUTEn in the remarks section of the
#        docblock to reflect a new option for simple file copying in ush script
#        ingest_process_orbits.sh when TARGETFILE is imported as "same_name"
#        for case when script variable EXECUTE is imported as "copy_to_target".
# 2014-09-04  D. Keyser   Modified the script so that different file streams
#        can be processed using different values of wget_network_timeout
#        (connection or read timeout in seconds).  If not found for a
#        particular file stream, the global value is used.
# 2015-07-03  D. Keyser   Updated definitions of script variables TARGETFILE,
#        TARGETFILEn, TANKFILEn and EXECUTEn in the remarks section of the
#        docblock to reflect a new option for simple file copying in ush script
#        ingest_process_orbits.sh when TARGETFILE is imported as "same_name2"
#        for case when script variable EXECUTE is imported as "copy_to_target".
# 2015-08-07  D. Keyser   Improved docblock description for variable FTYPE, and
#        updated its definition to reflect that it no longer controls whether
#        or not file uncompression will be performed if UNCOMPRESS_UNIX is set
#        to YES.  Updated definition of script variable UNCOMPRESS_UNIX to
#        reflect that it no longer has a dependency on FTYPE value, and that
#        files will be uncompressed only if they contain 1 of the 8 specific
#        file name suffixes noted in the gzip man page.
# 2011-11-17  D. Keyser
#           - Modified to pass along new environment variable PROC_MULT_FILES
#        which, when "YES", concatenates all input files for a particular
#        family (with FTYPE "bufr" or "ncepbufr") into a single file prior to
#        any subsequent processing. Different file streams can be processed
#        using different values of PROC_MULT_FILES (if not found for a
#        particular file stream, the global value is used).  The default is
#        PROC_MULT_FILES=NO (which, as before, processes each new file one at a
#        time).
#           - Modified to pass along new environment variable IFILES_MAX_MULT
#        which defines the  maximum number of new files on the remote machine
#        for which the files will be concatenated prior to processing when
#        new environment variable PROC_MULT_FILES for this family is set to
#        "YES".  If greater than IFILES_MAX_MULT new files are found on the
#        remote machine for a particular file family, PROC_MULT_FILES will be
#        reset to "NO" for this family.  Different file streams can be
#        processed using different values of IFILES_MAX_MULT (if not found for
#        a particular file stream, the global value is used).  The default is
#        IFILES_MAX_MULT=100.
# 2016-01-08  D. Keyser   Corrected definition of script variable ndayarch in
#        the remarks section of the docblock.  Modified the script so that
#        different file streams can be processed using different values of
#        ndayarch (number of days files are retained in root directory path
#        under certain situations).  If not found for a particular file stream,
#        the global value is used.  Added definition of existing script
#        variable CLEAN in the remarks section of the docblock.
# 2016-04-21  D. Keyser/D. Stokes
#           - Modified to pass along new environment variable SUBDATE_CHECK
#        which, when "YES", forces program bufr_tranjb to unpack each subset
#        from the current input BUFR message in order to check its date in
#        order to ensure that all BUFR messages opened for appending to the
#        output TANK file have a sec. 1 date (year, month, day, hour) that is
#        the same as each subset within the message.  If set to "NO", program
#        bufr_tranjb will append each input BUFR message to the output TANK
#        file, as is, and no date checking will be performed.  This must be set
#        within individual families, there is no global value.  If this is not
#        set for a particular family, then bufr_tranjb will determine the value
#        for SUBDATE_CHECK based on message compression.
#           - Modified to pass along new environment variable MESSAGE_LENGTH
#        which sets the output BUFR message length upper limit (in bytes) in
#        program bufr_tranjb.  This must be set within individual families,
#        there is no global value.  If this is not set for a particular family,
#        then bufr_tranjb will set it to the default BUFRLIB message length
#        upper limit.
#           - Modified to pass along new environment variable MXMSGL which sets
#        BUFRLIB maximum allowable message length (in bytes) for input or output
#        in program bufr_tranjb.  This must be set within individual families,
#        there is no global value.  If this is not set for a particular family,
#        the bufrlib default will be used (unless overridden by a larger value
#        imported as variable MESSAGE_LENGTH)
# 2016-06-10  D. Stokes   Updated docblock to modify definition of script
#        variable IFILES_MAX_GET and to remove reference to condition code 2 in
#        remarks since it can no longer occur.
# 2016-06-10  D. Keyser   Updated docblock to modify definition of script
#        variable UNCOMPRESS_UNIX.
# 2016-07-18  D. Keyser   Modified to pass along new environment variable
#        RENAME_FILE_SUFFIX which allows an original, input filename to be
#        modified by appending the suffix $RENAME_FILE_SUFFIX to the name when
#        copying it to output path in ush script ingest_copy.sh. This must be
#        set within individual families, there is no global value.
# 2017-11-10  D. Keyser   Modified to pass along new environment variable
#        $REMOTEDIRGRP which, when set, specifies the directory path to
#        $REMOTEDSNGRP, which is then defined as the files containing the
#        leading portion of the name of a family of files from the remote unix
#        machine.  This is invoked only when $TRANSFER_COMMAND is imported as
#        "ftp" or "wget". $REMOTEDIRGRP must begin with "/".  If $REMOTEDIRGRP
#        is not set it defaults to "." (current directory) and $REMOTEDSNGRP is
#        expected to contain the complete path to the leading portion of the
#        name of a family of files from the remote unix machine (it's "normal"
#        definition).
#        BENEFIT: Allows two different file families pulling the same file names
#                 from two different servers (e.g., a primary and backup) in two
#                 different directories to store file listings in same history
#                 files.  Currently used in the LGYCLD1 and LGYCLD2 families in
#                 job JIGOES_RADSND - needed because LGYCLD1 (the backup)
#                 currently pulls LaRC GOES cloud files from an ESRL gsdftp
#                 server in a different directory than the same files pulled by
#                 LGYCLD2 (the primary) which continues to pull these same files
#                 from the LaRC typhoon server.
# 2022-01-18  S. Stegall  Replaced $DATA/ before calling utility scripts and instead 
#         used $UTILROOT/ush/ to properly leverage the prod_util module.
#
# Usage: existore.sh.ecf
#
#   Script parameters: None
#
#   Modules and files referenced:
#     scripts    : $USHobsproc_satingest/ingest_qmgr.sh
#                  $USHobsproc_satingest/ingest_process_onetype_neworbits.sh
#                  $USHobsproc_satingest/ingest_check_lapsed_data.sh
#                  $UTILROOT/ush/prep_step
#                  $UTILROOT/ush/postmsg
#                  $UTILROOT/ush/err_exit
#     data cards : file(s) containing BUFR tables (see below)
#     executables: executable(s) to create BUFR messages and append to tank
#                  file (see below)
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
#                     244 - No eligible executable or script found in one or
#                           more groups (families) -- this is a FATAL error
#       In addition, this script will complete with condition code ZERO but
#        will stamp out an ABNORMAL COMPLETION diagnostic if any one of the
#        groups (families) returns a status code from child script
#        ingest_process_onetype_neworbits.sh that is > 0 - these include:
#                       1 - Query failed to produce any files to process
#                     111 - All files to be processed in a particular group
#                           (family) were unprocessable
#                     199 - One or more files were untransferable in last 5 (or
#                           possibly some other number) runs of this job
#                     220 - No action specified for multiple file families
#                     222 - All files to be processed were already processed
#                     230 - No files submitted for processing
#                     254 - Failure in sort of list of files to be processed or
#                           in sort of files in history file
#
#
#   The following script variables are user controllable.  They must be
#    specified by the user in the script that invokes
#   $USHobsproc_satingest/ingest_process_onetype_neworbits.sh (except where
#     there are default values noted).
#
#     USHobsproc_satingest : path to obsproc_satingest ush directory.
#     USHobsproc_shared_bufr_cword : path to obsproc_shared ush directory
#       containing bufr_cword.sh.
#     USHbufr : path to BUFR ush directory.
#     FIXobsproc_satingest : path to obsproc_satingest fix directory.
#     FIXbufr : path to BUFR fix directory.
#     USERDIR : path to directory containing housekeeping and file-processing
#       history files.
#     DATA : path to directory where files from the remote unix machine are to
#       be put.
#     TANKDIR : path to directory where BUFR database tanks are being created
#       and appended to.
#     OUTDIR : path to directory where output listing is to be put.
#     FTYPE : file type used to determine if some preprocessing on the file
#       transferred from the remote unix machine is necessary.  One may also
#       specify a different file assignment for one or more file families as
#       FTYPE1, FTYPE2,. . . (see below).  Possible values are:
#              bufr     - execute $USHobsproc_shared_bufr_cword/bufr_cword.sh
#                         to unblock file and strip off any extraneous
#                         characters from BUFR messages prior to either:
#                           1) executing any translation programs or scripts
#                              which themselves execute
#                              $USHobsproc_satingest/bufr_tranjb.sh after all
#                              other processing is complete
#                                     -- or --
#                           2) executing $USHobsproc_satingest/bufr_tranjb.sh
#                              without any prior execution of translation
#                              programs or scripts
#                         Note: In this case
#                               $USHobsproc_shared_bufr_cword/bufr_cword.sh is
#                               NEVER executed inside
#                               $USHobsproc_satingest/bufr_tranjb.sh.
#              ncepbufr - execute $USHobsproc_shared_bufr_cword/bufr_cword.sh
#                         to unblock file and strip off any extraneous
#                         characters from BUFR messages within execution of
#                         $USHobsproc_satingest/bufr_tranjb.sh
#                         Note: This should only be set if EXECUTEn is exported
#                               as bufr_tranjb.sh (i.e., there is no prior
#                               execution of a translation program or script).
#              none     - do nothing (default)
#     UNCOMPRESS_UNIX : if set to "YES", files from a remote unix machine are
#       assumed to be unix-compressed and are uncompressed using either
#       "gunzip" or "bunzip2" depending upon suffix character string in input
#       file name.  One may also specify different values for one or more file
#       families as UNCOMPRESS_UNIX1, UNCOMPRESS_UNIX2,. . . (see below).
#       Defaults to "NO". (No dependency on value for FTYPE.)
#       Note: A file will be uncompressed only if its name ends in one of the
#             following strings: ".gz", "-gz", ".z", "-z", "_z", ".Z", "-Z" or
#             "_Z"  for "gunzip"; or ".bz2" or ".bz" for "bunzip2".  If a
#             file's name does not end in one of the above strings, it will not
#             be uncompressed.  This will be noted, and the file will be
#             skipped (it will also be skipped if uncompression fails).
#     HISTLENMIN, HISTLENMAX : minimum and maximum length of the file keeping
#       track of the file-processing history. Each line of the file corresponds
#       to one file successfully processed.  One may also specify a different
#       minimum and/or maximum length for one or more file families as
#       HISTLENMIN1, HISTLENMIN2,. . . or HISTLENMAX1, HISTLENMAX2,. . . (see
#       below). HISTLENMIN defaults to "900" and HISTLENMAX defaults to "1200".
#     TARGETFILE : either:
#              - imported as "same_name" in which case a file is just simply
#                copied from the current working directory to the same file
#                name in directory path $TANKDIR/$TANKFILE
#                  -- or --
#              - imported as "same_name2" in which case a file is just simply
#                copied from the current working directory to the same file
#                name in directory path $TANKDIR/<YYYYMMDD>/$TANKFILE
#                  -- or --
#              - the name of the output file when simply copied from the
#                current working directory to directory path
#                $TANKDIR/<YYYYMMDD>/$TANKFILE.
#              (Invoked only when EXECUTEn is imported as "copy_to_target".)
#              (See TANKDIR above, and TANKFILEn and EXECUTEn below.)
#              One may also specify a different filename for one or more file
#              families, TARGETFILE1, TARGETFILE2, . . . (see below).  Defaults
#              to "NO".
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
#     HOURS2ALARM : in case file transfers or processing come to a halt, this
#       is the number of hours that will pass before a return code will be
#       issued to trigger an alarm mechanism.  One may also specify a different
#       number of hours for one or more file families as HOURS2ALARM1,
#       HOURS2ALARM2,. . . (see below).  Defaults to "30".
#     CRITICAL : if data type is critical to NCEP production, variable is set
#       to "YES" and data will flag red in Big Brother when there is a lapse in
#       the data.  If data type is not critical, variable is set to default
#       value of "NO". One may specify different values for different file
#       families such as CRITICAL1, CRITICAL2, . . . (see below).  As said,
#       defaults to "NO".
#     ndayarch : number of days for which input files previously transferred
#       from the remote unix machine without any processing will be retained in
#       root directory path (applies only when $EXECUTEn is exported as
#       "ingest_copy.sh" and only when $CLEAN is either exported as "YES" or
#       defaults to "YES" in ush script ingest_copy.sh).  One may also specify
#       a different number days to retain files for one or more file families
#       as ndayarch1,, ndayarch2 ,. . . (see below).  Defaults to "30".
#     CLEAN : if set to "YES", remove files stored in root directory path that
#       are more than $ndayarch or $ndayarchN days old (applies only when
#       $EXECUTEn is exported as "ingest_copy.sh").  There is no default here,
#       the default is set in ush script ingest_copy.sh.
#     DELAFTPROC : if set to "YES", files from the remote unix machine are not
#       kept after processing.
#     COPYFILES  : if set to "YES", individual files pulled from the remote
#       unix machine are copied to $TANKDIR/<YYYYMMDD>/wbufbul directory after
#       being translated into NCEP BUFR and then standardized into WMO BUFR
#       Note: This currently works only for files with name:
#                           *.Dyyddd.S*.*
#                           *.Dyyddd.T*.*
#                           *.Dyyyymmddhh.T*.*
#                           *.Dyyyymmddhh.S*.*
#             (where yy is 2-digit year, ddd is day-of-year, yyyy is 4-digit
#              year, mm is month-of-year, dd is day-of-month, hh is hour-of-
#              day)
#       One may also specify a different value for one or more file families as
#       COPYFILES1, COPYFILES2,. . . (see below).  Defaults to "NO".
#     MACHINE : name of remote unix machine to be used in transfer requests.
#       One may also specify a different machine for one or more file families
#       as MACHINE1, MACHINE2,. . . (see below).  Defaults to "none".
#     TRANSFER_COMMAND : type of transfer to be done (e.g., 'ftp', 'sftp',
#       'lftp', 'wget').  One may also specify a different transfer command for
#       one or more file families as TRANSFER_COMMAND1, TRANSFER_COMMAND2,. . .
#       (see below).  Defaults to "ftp".
#            Note: Currently should use 'sftp' only for connections to same
#                  WCOSS machine as that in which this script is running. Note:
#                  'sftp' will not work for pulling files from other WCOSS
#                  machine.
#     log : full path name of the file to which standard output from
#       BUFR_TRANJB should be directed.  One may also specify a different
#       BUFR_TRANJB standard output file name for one or more file families as
#       log1, log2,. . . (see below).  Defaults to "$OUTDIR/tranjb.out".
#     TANKPROTECT : if set to "YES", the tank file being updated will be copied
#       to a temporary file before being updated, after which the temporary
#       tank file will be moved over the permanent tank file.  If set to
#       anything other than "YES", the tank file will be updated in situ.
#     COMPRESS : if set to "YES", the tank file being updated will be
#       compressed (in BUFR) if there is an option to compress the particular
#       data type.  If set to anything other than "YES", the tank file will
#       remain uncompressed BUFR.  One may also specify different values for
#       one or more file families as COMPRESS1, COMPRESS2,. . . (see below).
#       Defaults to "NO".
#     DEBUGSCRIPTS : if set to "ON" or "YES", all scripts will run with
#      "set -x" on. Intended for debugging.
#     PROCESS_Ta : if set to "YES",  then process antenna temperature (Ta)
#       reports into BUFR, otherwise do not.  One may also specify different
#       values for one or more file families as PROCESS_Ta1, PROCESS_Ta2,
#       . . . (see below).  (Note: Antenna temperature can only be processed
#       for ATOVS AMSU-A data.)  Defaults to "NO".
#     PROCESS_Tb : if set to "YES",  then process brightness temperature (Tb)
#       reports into BUFR, otherwise do not.  One may also specify different
#       values for one or more file families as PROCESS_Tb1, PROCESS_Tb2,
#       . . . (see below).  Defaults to "YES".
#     ITRIES_MAX_QUERY: the maximum number of failed attempts to query files on
#       the remote machine before giving up.  One may also specify a different
#       maximum  number of file query attempts for one or more file families
#       as ITRIES_MAX_QUERY1, ITRIES_MAX_QUERY2,. . . (see below).  Defaults to
#       "2".
#     ITRIES_MAX_GET: the maximum number of failed attempts to transfer a file
#       from the remote machine before giving up.  One may also specify a
#       different maximum number of file transfer attempts for one or more file
#       families as ITRIES_MAX_GET1, ITRIES_MAX_GET2,. . . (see below).
#       Defaults to "2".
#     IFILES_MAX_GET: the maximum number of new files on the remote machine for
#       which transfers for a file family will occur.  If greater than
#       $IFILES_MAX_GET new files are found on the remote machine for a
#       particular file family, the job will transfer and process only the first
#       $IFILES_MAX_GET new files for that family.  One may also specify a
#       different number of new file transfer limits for one or more file
#       families as IFILES_MAX_GET1, IFILES_MAX_GET2,. . . (see below).
#       Defaults to "999".
#     PROC_MULT_FILES: if set to "YES", all input files for a particular family
#       with FTYPE "bufr" or "ncepbufr" will be concatenated into a single file
#       prior to any processing.  If set to anything other than "YES", each
#       input file will be processed separately, one-by-one.  (Applies only
#       when $FTYPE or $FTYPE1 is "bufr" or "ncepbufr".  For anything else,
#       $PROC_MULT_FILES will be reset to "NO" for this family.)  One may also
#       specify different values for one or more file families as
#       PROC_MULT_FILES1, PROC_MULT_FILES2,. . . (see below).  
#       Defaults to "NO".
#     IFILES_MAX_MULT: the maximum number of new files on the remote machine
#       for which the files will be concatenated prior to processing when
#       switch $PROC_MULT_FILES for this family is set to "YES".  If greater
#       than $IFILES_MAX_MULT new files are found on the remote machine for a
#       particular file family, $PROC_MULT_FILES will be reset to "NO" for this
#       family and files will be processed one at a time rather than after all
#       new files are first concatenated. (Applies only when $PROC_MULT_FILES
#       or $PROC_MULT_FILESn is "YES".)  Prevents the creation of a
#       concatenated file that is so big that it might greatly slow down
#       processing.  One may also specify a different number of new file
#       concatenation limits for one or more file families as IFILES_MAX_MULT1,
#       IFILES_MAX_MULT2,. . . (see below).
#       Defaults to "100".
#     mod_sec3_desc: if set to "NO", program bufr_tranmtypsbt (if it is run)
#       will not overwrite the top-level sequence descriptor in Sec. 3 of every
#       BUFR message with the read-in value from the external BUFR mnemonic
#       table, otherwise it will overwrite this.  (See Docblock in program
#       bufr_tranmtypsbt for more information.)  One may also specify different
#       values for one or more file families as mod_sec3_desc1, mod_sec3_desc2,
#       . . . (see below).  Defaults to "YES".
#     wget_network_timeout: connection or read timeout in seconds (applies only
#       when $TRANSFER_COMMAND or $TRANSFER_COMMANDn is "wget").  Applies to
#       both ingest_query.sh and ingest_get.sh.  One may also specify a
#       different connection or read timeout value for one or more file
#       families as wget_network_timeout1, wget_network_timeout2,. . . (see
#       below).  Defaults to "120".
#     wget_tries: number of times to try to connect (no retries for fatal
#       errors) (applies only when $TRANSFER_COMMAND or $TRANSFER_COMMANDn is
#      "wget").
#       Defaults to "1" in ingest_query.sh.
#       Defaults to "2" in ingest_get.sh.
#     lftp_dns_timeout: time limit for DSN queries in seconds (applies only
#       when $TRANSFER_COMMAND or $TRANSFER_COMMANDn is "lftp").
#       Defaults to "120" in both ingest_query.sh and ingest_get.sh.
#     lftp_recon_int_base: base minimal time between reconnects in seconds
#       (applies only when $TRANSFER_COMMAND or $TRANSFER_COMMANDn is "lftp").
#       Defaults to "120" in both ingest_query.sh and ingest_get.sh.
#     lftp_max_tries: number of times to try to connect (applies only when
#       $TRANSFER_COMMAND or $TRANSFER_COMMANDn is "lftp").
#       Defaults to "1" in ingest_query.sh.
#       Defaults to "2" in ingest_get.sh.
#     lftp_recon_int_mult: multiplier by which lftp_recon_int_base (see above)
#       is multiplied each time a new attempt to reconnect occurs (applies only
#       when $TRANSFER_COMMAND or $TRANSFER_COMMANDn is "lftp").
#       Defaults to "1" in both ingest_query.sh and ingest_get.sh.
#
#
#
#       The following environmental variables allow multiple file families on
#       the remote unix machine to be processed with one invocation of
#       $USHobsproc_satingest/ingest_process_onetype_neworbits.sh :
#     
#     ngroup : The number of file families from the remote unix machine that
#       are being processed.  
#
#       In the definitions that follow, the suffixed "n" or "N" means that a
#       list of variables such as : ORBITLIST1, ORBITLIST2, . . ,
#       ORBITLIST$ngroup should be defined for use by the script
#       $USHobsproc_satingest/ingest_process_onetype_neworbits.sh (some values 
#       below do have default values).
#
#     COPYFILESn : if set to "YES", individual files pulled from the remote
#       unix machine are copied to $TANKDIR/<YYYYMMDD>/wbufbul directory after
#       being translated into NCEP BUFR and then standardized into WMO BUFR
#       Note: This currently works for files with name:
#                           *.Dyyddd.S*.*
#                           *.Dyyddd.T*.*
#                           *.Dyyyymmddhh.T*.*
#                           *.Dyyyymmddhh.S*.*
#             (where yy is 2-digit year, ddd is day-of-year, yyyy is 4-digit
#              year, mm is month-of-year, dd is day-of-month, hh is hour-of-
#              day)
#       For a particular family "n", if $COPYFILESn does not exist, then the
#       global default value $COPYFILES is used (see above).
#     MACHINEn : name of remote unix machine to be used in transfer requests.
#       For a particular family "n", if $MACHINEn does not exist, then the
#       global default value "$MACHINE" is used (see above).
#     TRANSFER_COMMANDn : type of transfer to be done (e.g., 'ftp', 'sftp',
#       'lftp', 'wget').  For a particular family "n", if $TRANSFER_COMMANDn
#       does not exist, then the global default value "TRANSFER_COMMAND" is
#       used (see above).
#     ORBITLISTn : the file keeping track of a family of files to be processed
#       on the remote unix machine.  The file containing the file-processing
#       history will have the name ORBITLISTn.history.
#     REMOTEDIRGRPn : the directory path to REMOTEDSNGRPn, which is then defined
#       as the files containing the leading portion of the name of a family of
#       files from the remote unix machine. REMOTEDIRGRP must begin with "/".
#       (Invoked only when TRANSFER_COMMAND or TRANSFER_COMMANDn is imported as
#        "ftp" or "wget".)
#       If REMOTEDIRGRPn is not set it defaults to "." (current directory) and
#       REMOTEDSNGRPn is expected to contain the complete path to the leading
#       portion of the name of a family of files from the remote unix machine.
#     REMOTEDSNGRPn : the leading portion of the name of a family of files from
#       the remote unix machine.
#       (Normally this is defined as the complete path to these files, however
#        if REMOTEDIRGRPn is set, it is then defined as only the files
#        themselves.)
#     FTYPEn : file type used to determine if some preprocessing on the file
#       transferred from the remote unix machine is necessary.  For a
#       particular family "n", if $FTYPEn does not exist, then the global
#       default value "$FTYPE" is used (see above, also see above for possible
#       values).
#     logN : full path name of the file to which standard output from
#       BUFR_TRANJB should be directed.  For a particular family "N", if $logN
#       does not exist, then the global default value "$log" is used (see
#       above).
#     HISTLENMINn, HISTLENMAXn : minimum and maximum length of the file keeping
#       track of the file-processing history. Each line of the file corresponds
#       to one file successfully processed.  For a particular family "n", if
#       $HISTLENMINn or $HISTLENMAXn does not exist, then the global default
#       values "$HISTLENMIN" and "$HISTLENMAX" are used (see above).
#     TANKFILEn : either:
#              - the final part of the path to the name of the BUFR tank file
#                to be created/appended (normally a single sub-directory and
#                the file name in the form bTTT/xxSSS, where TTT is the BUFR
#                message type and SSS is the BUFR message subtype)
#                (Invoked only when EXECUTEn is imported as the name of a
#                 program or script used to create/append BUFR messages to the
#                 tank file.) 
#                  -- or --
#              - either:
#                    - sub-directory in path $TANKDIR containing output file
#                      simply copied from the current working directory with
#                      the same file name (e.g., file is
#                      $TANKDIR/$TANKFILE/<filename>) {when TARGETFILE (or
#                      TARGETFILE1) is imported as "same_name"}
#                        -- or --
#                    - sub-directory in path $TANKDIR/<YYYYMMDD> containing
#                      output file simply copied from the current working
#                      directory with same file name (e.g., file is
#                      $TANKDIR/<YYYYMMDD>/$TANKFILE/<filename>) {when
#                      TARGETFILE (or TARGETFILE1) is imported as "same_name2"}
#                        -- or --
#                    - sub-directory in path $TANKDIR/<YYYYMMDD> containing
#                      output file simply copied from the current working
#                      directory with file name TARGETFILE (or TARGETFILEn)
#                      (e.g., file is $TANKDIR/<YYYYMMDD>/$TANKFILE/$TARGETFILE)
#                      {when TARGETFILE (or TARGETFILE1) is imported as a file
#                       name (anything other than "same_name" or "same_name2")}.
#                    (Invoked only when EXECUTEn is imported as
#                     "copy_to_target".)
#                    {See TANKDIR above, and TARGETFILE above (or TARGETFILEn
#                     below).}
#              (See EXECUTEn below.)
#     EXECUTEn : the name of a program or script used to create/append BUFR
#       messages to the tank file.  The record of the transaction will be
#       written to EXECUTEn.out and EXECUTEn.errlog in the TANKDIR (see above)
#       directory.
#       An exception occurs when EXECUTEn is set to "copy_to_target", in which
#       case a file is just simply copied from the current working directory to
#       either:
#              - the same file name in directory path $TANKDIR/$TANKFILE {when
#                TARGETFILE (or TARGETFILEn) is imported as "same_name")
#                  -- or --
#              - the same file name in directory path
#                $TANKDIR/<YYYYMMDD>/$TANKFILE {when TARGETFILE (or TARGETFILEn)
#                is imported as "same_name2")
#                  -- or --
#              - file name TARGETFILE (or TARGETFILEn) in directory path
#                $TANKDIR/<YYYYMMDD>/$TANKFILE {when TARGETFILE (or TARGETFILEn)
#                is imported as anything other than "same_name" or
#                "same_name2"}.
#              {See TANKDIR and TANKFILEn above, and TARGETFILE above (or
#               TARGETFILEn below).}
#       If EXECUTEn is not set or if it is set to "copy_to_target", the script
#       "ingest_translate_orbits" will not be executed.
#     TARGETFILEn : either:
#              - imported as "same_name" in which case a file is just simply
#                copied from the current working directory to the same file
#                name in directory path $TANKDIR/$TANKFILE
#                  -- or --
#              - imported as "same_name2" in which case a file is just simply
#                copied from the current working directory to the same file
#                name in directory path $TANKDIR/<YYYYMMDD>/$TANKFILE
#                  -- or --
#              - the name of the output file when simply copied from the
#                current working directory to directory path
#                $TANKDIR/<YYYYMMDD>/$TANKFILE.
#              (Invoked only when EXECUTEn is imported as "copy_to_target".)
#              (See TANKDIR, TANKFILEn and EXECUTEn above.)
#              For a particular family "n", if $TARGETFILEn does not exist,
#              then the global default value "$TARGETFILE" is used (see above).
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
#     BUFRTABLEn : the file holding the BUFR table which controls the creation
#       of BUFR messages to be added to the tank file.  Defaults to
#       "nobufrtable".
#     FORGNTABLEn : the file holding the BUFR table which controls the reading
#       of foreign BUFR messages, if the input file is in BUFR.  Defaults to
#       "noforgntable".
#     HOURS2ALARMn : in case file transfers or processing come to a halt, this
#       is the number of hours that will pass before a return code will be
#       issued to trigger an alarm mechanism.  For a particular family "n", if
#       $HOURS2ALARMn does not exist, then the global default value
#       "$HOURS2ALARM" is used (see above).
#     CRITICALn : if data type is critical to NCEP production, variable is set
#       to "YES" and data will flag red in Big Brother when there is a lapse in
#       the data.  If data type is not critical, variable is set to default
#       value of "NO". For a particular family "n", if $CRITICALn does not
#       exist, then the global default value "$CRITICAL" is used (see above).
#     RENAME_FILE_SUFFIXn : string to append to original, input filename (as a
#       suffix) when writing it to output path, thus allowing output filename
#       to be different than input filename (applies only when $EXECUTEn is
#       exported as "ingest_copy.sh"). For a particular family "n", if
#       RENAME_FILE_SUFFIXn does not exist, then default is to not set this
#       variable here (i.e., output filename remains same as original, input
#       filename. (See Docblock in ush script ingest_copy.sh for more
#       information.) 
#     ndayarchN : number of days for which input files previously transferred
#       from the remote unix machine without any processing will be retained in
#       root directory path (applies only when $EXECUTEn is exported as
#       "ingest_copy.sh" and only when $CLEAN is either exported as "YES" or
#       defaults to "YES" in ush script ingest_copy.sh).  For a particular
#       family "N", if $ndayarchN does not exist, then the global default value
#       "$ndayarch" is used (see above).
#     COMPRESSn : if set to "YES", the tank file being updated will be
#       compressed (in BUFR) if there is an option to compress the particular
#       data type.  If set to anything other than "YES", the tank file will
#       remain uncompressed BUFR.  For a particular family "n", if $COMPRESSn
#       does not exist, then the global default value "$COMPRESS" is used (see
#       above).
#     UNCOMPRESS_UNIXn : if set to "YES", files from a remote unix machine are
#       assumed to be unix-compressed and are uncompressed using either
#       "gunzip" or "bunzip2" depending upon suffix character string in input
#       file name.   For a particular family "n", if $UNCOMPRESS_UNIXn does not
#       exist, then the global default value "$UNCOMPRESS_UNIX is used (see
#       above).
#     PROCESS_TaN : if set to "YES",  then process antenna temperature (Ta)
#       reports into BUFR, otherwise do not.  For a particular family "N", if
#       $PROCESS_TaN does not exist, then the global default value
#       "$PROCESS_Ta" is used (see above).  (Note: Antenna temperature can only
#       be processed for ATOVS AMSU-A data.)
#     PROCESS_TbN : if set to "YES",  then process brightness temperature (Tb)
#       reports into BUFR, otherwise do not.  For a particular family "N", if
#       $PROCESS_TbN does not exist, then the global default value
#       "$PROCESS_Tb" is used (see above).
#     ITRIES_MAX_QUERYn: the maximum number of failed attempts to query files
#       on the remote machine before giving up.  For a particular family "n",
#       if "$ITRIES_MAX_QUERYn" does not exist, then the global default value
#       "$ITRIES_MAX_QUERY" is used (see above).
#     ITRIES_MAX_GETn: the maximum number of failed attempts to transfer a file
#       from the remote machine before giving up.  For a particular family "n",
#       if "$ITRIES_MAX_GETn" does not exist, then the global default value
#       "$ITRIES_MAX_GET" is used (see above).
#     IFILES_MAX_GETn: the maximum number of new files on the remote machine
#       for which transfers for a file family will occur.  If greater than
#       $IFILES_MAX_GETn new files are found on the remote machine for a
#       particular file family, the job will transfer and process only the first
#       $IFILES_MAX_GETn new files for that family.  For a particular family
#       "n", if "$IFILES_MAX_GETn" does not exist, then the global default
#       value "$IFILES_MAX_GET" is used (see above).
#     PROC_MULT_FILESn: if set to "YES", all input files for a particular
#       family with FTYPE "bufr" or "ncepbufr" will be concatenated into a
#       single file prior to any processing.  If set to anything other than
#       "YES", each input file will be processed separately, one-by-one.
#       (Applies only when $FTYPE or $FTYPE1 is "bufr" or "ncepbufr".  For
#       anything else, $PROC_MULT_FILESn will be reset to "NO" for this
#       family.)  For a particular family "n", if $PROC_MULT_FILESn does not
#       exist, then the global default value "$PROC_MULT_FILES" is used (see
#       above).
#     IFILES_MAX_MULTn: the maximum number of new files on the remote machine
#       for which the files will be concatenated prior to processing when
#       switch $PROC_MULT_FILES for this family is set to "YES".  If greater
#       than $IFILES_MAX_MULTn new files are found on the remote machine for a
#       particular file family, $PROC_MULT_FILES will be reset to "NO" for this
#       family and files will be processed one at a time rather than after all
#       new files are first concatenated. (Applies only when $PROC_MULT_FILES
#       or $PROC_MULT_FILESn is "YES".)  Prevents the creation of a
#       concatenated file that is so big that it might greatly slow down
#       processing.  For a particular family "n", if "$IFILES_MAX_MULTn" does
#       not exist, then the global default value "$IFILES_MAX_MULT" is used
#       (see above).
#     mod_sec3_descN: if set to "NO", program bufr_tranmtypsbt (if it is run)
#       will not overwrite the top-level sequence descriptor in Sec. 3 of every
#       BUFR message with the read-in value from the external BUFR mnemonic
#       table, otherwise it will overwrite this.  (See Docblock in program
#       bufr_tranmtypsbt for more information.)  For a particular family "N",
#       if $mod_sec3_descN does not exist, then the global default value
#       $mod_sec3_desc is used (see above).
#     wget_network_timeoutN: connection or read timeout in seconds (applies
#       only when $TRANSFER_COMMAND or $TRANSFER_COMMANDn is "wget").  Applies
#       to both ingest_query.sh and ingest_get.sh.  For a particular family
#       "N", if wget_network_timeoutN does not exist, then the global default
#       value $wget_network_timeout used (see above).
#     SUBDATE_CHECKn: if set to "YES, program bufr_tranjb will unpack each
#       subset from the current input BUFR message and its date will be checked
#       in order to ensure that all BUFR messages opened for appending to the
#       output TANK file have a sec. 1 date (year, month, day, hour) that is
#       the same as each subset within the message.  If set to "NO", program
#       bufr_tranjb will append each input BUFR message to the output TANK
#       file, as is, and no date checking will be performed.  For a particular
#       family "n", if SUBDATE_CHECKn does not exist, then default is to not
#       set this variable here.  (See Docblock in ush script bufr_tranjb.sh for
#       more information.)
#     MESSAGE_LENGTHn: output BUFR message length upper limit (in bytes) in
#       program bufr_tranjb.  If set larger than the current BUFRLIB maximum
#       message length, it will override that setting.  There is technically no
#       minimum value for MESSAGE_LENGTH but it makes sense not to set this
#       less than 2500. For a particular family "n", if MESSAGE_LENGTHn does
#       not exist, then default is to not set this variable here.  In this case
#       it will be set to default BUFRLIB output message length upper limit in
#       bufr_tranjb.  (See Docblock in ush script bufr_tranjb.sh for more
#       information.)
#     MXMSGLn: Optional override of default BUFR message maximum length (in
#        bytes) for reads or writes in program bufr_tranjb (for family "n").
#
#     It should be noted that, for each family of files, the environmental
#       variables with a suffixed "n" or "N" (i.e., COPYFILES1, MACHINE1,
#       TRANSFER_COMMAND1, ORBITLIST1, REMOTEDIRGRP1, REMOTEDSNGRP1, FTYPE1,
#       log1, HISTLENMIN1, HISTLENMAX1, TANKFILE1, EXECUTE1, TARGETFILE1,
#       timetype1, user_spec_timedatecurr1, BUFRTABLE1, FORGNTABLE1,
#       HOURS2ALARM1, CRITICAL1, RENAME_FILE_SUFFIX1, ndayarch1, COMPRESS1,
#       UNCOMPRESS_UNIX1, PROCESS_Ta1, PROCESS_Tb1, ITRIES_MAX_QUERY1,
#       ITRIES_MAX_GET1, IFILES_MAX_GET1, PROC_MULT_FILES1, IFILES_MAX_MULT1,
#       mod_sec3_desc1, wget_network_timeout1, SUBDATE_CHECK1, MESSAGE_LENGTH1,
#       MXMSGL1) are assigned to variables without the "n" or "N" (i.e.,
#       COPYFILES, MACHINE, TRANSFER_COMMAND, ORBITLIST, REMOTEDIRGRP,
#       REMOTEDSNGRP, FTYPE, log, HISTLENMIN, HISTLENMAX, TANKFILE, EXECUTE,
#       TARGETFILE, timetype, user_spec_timedatecurr, BUFRTABLE, FORGNTABLE,
#       HOURS2ALARM, CRITICAL, RENAME_FILE_SUFFIX, ndayarch, COMPRESS,
#       UNCOMPRESS_UNIX, PROCESS_Ta, PROCESS_Tb, ITRIES_MAX_QUERY,
#       ITRIES_MAX_GET, IFILES_MAX_GET, PROC_MULT_FILES, IFILES_MAX_MULT,
#       mod_sec3_desc, wget_network_timeout, SUBDATE_CHECK, MESSAGE_LENGTH,
#       MXMSGL) and exported for use by the script
#       $USHobsproc_satingest/ingest_process_onetype_neworbits.sh.
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
msg="$DATATYPE PROCESSING FROM TIME-STAMPED FILES HAS BEGUN"
$UTILROOT/ush/postmsg "$jlogfile" "$msg"

########################################

ksh $USHobsproc_satingest/ingest_qmgr.sh
errsc=$?

cd $DATA

if [ $errsc -eq 99 ]; then
   msg="Another job with this name is in the system, this ingest job will \
continue but not ingest any satellite data"
   echo $msg
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
   exit $errsc
fi
cd $DATA

pwd
ls -ltr



##########################################

set +x
echo " "
echo "############################################################"
echo "         INGEST DATA FROM REMOTE UNIX MACHINE               "
echo "                 (TIME-STAMPED FILES)                       "
echo "############################################################"
echo " "
set -x

pgm='ingest_process_onetype_neworbits.sh'

set +u
. $UTILROOT/ush/prep_step
set -u

set +u
[ -n "$USERDIR" -a ! -d "$USERDIR" ] && mkdir -p $USERDIR
[ -n "$TANKDIR" -a ! -d "$TANKDIR" ] && mkdir -p $TANKDIR
[ -n "$OUTDIR"  -a ! -d "$OUTDIR"  ] && mkdir -p $OUTDIR
set -u

COPYFILES=${COPYFILES:-NO}
MACHINE=${MACHINE:-none}
TRANSFER_COMMAND=${TRANSFER_COMMAND:-ftp}
FTYPE=${FTYPE:-none}
log=${log:-$OUTDIR/tranjb.out}
HOURS2ALARM=${HOURS2ALARM:-30}
CRITICAL=${CRITICAL:-NO}
ndayarch=${ndayarch:-30}
UNCOMPRESS_UNIX=${UNCOMPRESS_UNIX:-NO}
HISTLENMIN=${HISTLENMIN:-900}
HISTLENMAX=${HISTLENMAX:-1200}
COMPRESS=${COMPRESS:-NO}
PROCESS_Ta=${PROCESS_Ta:-NO}
PROCESS_Tb=${PROCESS_Tb:-YES}
ITRIES_MAX_QUERY=${ITRIES_MAX_QUERY:-2}
ITRIES_MAX_GET=${ITRIES_MAX_GET:-2}
IFILES_MAX_GET=${IFILES_MAX_GET:-999}
PROC_MULT_FILES=${PROC_MULT_FILES:-NO}
IFILES_MAX_MULT=${IFILES_MAX_MULT:-100}
mod_sec3_desc=${mod_sec3_desc:-YES}
wget_network_timeout=${wget_network_timeout:-120}
TARGETFILE=${TARGETFILE:-NO}
timetype=${timetype:-UTC}
user_spec_timedatecurr=\
${user_spec_timedatecurr:-`date -u '+%Y%m%d %j %w %H%M'`}

FTYPE_save=$FTYPE
log_save=$log
HISTLENMIN_save=$HISTLENMIN
HISTLENMAX_save=$HISTLENMAX
COMPRESS_save=$COMPRESS
COPYFILES_save=$COPYFILES
MACHINE_save=$MACHINE
TRANSFER_COMMAND_save=$TRANSFER_COMMAND
HOURS2ALARM_save=$HOURS2ALARM
CRITICAL_save=$CRITICAL
ndayarch_save=$ndayarch
UNCOMPRESS_UNIX_save=$UNCOMPRESS_UNIX
PROCESS_Ta_save=$PROCESS_Ta
PROCESS_Tb_save=$PROCESS_Tb
ITRIES_MAX_QUERY_save=$ITRIES_MAX_QUERY
ITRIES_MAX_GET_save=$ITRIES_MAX_GET
IFILES_MAX_GET_save=$IFILES_MAX_GET
PROC_MULT_FILES_save=$PROC_MULT_FILES
IFILES_MAX_MULT_save=$IFILES_MAX_MULT
mod_sec3_desc_save=$mod_sec3_desc
wget_network_timeout_save=$wget_network_timeout
TARGETFILE_save=$TARGETFILE
timetype_save=$timetype
user_spec_timedatecurr_save=$user_spec_timedatecurr

igroup=0
errsave=0
lapseret=0

#  Process each group of data (family)
#  -----------------------------------

while [ $igroup -lt $ngroup ] ; do

  igroup=$(($igroup+1))

  msg="$pgm group $igroup has begun."
  $UTILROOT/ush/postmsg "$jlogfile" "$msg"

  set +x
  echo
  echo "####################################################################"
  echo "####################################################################"
  echo "              PROCESSING FOR GROUP $igroup FAMILY HAS BEGUN         "
  echo "####################################################################"
  echo "####################################################################"
  echo
  set -x

  eval ORBITLIST=$USERDIR/\$ORBITLIST$igroup
  eval COPYFILES=\${COPYFILES$igroup:-\$COPYFILES_save}
  eval MACHINE=\${MACHINE$igroup:-\$MACHINE_save}
  eval TRANSFER_COMMAND=\${TRANSFER_COMMAND$igroup:-\$TRANSFER_COMMAND_save}
  eval REMOTEDIRGRP=\${REMOTEDIRGRP$igroup:-.}
  eval REMOTEDSNGRP=\$REMOTEDSNGRP$igroup
  eval FTYPE=\${FTYPE$igroup:-\$FTYPE_save}
  eval log=\${log$igroup:-\$log_save}
  eval HISTLENMIN=\${HISTLENMIN$igroup:-\$HISTLENMIN_save}
  eval HISTLENMAX=\${HISTLENMAX$igroup:-\$HISTLENMAX_save}
  eval TANKFILE=\$TANKFILE$igroup
  eval EXECUTE=\${EXECUTE$igroup:-nullexec}
  eval BUFRTABLE=\${BUFRTABLE$igroup:-nobufrtable}
  eval FORGNTABLE=\${FORGNTABLE$igroup:-noforgntable}
  eval COMPRESS=\${COMPRESS$igroup:-\$COMPRESS_save}
  eval HOURS2ALARM=\${HOURS2ALARM$igroup:-$HOURS2ALARM_save}
  eval CRITICAL=\${CRITICAL$igroup:-$CRITICAL_save}
  eval RENAME_FILE_SUFFIX=\${RENAME_FILE_SUFFIX$igroup:-""}
  eval ndayarch=\${ndayarch$igroup:-$ndayarch_save}
  eval UNCOMPRESS_UNIX=\${UNCOMPRESS_UNIX$igroup:-$UNCOMPRESS_UNIX_save}
  eval PROCESS_Ta=\${PROCESS_Ta$igroup:-\$PROCESS_Ta_save}
  eval PROCESS_Tb=\${PROCESS_Tb$igroup:-\$PROCESS_Tb_save}
  eval ITRIES_MAX_QUERY=\${ITRIES_MAX_QUERY$igroup:-\$ITRIES_MAX_QUERY_save}
  eval ITRIES_MAX_GET=\${ITRIES_MAX_GET$igroup:-\$ITRIES_MAX_GET_save}
  eval IFILES_MAX_GET=\${IFILES_MAX_GET$igroup:-\$IFILES_MAX_GET_save}
  eval PROC_MULT_FILES=\${PROC_MULT_FILES$igroup:-\$PROC_MULT_FILES_save}
  eval IFILES_MAX_MULT=\${IFILES_MAX_MULT$igroup:-\$IFILES_MAX_MULT_save}
  eval mod_sec3_desc=\${mod_sec3_desc$igroup:-\$mod_sec3_desc_save}
  eval wget_network_timeout=\${wget_network_timeout$igroup:-\$wget_network_timeout_save}
  eval TARGETFILE=\${TARGETFILE$igroup:-\$TARGETFILE_save}
  eval timetype=\${timetype$igroup:-\$timetype_save}
  eval user_spec_timedatecurr=\
\${user_spec_timedatecurr$igroup:-\$user_spec_timedatecurr_save}
  eval SUBDATE_CHECK=\${SUBDATE_CHECK$igroup:-""}
  eval MESSAGE_LENGTH=\${MESSAGE_LENGTH$igroup:-""}
  eval MXMSGL=\${MXMSGL$igroup:-""}

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

  ksh $USHobsproc_satingest/ingest_process_onetype_neworbits.sh
  err=$?

  echo "error from ingest_process_onetype_neworbits.sh is " $err
  if [ $err -gt $errsave ] ; then
    errsave=$err
  fi

  ksh $USHobsproc_satingest/ingest_check_lapsed_data.sh $ORBITLIST.history

done

err=$errsave

cp $jlogfile jlogfile_local

grep -q "$jobid-No eligible executable or script named" jlogfile_local
err_grep=$?
[ $err_grep -eq 0 ]  &&  err=244

if [ $err -ne 0 ] 
then

#####################################################################
# ABNORMAL RUN
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


   if [ $err -ne 244 ]; then
      msg="$DATATYPE PROCESSING FROM TIME-STAMPED FILES HAS COMPLETED \
ABNORMALLY WITH R.C.=$err  --> non-fatal"
      set +x
      echo
      echo $msg
      echo $err
      echo
      set -x
      $UTILROOT/ush/postmsg "$jlogfile" "$msg"
   else
      msg="**FATAL ERROR: NO ELIGIBLE EXECUTABLE OR SCRIPT IN ONE OR MORE \
GROUPS (FAMILIES) IN $DATATYPE PROCESSING FROM TIME-STAMPED FILES - R.C.=$err"
      set +x
      echo
      echo $msg
      echo $err
      echo
      set -x
      $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      $UTILROOT/ush/err_exit
   fi

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


   msg="$DATATYPE PROCESSING FROM TIME-STAMPED FILES HAS COMPLETED NORMALLY."
   echo $msg
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"

fi

############## END OF SCRIPT #######################
