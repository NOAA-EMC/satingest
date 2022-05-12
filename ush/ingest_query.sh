#!/bin/ksh
# Run under ksh

####  UNIX Script Documentation Block
#
# Script name:   ingest_query.sh
#
# RFC contact:  Keyser               org: NP22        date: 2017-11-10
#
# Abstract: Determines the availability of a group of files on a remote unix
#  machine.  The file containing the list of available data sets is returned in
#  script parameter 2.
#
# Script history log:
# 2006-05-12  D. Keyser   Original version for implementation.  Combines/
#     generalizes previous scripts ingest_cemscsquery (for CEMSCS machine
#     only) and ingest_unixquery (for unix machines only).  Changed to account
#     for all remote machines now being unix (since MVS CEMSCS machine was
#     replaced with unix DDS machine).  Improved documentation and comments,
#     more appropriate messages posted to joblog.
# 2006-09-29  D. Keyser   If query fails on first attempt (for whatever reason)
#     now sleeps 30 sec and tries a second time, if this also fails script
#     gives up (allows query to bypass possible momentary ftp glitches).
# 2007-05-14  D. Keyser   Now uses imported script variable ITRIES_MAX_QUERY
#     to determine maximum number of failed attempts to query files from remote
#     machine via ftp before giving up (had been hardwired to "2").
# 2008-01-31  D. Keyser   Now treats embedded asterisk ("*") characters in
#     file group name as wildcards matching any string rather than as a
#     wildcard matching exactly 1 character per asterisk. Now treats question
#     mark ("?") characters in file group name as wildcards matching exactly 1
#     character.  If file group name contains 1 or more "*" or "?" characters,
#     then an asterisk is never placed at end of file group name when doing
#     query (i.e., files to be queried exactly match end of file group name).
#     Removed logic which used awk to extract filenames from ftp listing -
#     this is no longer needed now that MVS CEMSCS machine has been retired.
# 2010-06-24  P. O'Reilly Modified to remove section that creates special
#     .netrc for the gp16.ssd.nesdis.noaa.gov system. This system has been
#     retired.
# 2010-07-06  D. Keyser   Modified to pull files from same CCS machine via
#     sftp.  Will not expand filenames which include substitution characters
#     "?" and "*" since files being sftp'd are on same CCS machine as that in
#     which the job is running. Note: sftp will not work for pulling files from
#     other CCS machine.
# 2010-09-09  B. Katz     Modified to check to see if the lines returned by the
#     'ls' command include the full directory path (which is expected in down-
#     stream processing).  If not, adds the directory in front of the filename
#     while creating $DIRFILE.  Otherwise, it passes the lines through to
#     $DIRFILE unchanged. This is needed because, for the first time, the 'ls'
#     command returns only filenames when querying AMSR-E files on
#     machine  ftp.misst.org.  One caveat: this may not work if the ftp 'ls'
#     request includes a directory containing wildcard characters.  Only the
#     filename may contain such characters.
# 2012-11-14  D. Keyser   Modified to run on WCOSS.  Checks value of new
#     imported script variable TRANSFER_COMMAND {current choices are 'ftp'
#     (default), 'sftp' and the newest option 'wget'}, the 'wget' option now
#     requires additional logic to handle its structure which is quite
#     different from 'ftp' or 'sftp', will eventually add 'ftps' as an option.
#     WCOSS/Eddy currently does not support ftp, but it does support wget
#     (later WCOSS/Tide does support ftp for some queues).  This script is now
#     set to run under ksh shell as the default.  As in the case of the CCS,
#     sftp can be used only to pull files from same WCOSS machine (will not
#     work for pulling files from other WCOSS machine).
# 2012-11-14  S. Melchior  Modified to properly parse filename listing returned
#     from ftp "ls" lister.  ftp initiated from WCOSS/Tide machine returns more
#     verbose meta information in a listing ahead of the filename (e.g. perms,
#     mod time, etc).  Also removed "passive" ftp subcommand (which toggles
#     passive mode on/off) and replaced it with "-p" ftp command line option
#     (which forces passive mode on); needed since ftp from WCOSS/Tide machine
#     defaults to passive on while ftp from CCS machines defaults to passive
#     off (this ensures passive mode is on for both WCOSS and CCS).
# 2013-05-09  C. Klemmer  Increase the network timeout to 120 seconds
#     due to recent delays connecting to a NESDIS server.
# 2013-05-14  D. Stokes   Added TRANSFER_COMMAND option lftp which supports 
#     the ftps protocol needed to access the NDE server.  Made wget options
#     variable.
# 2014-01-03  D. Keyser   Renamed to add suffix .sh qualifier.  Now includes
#     hostname as well as process id in temporary filenames where only process
#     id was present before.  Changed all "date" commands to "date -u" since
#     WCOSS should always present date in UTC.  Added information to docblock
#     and new comments.  Updated some existing comments.
# 2014-09-04  D. Keyser    Imported variable wget_network_timeout (connection
#     or read timeout in seconds) must now be passed in (before it had a
#     default of 120).  This variable is now set in either the upstream job or
#     model script.
# 2017-11-10  D. Keyser   New imported variable REMOTEDIRGRP {directory path to
#     REMOTEDSNGRP (then defined as file names only), if not set to default of
#     '.' (current directory)}. Invoked only when TRANSFER_COMMAND is "ftp" or
#     "wget".  REMOTEDIRGRP must begin with "/".
#     BENEFIT: Allows two different file families pulling the same file names
#              from two different servers (e.g., a primary and backup) in two
#              different directories to store file listings in same history
#              files.
# 2022-01-18  S. Stegall  Replaced $DATA/ before calling utility scripts and instead 
#      used $UTILROOT/ush/ to properly leverage the prod_util module.
#
#
# Usage: ingest_query.sh <remote_machine> <directory_listing_file> <file_group>
#
#   Script parameters: $1 - name of remote unix machine to be used in transfer
#                           requests
#                      $2 - directory_listing_file (output: path to file
#                           containing listing of all available files on remote
#                           unix machine)
#                      $3 - file_group (partial remote filename to look for)
#
#   Modules and files referenced:
#     scripts    : $UTILROOT/ush/postmsg
#     data cards : none
#     executables: none
#
# Remarks: Invoked by the ush script ingest_process_onetype_neworbits.sh.
#
#   Imported Variables that must be passed in:
#      DATA                 - path to current working directory
#      jlogfile             - path to joblog file
#      REMOTEDIRGRP         - the directory path to REMOTEDSNGRP, which is then
#                             defined as the files containing the leading
#                             portion of the name of a family of files from the
#                             remote unix machine (invoked only when
#                             TRANSFER_COMMAND is imported as "ftp" or "wget") -
#                             REMOTEDIRGRP must begin with "/" - if not set,  it
#                             defaults to "." (current directory) and
#                             REMOTEDSNGRP is expected to contain the complete
#                             path to the leading portion of the name of a
#                             family of files from the remote unix machine
#      REMOTEDSNGRP         - the leading portion of the name of a family of
#                             files from the remote unix machine (normally this
#                             is defined as the complete path to these files,
#                             however if REMOTEDIRGRP is set, it is then defined
#                             as only the files themselves)
#      TRANSFER_COMMAND     - type of transfer to be done (e.g., 'ftp', 'sftp',
#                             'lftp', 'wget')
#      MACHINE              - name of remote unix machine to be used in
#                             transfer requests
#      ITRIES_MAX_QUERY     - the maximum number of failed attempts to query
#                             files on the remote machine before giving up
#      wget_network_timeout - connection or read timeout in seconds (applies
#                             only when $TRANSFER_COMMAND is "wget")
#
#   Imported Variables that can be passed in:
#      DEBUGSCRIPTS         - if set to "ON" or "YES", will run with "set -x"
#                             on (intended for debugging)
#                             (default = 'OFF')
#      wget_tries           - number of times to try to connect (note there are
#                             no retries for fatal errors) (applies only when
#                             $TRANSFER_COMMAND is "wget")
#                             (default = '1')
#      lftp_dns_timeout     - time limit for DSN queries in seconds (applies
#                             only when $TRANSFER_COMMAND is "lftp")
#                             (default = '120')
#      lftp_recon_int_base  - base minimal time between reconnects in seconds
#                             (applies only when $TRANSFER_COMMAND is "lftp")
#                             (default = '120')
#      lftp_recon_int_mult  - multiplier by which lftp_recon_int_base is
#                             multiplied each time a new attempt to reconnect
#                             occurs (applies only when $TRANSFER_COMMAND is
#                             "lftp")
#                             (default = '-1')
#      lftp_max_tries       - number of times to try to connect (applies only
#                             when $TRANSFER_COMMAND is "lftp")
#                             (default = '1')
#
#   Condition codes:
#     0 - no problem encountered
#   > 0 - some problem encountered
#     Specifically:   1 - Query of file(s) failed
#
# Attributes:
#   Language: ksh script
#   Machine:  NCEP WCOSS
#
####

set -au

echo
echo "#######################################################################"
echo "                      START INGEST_QUERY                               "
echo "#######################################################################"
echo

DEBUGSCRIPTS=${DEBUGSCRIPTS:-OFF}
if [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ] ; then
   set -x
fi

host=$(hostname -s)

MACHINE=$1
DIRFILE="$2"
fname=$3

#  If the file group name contains one or more embedded asterisks ("*" -
#   wildcard matching any string of 1 or more characters) or one or more
#   question marks ("?" -  wildcard matching exactly 1 character ), then DON'T
#   put an asterisk at the end of the file group name (i.e., the query will
#   look only for files whose ending characters exactly match the file group
#   name ending characters, including "?"'s).
#    The file group name may have "?" as the last character but should never
#     have "*" as the last character.
#  ---------------------------------------------------------------------------

echo "$REMOTEDSNGRP" | grep -Fe "*" -Fe "?"
iret=$?

if [ $iret -eq 0 ]; then
   REMOTEDSNGRP="$fname"
else
   REMOTEDSNGRP="$fname*"
fi

transout=$DATA/transout.$host.$$


#  Get a listing of REMOTEDSNGRP files from the remote unix machine using
#    $TRANSFER_COMMAND
#  ----------------------------------------------------------------------

set +x
echo
echo "Time is now $(date -u)."
echo
[ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x


#  Based on transfer mechanism, set up file transfer instructions
#  --------------------------------------------------------------

if [ $TRANSFER_COMMAND = wget ]; then
   wget_tries=${wget_tries:-1}  # number of times to try to connect
                                # (note there are no retries for fatal errors)
   transfer_options="--dont-remove-listing -T ${wget_network_timeout} -t ${wget_tries}"
# Change all "?" values to "." in $REMOTEDSNGRP so later grep works properly
   remotedsngrp=$(echo "$REMOTEDSNGRP" | sed "s/?/./g")
   if [ $REMOTEDIRGRP != '.' ]; then
      dir=$REMOTEDIRGRP
      fil=$remotedsngrp
   else
# Account for dds files which may not have "/" in first char. of $REMOTEDSNGRP
      [ `echo $remotedsngrp | cut -c1` != '/' ]  &&  remotedsngrp=/$remotedsngrp
      dir=`dirname $remotedsngrp`/
      fil=`basename $remotedsngrp`
   fi
   machine=ftp://${MACHINE}${dir}
   > $DATA/transquery.input.$host.$$

elif [ $TRANSFER_COMMAND = sftp ]; then
   transfer_options=-v
   remotedsngrp=$REMOTEDSNGRP
   machine=$MACHINE
   cat <<EOH_trans_sftp > $DATA/transquery.input.$host.$$
lls $REMOTEDSNGRP > $DATA/transquery.output.$host.$$
quit
EOH_trans_sftp

elif [ $TRANSFER_COMMAND = lftp ]; then
   transfer_options=""
   remotedsngrp=$REMOTEDSNGRP
   machine=$MACHINE  
   cat <<EOH_trans_lftp > $DATA/transquery.input.$host.$$
debug 3
set ssl:check-hostname no
set dns:fatal-timeout ${lftp_dns_timeout:-120}
set net:reconnect-interval-base ${lftp_recon_int_base:-120}   
set net:reconnect-interval-multiplier ${lftp_recon_int_mult:-1} 
set net:max-retries ${lftp_max_tries:-1}
set net:timeout ${lftp_timeout:-120}
rels $REMOTEDSNGRP > $DATA/transquery.output.$host.$$
quit
EOH_trans_lftp
# despite setting name net:max-retries, seems to be "tries", not "retries"

else
   transfer_options="-vi -p"
   remotedsngrp=$REMOTEDSNGRP
   machine=$MACHINE
   cat <<EOH_trans > $DATA/transquery.input.$host.$$
cd $REMOTEDIRGRP
ls $REMOTEDSNGRP $DATA/transquery.output.$host.$$
quit
EOH_trans

fi


transerror=99
itries=1
while [ $transerror -gt 0 -a $itries -le $ITRIES_MAX_QUERY ]; do
   [ -s $DATA/transquery.output.$host.$$ ]  &&  \
    rm $DATA/transquery.output.$host.$$
   if [ $itries -gt 1 ]; then
      msg="QUERY OF $3 FILES FAILED!!!! - SLEEP 30 SEC AND TRY AGAIN."
      $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      sleep 30
   fi

   echo
   echo "Use $TRANSFER_COMMAND."
   echo
   $TRANSFER_COMMAND $transfer_options $machine < \
    $DATA/transquery.input.$host.$$ > $transout 2>&1
   transerror=$?

#  Cat out the standard output from the transfer process and remove it
#  -------------------------------------------------------------------

   set +x
   echo
   cat $transout
   echo
   [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x

   rm $transout

   if [ $TRANSFER_COMMAND = wget ]; then

#  wget listing contains full "ls -l" type listing (e.g., perms, mod time, etc)
#   of ALL files in directory but with no directory path on file names in last
#   field (column) of listing, and contains <esc>M at the end of every line
#   It is output to $DATA/.listing.  Need to do several things here to obtain
#   listing containing one field (column) of requested (and only requested)
#   file names:
#      1) grep file name pattern out of $DATA/.listing
#      2) remove <esc>M from end of every line of output resulting from 1
#         (Note: On WCOSS <esc>M will appear only when using "cat -v" command,
#                it does not appear when using "cat" or "vim" commands; on CCS
#                <esc>M will appear when using "cat -v" and "vi" commands but
#                not when using "cat" command)
#      3) retrieve last (usually 9'th) field from output resulting from 2
#      4) grep only lines beginning with file name pattern and ending with
#         file name pattern from output resulting from 3
#   Note: Full directory path will be added back on later - UNLESS $REMOTEDIRGRP
#         is set (i.e., not the default of '.').
#  -----------------------------------------------------------------------------

      grep $fil $DATA/.listing | sed 's/
$//' | cat | \
       awk -F" " '{print $NF}' | grep ^$fil\$ > \
       $DATA/transquery.output.$host.$$
      rm $DATA/.listing
      rm $DATA/index.html*

   elif [ $TRANSFER_COMMAND = ftp -o $TRANSFER_COMMAND = lftp ]; then

#  On WCOSS/Tide, ftp "ls" listing contains full "ls -l" type listing (e.g.,
#   perms, mod time, etc) but with no directory path on file names.
#   Furthermore, on some machines linked file names will be followed by
#   pointers to their target (full path) file names.  This is different than on
#   CCS where ftp "ls" listing contains only file names (with full directory
#   paths) and never any links. Need to do several things here to obtain
#   listing containing one field (column) of requested file names without any
#   links:
#      1) retrieve only the information on each line prior to any soft link
#         target (if present) via awk delimiter " ->"
#      2) retrieve last (usually 9'th) field from output resulting from 1
#   Note 1: If only one field is in original listing, as on CCS, these actions
#           will simply return the same information as input.
#   Note 2: For WCOSS case, full directory path will be added back on later -
#           UNLESS $REMOTEDIRGRP is set (i.e., not the default of '.').
#  ----------------------------------------------------------------------------

      cat $DATA/transquery.output.$host.$$ | awk -F" ->" '{print$1}' | \
       awk -F" " '{print $NF}' > $DATA/transquery.testoutput.$host.$$
      mv $DATA/transquery.testoutput.$host.$$ $DATA/transquery.output.$host.$$
   fi

   [ ! -s $DATA/transquery.output.$host.$$ ]  &&  transerror=1
   itries=`expr $itries + 1`
done
itries=`expr $itries - 1`

set +x
echo
echo "Time is now $(date -u)."
echo
[ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x

#  If there was an error in the transfer (including if no listing was produced)
#   then exit w/ return code 1
#  ----------------------------------------------------------------------------

if [ $transerror -ne 0 ]; then
   [ -s $DATA/transquery.output.$host.$$ ]  &&  \
    rm $DATA/transquery.output.$host.$$
   [ -s $DATA/transquery.input.$host.$$ ] && rm $DATA/transquery.input.$host.$$
   msg="Exiting with rc = 1 - query of $3 files on remote unix machine \
$MACHINE failed after $itries tries --> non-fatal"
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
   set +x
   echo
   echo " Query of file group $3 on remote unix machine $MACHINE failed \
after $itries tries. "
   echo
   exit 1
fi

#  Continue on if no transfer problems
#  -----------------------------------

msg="QUERY OF $3 FILES successful on try no. ${itries}."
$UTILROOT/ush/postmsg "$jlogfile" "$msg"

#  Check to see if the lines returned by the 'ls' command include the full
#   directory path - if not add the directory in front of the filename here
#   while creating $DIRFILE -- UNLESS $REMOTEDIRGRP is set (i.e., not the
#   default of '.'), in which case only the filenames are used in the creation
#   of $DIRFILE.
#  ---------------------------------------------------------------------------

echo `head -n1 $DATA/transquery.output.$host.$$` | grep /
err_grep=$?

if [ $err_grep -eq 0 -o $REMOTEDIRGRP != '.' ]; then
   cp $DATA/transquery.output.$host.$$ $DIRFILE
else
   direct=$(dirname $REMOTEDSNGRP)

   cat $DATA/transquery.output.$host.$$ | {
      read directfilename
      iret=$?
      set +x   # too much printout here when lots of files!
      while (( $iret == 0 )) ; do
         filename=$(basename $directfilename)
         if [[ $filename = $directfilename ]] ; then
            echo ${direct}/$filename
         else
            echo $directfilename
         fi
         read directfilename
         iret=$?
      done } > $DIRFILE
      set -x
fi

rm $DATA/transquery.output.$host.$$
rm $DATA/transquery.input.$host.$$

exit 
