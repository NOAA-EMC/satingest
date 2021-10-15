#!/bin/ksh
# Run under ksh

####  UNIX Script Documentation Block
#
# Script name:   ingest_get.sh
#
# RFC contact:  Keyser               org: NP22        date: 2017-11-10
#
# Abstract: Transfers a file from a remote unix machine to the NCEP WCOSS.  The
#   transfer involves no data conversion.
#
# Script history log:
# 2006-05-12  D. Keyser   Original version for implementation.  Combines/
#     generalizes previous scripts ingest_cemscsget (for CEMSCS machine only)
#     and ingest_unixget (for unix machines only), and changed to account for
#     all remote machines now being unix (since MVS CEMSCS machine was replaced
#     with unix DDS machine).  Improved documentation and comments, more
#     appropriate messages posted to joblog.
# 2007-06-12  D. Keyser   Modified to perform a "passive" ftp to the remote
#     machine.  Instead of the client (the CCS) initiating a connection, then
#     the server (the remote machine) initiating a second, the client initiates
#     both connections.  This corrects the "socket error" which had been
#     occurring in about 1 in 500 ftp attempts to NESDIS' DDS machine.
# 2007-10-24  D. Keyser   Modified to add the remote and local file names on
#     the same line as the "get" command in the ftp logic.  The previous form,
#     with "get", remote file and local file on separate lines, no longer works
#     on the NCEP CCS Dew machine due to a change in the ftp client software.
# 2010-06-24  P. O'Reilly Modified to remove section that creates special
#     .netrc for the gp16.ssd.nesdis.noaa.gov system. This system has been
#     retired.
# 2010-07-06  D. Keyser   Modified to pull files from same CCS machine via
#     sftp.  Note: sftp will not work for pulling files from other CCS machine.
# 2012-11-14  D. Keyser   Modified to run on WCOSS.  Checks value of new
#     imported script variable TRANSFER_COMMAND {current choices are 'ftp'
#     (default), 'sftp' and the newest option 'wget'}, the 'wget' option now
#     requires additional logic to handle its structure which is quite
#     different from 'ftp' or 'sftp', will eventually add 'ftps' as an option.
#     WCOSS/Eddy currently does not support ftp, but it does support wget
#     (later WCOSS/Tide does support ftp for some queues).  This script is now
#     set to run under ksh shell as the default. As in the case of the CCS,
#     sftp can be used only to pull files from same WCOSS machine (will not
#     work for pulling files from other WCOSS machine).
# 2012-11-14  S. Melchior  Removed "passive" ftp subcommand (which toggles
#     passive mode on/off) and replaced it with "-p" ftp command line option
#     (which forces passive mode on); needed since ftp from WCOSS/Tide machine
#     defaults to passive on while ftp from CCS machines defaults to passive
#     off (this ensures passive mode is on for both WCOSS and CCS).
# 2013-05-09  C. Klemmer  Increase the network timeout from 15 to 120 seconds
#     due to recent delays connecting to a NESDIS server.
# 2013-05-14  D. Stokes   Added TRANSFER_COMMAND option lftp which supports
#     the ftps protocol needed to access the NDE server.  Made wget options
#     variable.
# 2014-01-03  D. Keyser    Renamed to add suffix .sh qualifier.  Now includes
#     hostname as well as process id in temporary filenames where only process
#     id was present before.  Changed all "date" commands to "date -u" since
#     WCOSS should always present date in UTC.  Added information to docblock
#     and new comments.  Updated some existing comments.
# 2014-09-04  D. Keyser    Imported variable wget_network_timeout (connection
#     or read timeout in seconds) must now be passed in (before it had a
#     default of 120).  This variable is now set in either the upstream job or
#     model script.
# 2017-11-10  D. Keyser   New imported variable REMOTEDIRGRP {directory path to
#     remote_file (then defined as file names only), if not set to default of
#     '.' (current directory)}. Invoked only when TRANSFER_COMMAND is "ftp" or
#     "wget".  REMOTEDIRGRP must begin with "/".
#     BENEFIT: Allows two different file families pulling the same file names
#              from two different servers (e.g., a primary and backup) in two
#              different directories to store file listings in same history
#              files.
#
#
# Usage: ingest_get.sh  <remote_machine>  <local_file>  <remote_file>
#
#   Script parameters:
#                 $1: remote_machine - name of remote unix machine to be used
#                                      in transfer requests
#                 $2: local_file     - local file name (usually full path)
#                 $3: remote_file    - remote file name (usually full path, but
#                                      may only be the file name itself, see
#                                      REMOTEDIRGRP below)
#
#   Modules and files referenced:
#     scripts    : none
#     data cards : none
#     executables: none
#
# Remarks: Invoked by the ush scripts ingest_process_days.sh and
#   ingest_process_orbits.sh.
#
#   Imported Variables that must be passed in:
#      DATA                 - path to current working directory
#      TRANSFER_COMMAND     - type of transfer to be done (e.g., 'ftp', 'sftp',
#                            'lftp', 'wget')
#      wget_network_timeout - connection or read timeout in seconds (applies
#                             only when $TRANSFER_COMMAND is "wget")
#      REMOTEDIRGRP         - the directory path to remote_file read in script
#                             parameter 3, which is then defined as just a
#                             file name on the remote unix machine (invoked only
#                             when TRANSFER_COMMAND is imported as "ftp" or
#                             "wget") - REMOTEDIRGRP must begin with "/" - if
#                             not set,  it defaults to "." (current directory)
#                             and remote_file is expected to contain the
#                             complete path to the file on the remote unix
#                             machine
#
#   Imported Variables that can be passed in:
#      DEBUGSCRIPTS         - if set to "ON" or "YES", will run with "set -x"
#                             on (intended for debugging)
#                             (default = 'OFF')
#      wget_tries           - number of times to try to connect (note there are
#                             no retries for fatal errors) (applies only when
#                             $TRANSFER_COMMAND is "wget")
#                             (default = '2')
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
#                             (default = '2')
#
#   Condition codes:
#     0 - no problem encountered
#   > 0 - some problem encountered
#     Specifically:   1 - Transfer of file failed
#
# Attributes:
#   Language: ksh script
#   Machine:  NCEP WCOSS
#
####

set -au

echo
echo "#######################################################################"
echo "                       START INGEST_GET                                "
echo "#######################################################################"
echo

DEBUGSCRIPTS=${DEBUGSCRIPTS:-OFF}
if [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ] ; then
   set -x
fi

if [ $# -ne 3 ] ; then
   echo "Arguments to ingest_get.sh:"
   echo "  (1) remote_machine"
   echo "  (2) local_file"
   echo "  (3) remote_file"
   exit 0
fi

host=$(hostname -s)

MACHINE="$1"
LOCDSN="$2"
REMOTEDSN="$3"
transout=$DATA/transout.$host.$$

if [ $TRANSFER_COMMAND = wget ]; then
   WGET_PROTOCOL=${WGET_PROTOCOL:-ftp}
   wget_tries=${wget_tries:-2}  # number of times to try to connect
                                # (note there are no retries for fatal errors)
  transfer_options="-nv -T ${wget_network_timeout} -t ${wget_tries} -O $LOCDSN"
   if [ $REMOTEDIRGRP != '.' ]; then
      machine=${WGET_PROTOCOL}://${MACHINE}${REMOTEDIRGRP}${REMOTEDSN}
   else
      machine=${WGET_PROTOCOL}://${MACHINE}${REMOTEDSN}
# Account for dds files which may not have "/" in first char. of $REMOTEDSN
      [ `echo $REMOTEDSN | cut -c1` != '/' ]  &&  \
       machine=${WGET_PROTOCOL}://${MACHINE}/${REMOTEDSN}
   fi
   > $DATA/transget.input.$host.$$

elif [ $TRANSFER_COMMAND = sftp ]; then
   transfer_options=-v
   machine=$MACHINE
   cat <<EOH_trans_sftp > $DATA/transget.input.$host.$$
get $REMOTEDSN $LOCDSN
quit
EOH_trans_sftp

elif [ $TRANSFER_COMMAND = lftp ]; then
   transfer_options=""
   machine=$MACHINE
   cat <<EOH_trans_lftp > $DATA/transget.input.$host.$$
debug 3
set ssl:check-hostname no
set dns:fatal-timeout ${lftp_dns_timeout:-120}
set net:reconnect-interval-base ${lftp_recon_int_base:-120}
set net:reconnect-interval-multiplier ${lftp_recon_int_mult:-1}
set net:max-retries ${lftp_max_tries:-2}
set net:timeout ${lftp_timeout:-120}
set xfer:log no
set xfer:clobber yes
get $REMOTEDSN -o $LOCDSN
bye
EOH_trans_lftp
# despite setting name net:max-retries, seems to be "tries", not "retries"

else
   transfer_options="-vi -p"
   machine=$MACHINE
   cat <<EOH_trans > $DATA/transget.input.$host.$$
binary
cd $REMOTEDIRGRP
get $REMOTEDSN $LOCDSN
quit
EOH_trans

fi


echo
echo "Use $TRANSFER_COMMAND."
echo
$TRANSFER_COMMAND $transfer_options $machine < $DATA/transget.input.$host.$$ \
 >$transout 2>&1
transerror=$?
rm $DATA/transget.input.$host.$$

#  Cat out the standard output from the transfer process and remove it
#  -------------------------------------------------------------------

set +x
echo
echo "Time is now $(date -u)."
echo
grep "transfer aborted" $transout
errgrep=$?
if [ $errgrep -eq 0 -a $TRANSFER_COMMAND = wget ]; then
   grep -Fe "' saved [" $transout
   errgrep1=$?
   [ $errgrep1 -eq 0 ]  &&  errgrep=1
fi
cat $transout
echo
[ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x

rm $transout

[ $TRANSFER_COMMAND = wget ]  &&  rm $DATA/index.html*

if [ $transerror -ne 0 ] || [ ! -s $LOCDSN ] || [ $errgrep -eq 0 ]; then
   echo " Transfer of file $REMOTEDSN from $MACHINE failed."
   exit 1
fi
exit 0
