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
# 2025-12-03 S.Nadiga added functionality to access files from AWS and local disks
#      localdiskcp refers to copying from local disks and syncaws refers to AWS
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

echo " IN INGEST_QUERY; STATION 1 "
echo " MACHINE is $MACHINE and  DIRFILE is $DIRFILE ; and fname is $fname "

########### CLEAN OLD FILES IN DIRECTORIES ##############
########### CLEAN OLD FILES IN DIRECTORIES ##############
########### CLEAN OLD FILES IN DIRECTORIES ##############

   if [ $TRANSFER_COMMAND = localdiskcp -o $TRANSFER_COMMAND = syncaws ] ; then
mkdir -p $PTMPDIR/STAGEFILES
mkdir -p $PTMPDIR/DISCARD
mkdir -p $STMPDIR/FORINGEST
	   IFILES_MAX_SAFE=$(( $IFILES_MAX_GET * 60 / 100 ))
	   IFILES_MIN_SAFE=$(( $IFILES_MAX_GET * 20 / 100 ))
	   IFILES_MIN_SYNC=$(( $IFILES_MAX_GET * 5 / 100 ))
	   echo " CALCULATION 2001; IFILES_MAX_GET IS $IFILES_MAX_GET and IFILES_MIN_SYNC IS $IFILES_MIN_SYNC AND IFILES_MAX_SAFE is $IFILES_MAX_SAFE "

find $PTMPDIR/STAGEFILES/* -ctime +1 -print
find $PTMPDIR/STAGEFILES/* -ctime +1 -exec rm -r -f {} \;
find $PTMPDIR/STAGEFILES/* -ctime +1 -print

find $PTMPDIR/DISCARD/* -ctime +1 -print
find $PTMPDIR/DISCARD/* -ctime +1 -exec rm -r -f {} \;
find $PTMPDIR/DISCARD/* -ctime +1 -print


   fi

########### FINISHED CLEANING OLD FILES IN DIRECTORIES ###############
########### FINISHED CLEANING OLD FILES IN DIRECTORIES ###############
########### FINISHED CLEANING OLD FILES IN DIRECTORIES ###############

   if [ $TRANSFER_COMMAND = localdiskcp ] ; then
	  
	  echo " FILES FROM LOCALDISK and TASK = $TASK "
	 
	 if [ $TASK = 't-radsnd_lgycld' ] ; then  
	   SOURCE_DIR=/lfs/h1/ops/prod/dcom/nasa_clouds/
	   echo " TASK = $TASK and SOURCE_DIR is $SOURCE_DIR "
	  elif [ $TASK = 't_metop-sga1' ] ; then 
	   SOURCE_DIR=/lfs/h2/emc/obsproc/noscrub/steve.stegall/DCOMDIR/METOPSGA1/data1/smcd1/Metop-SGA1/
	   echo " TASK = $TASK and SOURCE_DIR is $SOURCE_DIR "
	  elif [ $TASK = 't_mws_metop-sga1' ] ; then
	   SOURCE_DIR=/lfs/h2/emc/obsproc/noscrub/steve.stegall/DCOMDIR/MWS_METOPSGA1/data/smcd1/MWS_Metop-SGA1/
	   echo " TASK = $TASK and SOURCE_DIR is $SOURCE_DIR "
	 else
	  echo " FATAL ERROR; TRANSFER_COMMAND IS $TRANSFER_COMMAND ; FILES NOT FOUND FOR TASK = $TASK "
	  echo " FATAL ERROR; TRANSFER_COMMAND IS $TRANSFER_COMMAND ; FILES NOT FOUND FOR TASK = $TASK "
	  echo " FATAL ERROR; TRANSFER_COMMAND IS $TRANSFER_COMMAND ; FILES NOT FOUND FOR TASK = $TASK "
	  echo " FATAL ERROR; TRANSFER_COMMAND IS $TRANSFER_COMMAND ; FILES NOT FOUND FOR TASK = $TASK "
	  exit
         fi
	   cd $SOURCE_DIR
	   numb_files=`find . -maxdepth 1 -type f -print0 | xargs -0 ls | wc -l`
	   if [ $numb_files -ge 1 ] ; then

find . -type f -mtime -1 -print0 | rsync -0av --files-from=- ./ $PTMPDIR/STAGEFILES

	   fi

#           /usr/bin/rsync -avh $SOURCE_DIR/*.* $PTMPDIR/STAGEFILES


   fi

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


echo " INGEST QUERY ; STATION 2 and  REMOTEDSNGRP is $REMOTEDSNGRP "
if [ -s $DATA/transout.$host.$$ ] ; then 
ls -l $DATA/transout.$host.$$
cp  $DATA/transout.$host.$$ $DATA/outtrans1.$host.$$
fi
ls -l $DATA >> $DATA/AOUT_INGESTQUERY
echo " INGEST_QUERY FINISHED STATION 2 "


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

elif [ $TRANSFER_COMMAND = localdiskcp ]; then
####### CHECK IF THERE ARE FILES THAT WE NEED TO COPY OVER
#	numb_files=`find . -maxdepth 1 -type f -print0 | xargs -0 ls | wc -l`

	cd $STMPDIR/FORINGEST
	numb_STAGEAFTERCP=0
	numb_files=0
	if [ $numb_files -ge $IFILES_MIN_SAFE ] ; then 
		echo "NO NEED TO COPY OVER; The number of files available to ingest is $numb_files  for $TRANSFER_COMMAND and IFILES_MIN_SAFE IS $IFILES_MIN_SAFE "
        else 
		echo "YES, NEED TO COPY OVER; The number of files available to ingest is $numb_files  for $TRANSFER_COMMAND and IFILES_MIN_SAFE IS $IFILES_MIN_SAFE "
#### Copy over from $PTMPDIR/STAGEFILES to fill the gap
		numb_files_cp=`expr $IFILES_MIN_SAFE - $numb_files`
		echo "numb_files is $numb_files and IFILES_MIN_SAFE is $IFILES_MIN_SAFE and numb_files_cp is $numb_files_cp"

## REMOVE FROM DIRECTORY A THE FILES ALREADY IN DIRECTORY B. FOR US DIRECTORY A=$PTMPDIR/STAGEFILES and DIRECTORY B=$PTMPDIR/DISCARD
#    ls -1 /path/to/DirectoryB | xargs -I {} rm -f "/path/to/DirectoryA/{}"
#

	cd $PTMPDIR/STAGEFILES	
	        numb_filesSTAGE=`find . -maxdepth 1 -type f -print0 | xargs -0 ls | wc -l`
	echo " numb_filesSTAGE is $numb_filesSTAGE numb_files_cp is $numb_files_cp  numb_files is $numb_files and IFILES_MIN_SAFE is $IFILES_MIN_SAFE "
	if  [ $numb_filesSTAGE -lt $numb_files_cp ] ; then numb_files_cp=$numb_filesSTAGE ; fi
	if [ $numb_files_cp -ge 1 ] ; then 
		echo " numb_files_cp is now $numb_files_cp and we are moving files to $STMPDIR/FORINGEST " 
# Find the latest files and move this many [ $numb_files_cp ]

cd $PTMPDIR/STAGEFILES
ls -t $PTMPDIR/STAGEFILES/*.* | head -n $numb_files_cp | tr '\n' '\0' | xargs -0 mv -t $STMPDIR/FORINGEST
#find . -maxdepth 1 -type f | head -n $numb_files_cp | xargs -I {} mv {} $STMPDIR/FORINGEST 

	numb_STAGEAFTERCP=`find . -maxdepth 1 -type f -print0 | xargs -0 ls | wc -l`
	echo " BEFORE AND AFTER COPYING; CALCULATION 3001  $PTMPDIR/STAGEFILES HAD $numb_filesSTAGE files and now has $numb_STAGEAFTERCP files "
	echo " $PTMPDIR/STAGEFILES HAD $numb_filesSTAGE files and now has $numb_STAGEAFTERCP files "
	cd $STMPDIR/FORINGEST
	numb_files=`find . -maxdepth 1 -type f -print0 | xargs -0 ls | wc -l`
	echo " FINAL NUMBER OF FILES TO INGEST IS $numb_files "
	cd $DATA
	fi
	fi

##      CHECK IF MAX ALLOWED NUMBER OF FILES IS BREACHED 
	cd $STMPDIR/FORINGEST
	numb_files=`find . -maxdepth 1 -type f -print0 | xargs -0 ls | wc -l`
	if [ $numb_files -gt $IFILES_MAX_SAFE ] ; then
		echo " DIAGNOSE 1022; NUMBER OF FILES IN $STMPDIR/FORINGEST EXCEEDED $IFILES_MAX_SAFE "
		echo " DIAGNOSE 1022; NUMB OF FILES IS $numb_files and MAX ALLOWED IS $IFILES_MAX_SAFE "
	fi


elif [ $TRANSFER_COMMAND = syncaws ]; then
####### CHECK IF THERE ARE FILES THAT WE NEED TO COPY OVER
#	numb_files=`find . -maxdepth 1 -type f -print0 | xargs -0 ls | wc -l`

	cd $STMPDIR/FORINGEST
	numb_STAGEAFTERCP=0
	numb_files=0
	if [ $numb_files -ge $IFILES_MIN_SAFE ] ; then 
		echo "NO NEED TO COPY OVER; The number of files available to ingest is $numb_files  for $TRANSFER_COMMAND and IFILES_MIN_SAFE IS $IFILES_MIN_SAFE "
        else 
		echo "YES, NEED TO COPY OVER; The number of files available to ingest is $numb_files  for $TRANSFER_COMMAND and IFILES_MIN_SAFE IS $IFILES_MIN_SAFE "
#### Copy over from $PTMPDIR/STAGEFILES to fill the gap
		numb_files_cp=`expr $IFILES_MIN_SAFE - $numb_files`
		echo "numb_files is $numb_files and IFILES_MIN_SAFE is $IFILES_MIN_SAFE and numb_files_cp is $numb_files_cp"

## REMOVE FROM DIRECTORY A THE FILES ALREADY IN DIRECTORY B. FOR US DIRECTORY A=$PTMPDIR/STAGEFILES and DIRECTORY B=$PTMPDIR/DISCARD
#    ls -1 /path/to/DirectoryB | xargs -I {} rm -f "/path/to/DirectoryA/{}"
#

	cd $PTMPDIR/STAGEFILES	
	        numb_filesSTAGE=`find . -maxdepth 1 -type f -print0 | xargs -0 ls | wc -l`
	echo " numb_filesSTAGE is $numb_filesSTAGE numb_files_cp is $numb_files_cp  numb_files is $numb_files and IFILES_MIN_SAFE is $IFILES_MIN_SAFE "
	if  [ $numb_filesSTAGE -lt $numb_files_cp ] ; then numb_files_cp=$numb_filesSTAGE ; fi
	if [ $numb_files_cp -ge 1 ] ; then 
		echo " numb_files_cp is now $numb_files_cp and we are moving files to $STMPDIR/FORINGEST " 
# Find the latest files and move this many [ $numb_files_cp ]

cd $PTMPDIR/STAGEFILES
ls -t $PTMPDIR/STAGEFILES/*.* | head -n $numb_files_cp | tr '\n' '\0' | xargs -0 mv -t $STMPDIR/FORINGEST
#find . -maxdepth 1 -type f | head -n $numb_files_cp | xargs -I {} mv {} $STMPDIR/FORINGEST 

	numb_STAGEAFTERCP=`find . -maxdepth 1 -type f -print0 | xargs -0 ls | wc -l`
	echo " BEFORE AND AFTER COPYING; CALCULATION 3001  $PTMPDIR/STAGEFILES HAD $numb_filesSTAGE files and now has $numb_STAGEAFTERCP files "
	echo " $PTMPDIR/STAGEFILES HAD $numb_filesSTAGE files and now has $numb_STAGEAFTERCP files "
	cd $STMPDIR/FORINGEST
	numb_files=`find . -maxdepth 1 -type f -print0 | xargs -0 ls | wc -l`
	echo " FINAL NUMBER OF FILES TO INGEST IS $numb_files "
	cd $DATA
	fi
	fi

##      CHECK IF MAX ALLOWED NUMBER OF FILES IS BREACHED 
	cd $STMPDIR/FORINGEST
	numb_files=`find . -maxdepth 1 -type f -print0 | xargs -0 ls | wc -l`
	if [ $numb_files -gt $IFILES_MAX_SAFE ] ; then
		echo " DIAGNOSE 2022; NUMBER OF FILES IN $STMPDIR/FORINGEST EXCEEDED $IFILES_MAX_SAFE "
		echo " DIAGNOSE 2022; NUMB OF FILES IS $numb_files and MAX ALLOWED IS $IFILES_MAX_SAFE "
	fi


##### COPIED OVER FROM $PTMPDIR/STAGEFILES . NOW, CHECK IF YOU NEED TO SYNC WITH AWS
##### CHECK IF THE NUMBER OF FILES IN $PTMPDIR/STAGEFILES is GE THAN $IFILES_MAX_SAFE. IF YES, THEN
##### DON'T SYNC. IF NO, THEN YOU NEED TO SYNC WITH AWS

if [ $numb_STAGEAFTERCP -ge $IFILES_MIN_SYNC ] ; then 
	CLOUDSYNC=NO 
	echo "CLOUDSYNC=NO  ; numb_STAGEAFTERCP is $numb_STAGEAFTERCP and IFILES_MIN_SYNC is $IFILES_MIN_SYNC "
		echo " NOT GOING TO SYNC : THE FILES AVAILABLE in STAGEFILES ARE "
		ls -l $PTMPDIR/STAGEFILES 
else
	CLOUDSYNC=YES
	echo "CLOUDSYNC=YES ; numb_STAGEAFTERCP is $numb_STAGEAFTERCP and IFILES_MIN_SYNC is $IFILES_MIN_SYNC "
		echo " YES, GOING TO SYNC : THE FILES AVAILABLE in STAGEFILES ARE "
		ls -l $PTMPDIR/STAGEFILES 

###########  START SYNCING FILES FROM AWS ###################
###########  START SYNCING FILES FROM AWS ###################
###########  START SYNCING FILES FROM AWS ###################
###########  START SYNCING FILES FROM AWS ###################
###########  START SYNCING FILES FROM AWS ###################

date -u
#### FIRST CLEAN OUT $PTMPDIR/STAGEFILES
/bin/rm -rf $PTMPDIR/STAGEFILES/
mkdir -p $PTMPDIR/STAGEFILES

#####################################################################
# LOOK FOR A PATTERN AND SEPARATE STRING BEFORE AND AFTER THE PATTERN
#https://www.google.com/search?q=all+characters+until+a+pattern+in+bash&rlz=1C1GCEB_enUS1053US1054&oq=all+characters+until+a+pattern+in+bash&gs_lcrp=EgZjaHJvbWUyBggAEEUYOTIGCAEQRRhA0gEKMjAwNDRqMGoxNagCCLACAfEFjwCY1jHpgak&sourceid=chrome&ie=UTF-8
#####################################################################
#bash# Stop before the word "target" (Excludes "target")
#echo "hello-world-target-end" | grep -oP '^.*?(?=target)'
# Output: hello-world-

# Stop after the word "target" (Includes "target")
#echo "hello-world-target-end" | grep -oP '^.*?(?=target)target'
# Output: hello-world-target
#
# LOOK FOR A PATTERN AND SEPARATE STRING BEFORE AND AFTER THE PATTERN
#####################################################################
#  Google Search: get characters after string in bash
#
#bashtext="Error level: critical system failure"
#result=$(echo "$text" | sed 's/.*level: //')

#echo "$result"
# Output: critical system failure
#
#########################################################

#To overwrite an existing link or file in Linux using the ln -s command, 
#you must add the -f (force) option to your command: ln -sf /path/to/target /path/to/link. 
#By default, running a standard ln -s command when the destination link name already exists 
#will cause the terminal to throw a "File exists" error and fail.



kdays=0
while [ $kdays -le $kbackupdays ]; do

date -u
ikdate=$(date --date="- $kdays day" +%Y%m%d)
echo " ikdate is $ikdate ; kdays is $kdays and kbackupdays is $kbackupdays "

kyyyy=$(echo $ikdate | cut -c 1-4)
kmm=$(echo $ikdate | cut -c 5-6)
kdy=$(echo $ikdate | cut -c 7-8)
echo " idate is $ikdate kyyyy is $kyyyy ; kmm is $kmm ; kdy is $kdy "
kcounter=1
# Loop as long as the counter is less than or equal to the maximum value
while [ $kcounter -le $knumber ]; do
 echo "Count is $kcounter"

if [ $kcounter -eq 1 ] ; then  
	export DIRAWS=$DNCF1
	export DSTRNG=$SNCF1
elif [ $kcounter -eq 2 ] ; then  
	export DIRAWS=$DNCF2 
	export DSTRNG=$SNCF2 
elif [ $kcounter -eq 3 ] ; then  
	export DIRAWS=$DNCF3
	export DSTRNG=$SNCF3 
elif [ $kcounter -eq 4 ] ; then  
	export DIRAWS=$DNCF4
	export DSTRNG=$SNCF4 
elif [ $kcounter -eq 5 ] ; then  
	export DIRAWS=$DNCF5
	export DSTRNG=$SNCF5
elif [ $kcounter -eq 6 ] ; then  
	export DIRAWS=$DNCF6
	export DSTRNG=$SNCF6
elif [ $kcounter -eq 7 ] ; then  
	export DIRAWS=$DNCF7
	export DSTRNG=$SNCF7
elif [ $kcounter -eq 8 ] ; then  
	export DIRAWS=$DNCF8
	export DSTRNG=$SNCF8
else
	echo " kcounter greater than  8 "
fi
echo " STARTING TO SYNC "
echo " STARTING TO SYNC: DIRAWS is $DIRAWS and DSTRNG is $DSTRNG "
if [ $isyncalldays = NO ] ; then 
echo " isyncalldays is $isyncalldays ; SYNC BY DAYS "
echo " STARTING TO SYNC ${DIRAWS}$kyyyy/$kmm/$kdy "
mkdir -p $PTMPDIR/SYNCDIR/${DIRAWS}$kyyyy/$kmm/$kdy
set +e
aws s3 sync s3://${DIRAWS}$kyyyy/$kmm/$kdy $PTMPDIR/SYNCDIR/${DIRAWS}$kyyyy/$kmm/$kdy --profile nccf
find $PTMPDIR/SYNCDIR/${DIRAWS}$kyyyy/$kmm/$kdy -name "$DSTRNG" -mtime -1 -exec /usr/bin/ln -sf --backup=no -t $PTMPDIR/STAGEFILES {} +
echo "Done for ${DIRAWS}$kyyyy/$kmm/$kdy "
else
echo " isyncalldays is $isyncalldays ; SYNC THE WHOLE PREFIX "
echo " STARTING TO SYNC ${DIRAWS} "
mkdir -p $PTMPDIR/SYNCDIR/${DIRAWS}
set +e
aws s3 sync s3://${DIRAWS} $PTMPDIR/SYNCDIR/${DIRAWS} --profile nccf
find $PTMPDIR/SYNCDIR/${DIRAWS} -name "$DSTRNG" -mtime -1 -exec /usr/bin/ln -sf --backup=no -t $PTMPDIR/STAGEFILES {} +
echo "Done for ${DIRAWS} "
fi


ls -l $PTMPDIR/STAGEFILES
set -xe
  ((kcounter++))
done
echo "Loop finished."
echo "DONE SYNCING WITH AWS SERVER for kdays = $kdays "
 ((kdays++))
done
echo "DONE SYNCING WITH AWS SERVER for all lags; ikdate = $ikdate "
#####                 
#####
#exit

## REMOVE FROM DIRECTORY A THE FILES ALREADY IN DIRECTORY B. FOR US DIRECTORY A=$PTMPDIR/STAGEFILES and DIRECTORY B=$PTMPDIR/DISCARD
#    ls -1 /path/to/DirectoryB | xargs -I {} rm -f "/path/to/DirectoryA/{}"
#

echo " REMOVE FROM DIRECTORY A THE FILES ALREADY IN DIRECTORY B"
echo "FOR US DIRECTORY A is $PTMPDIR/STAGEFILES and DIRECTORY B is $PTMPDIR/DISCARD "

cd $PTMPDIR/STAGEFILES

	numb_STAGEBEFORECLEAN=`find . -maxdepth 1 -type f -print0 | xargs -0 ls | wc -l`
echo " BEFORE REMOVING FILES ALSO IN $PTMPDIR/DISCARD, WE NOW HAVE $numb_STAGEBEFORECLEAN files in $PTMPDIR/STAGEFILES "

    ls -1 $PTMPDIR/DISCARD | xargs -I {} rm -f "$PTMPDIR/STAGEFILES/{}"

	numb_STAGEAFTERCLEAN=`find . -maxdepth 1 -type f -print0 | xargs -0 ls | wc -l`
echo " AFTER REMOVING FILES ALSO IN $PTMPDIR/DISCARD, WE NOW HAVE $numb_STAGEAFTERCLEAN files in $PTMPDIR/STAGEFILES "

cd $STMPDIR/FORINGEST
find . -maxdepth 1 -type f -print0 | xargs -0 ls -l
echo " DONE WITH THIS JOB "

	numb_files=`find . -maxdepth 1 -type f -print0 | xargs -0 ls | wc -l`
                numb_files_cp=`expr $IFILES_MIN_SAFE - $numb_files`
                echo "AFTER SYNCING; numb_files is $numb_files and IFILES_MIN_SAFE is $IFILES_MIN_SAFE and numb_files_cp is $numb_files_cp"

## REMOVE FROM DIRECTORY A THE FILES ALREADY IN DIRECTORY B. FOR US DIRECTORY A=$PTMPDIR/STAGEFILES and DIRECTORY B=$PTMPDIR/DISCARD
#    ls -1 /path/to/DirectoryB | xargs -I {} rm -f "/path/to/DirectoryA/{}"
#

        cd $PTMPDIR/STAGEFILES
                numb_filesSTAGE=`find . -maxdepth 1 -type f -print0 | xargs -0 ls | wc -l`
        echo " AFTER SYNCING; numb_filesSTAGE is $numb_filesSTAGE numb_files_cp is $numb_files_cp  numb_files is $numb_files and IFILES_MIN_SAFE is $IFILES_MIN_SAFE "
        if  [ $numb_filesSTAGE -lt $numb_files_cp ] ; then numb_files_cp=$numb_filesSTAGE ; fi
        if [ $numb_files_cp -ge 1 ] ; then
                echo " AFTER SYNCING; numb_files_cp is now $numb_files_cp and we are moving files to $STMPDIR/FORINGEST " 
# Find the latest files and move this many [ $numb_files_cp ]

cd $PTMPDIR/STAGEFILES
ls -t $PTMPDIR/STAGEFILES/*.* | head -n $numb_files_cp | tr '\n' '\0' | xargs -0 mv -t $STMPDIR/FORINGEST
#find . -maxdepth 1 -type f | head -n $numb_files_cp | xargs -I {} mv {} $STMPDIR/FORINGEST 

        numb_STAGEAFTERCP=`find . -maxdepth 1 -type f -print0 | xargs -0 ls | wc -l`
        echo " AFTER SYNCING; BEFORE&AFTER COPYING; CALCULATION 333001  $PTMPDIR/STAGEFILES HAD $numb_filesSTAGE files;now has $numb_STAGEAFTERCP files "
        echo " AFTER SYNCING; $PTMPDIR/STAGEFILES HAD $numb_filesSTAGE files and now has $numb_STAGEAFTERCP files "
        cd $STMPDIR/FORINGEST
        numb_files=`find . -maxdepth 1 -type f -print0 | xargs -0 ls | wc -l`
        echo " AFTER SYNCING; FINAL NUMBER OF FILES TO INGEST IS $numb_files "
        cd $DATA
        fi

##      CHECK IF MAX ALLOWED NUMBER OF FILES IS BREACHED 
        cd $STMPDIR/FORINGEST
        numb_files=`find . -maxdepth 1 -type f -print0 | xargs -0 ls | wc -l`
        if [ $numb_files -gt $IFILES_MAX_SAFE ] ; then
                echo " DIAGNOSE 333022; NUMBER OF FILES IN $STMPDIR/FORINGEST EXCEEDED $IFILES_MAX_SAFE "
                echo " DIAGNOSE 333022; NUMB OF FILES IS $numb_files and MAX ALLOWED IS $IFILES_MAX_SAFE "
        fi

###########  FINISH SYNCING FILES FROM AWS ###################
###########  FINISH SYNCING FILES FROM AWS ###################
###########  FINISH SYNCING FILES FROM AWS ###################
###########  FINISH SYNCING FILES FROM AWS ###################
###########  FINISH SYNCING FILES FROM AWS ###################
fi


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
   echo " IN INGEST_QUERY; STATION 400 and remotedsngrp is $remotedsngrp and machine is $machine "
   cat <<EOH_trans > $DATA/transquery.input.$host.$$
cd $REMOTEDIRGRP
ls $REMOTEDSNGRP $DATA/transquery.output.$host.$$
quit
EOH_trans

fi


transerror=99
itries=1
   if [ $TRANSFER_COMMAND = localdiskcp -o $TRANSFER_COMMAND = syncaws ] ; then


echo " QUERYPOINT 7711 "
####################################################
####################################################

   cd $DATA

#######################################

   transerror=0
   transfer_options=""
   remotedsngrp=$REMOTEDSNGRP
   machine=$MACHINE  
echo " IN INGEST_QUERY STATION 30000 LIST FILES IN $DATA BEFORE LISTING "
if [ -s $STMPDIR/FORINGEST/aout ] ; then rm $STMPDIR/FORINGEST/aout ; fi
if [ -s $DATA/aout ] ; then rm $DATA/aout ; fi
find $STMPDIR/FORINGEST/*.*  -printf "%f\n" > $DATA/aout
find $STMPDIR/FORINGEST/$REMOTEDSNGRP*  -printf "%f\n" > $DATA/aout_TEST
cp $DATA/aout  $DATA/transquery.output.$host.$$
if [ -s $STMPDIR/FORINGEST/aout ] ; then rm $STMPDIR/FORINGEST/aout ; fi
transerror=0
if [ -s $DATA/transquery.output.$host.$$ ] ; then 
cp $DATA/transquery.output.$host.$$ $DATA/transquery.input.$host.$$
fi
echo " QUERYPOINT 7713 "

echo " LIST FILES IN $DATA BEFORE LISTING "
ls -l $DATA
cp $DATA/aout $DATA/transquery.output.$host.$$
if [ -s $STMPDIR/FORINGEST/aout ] ; then rm $STMPDIR/FORINGEST/aout ; fi
cp $DATA/transquery.output.$host.$$ $DATA/querytrans.output101.$host.$$
echo " LIST FILES IN $DATA AFTER LISTING "
ls -l $DATA
echo " INGEST_QUERY STATION 3 ;   machine is $machine; REMOTEDSNGRP IS  $REMOTEDSNGRP "
cp $DATA/transquery.input.$host.$$ $DATA/querytrans.input101.$host.$$
ls -l $DATA/transquery.input.$host.$$
ls -l $DATA/transquery.output.$host.$$
cp $DATA/transquery.output.$host.$$ $DATA/querytrans.output101.$host.$$
ls -l $DATA
echo " END OF INGEST_QUERY STATION 3  CLOUD LISTING OF FILES "
echo " QUERYPOINT 7714 "
##################################

fi

echo " QUERYPOINT 77141 "
if [ $transerror -gt 0 ] ; then
echo " QUERYPOINT 77142 "
while [ $transerror -gt 0 -a $itries -le $ITRIES_MAX_QUERY ]; do
   [ -s $DATA/transquery.output.$host.$$ ]  &&  \
    rm $DATA/transquery.output.$host.$$
   if [ $itries -gt 1 ]; then
      msg="QUERY OF $3 FILES FAILED!!!! - SLEEP 30 SEC AND TRY AGAIN."
      $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      sleep 30
   fi

echo " QUERYPOINT 77143 "
   echo
   echo "Use $TRANSFER_COMMAND."
   echo

   echo " INGEST_QUERY STATION ; CHECK IF FILENAME IS BROUGHT AT THIS POINT"
   echo " itries is $itries; NO MESSAGE IS PRINTED ABOVE IF  itries is 1 " 
   cp $DATA/transquery.input.$host.$$ $DATA/querytrans.input103.$host.$$ 
   
   ls -l $DATA

echo " QUERYPOINT 77144; TRANSFER COMMAND is $TRANSFER_COMMAND  "
########################################################
      if [ $TRANSFER_COMMAND = localdiskcp -o $TRANSFER_COMMAND = syncaws ] ; then
	   echo " HERE IN INGEST_QUERY STATION 300 "
	   transerror=0
echo " QUERYPOINT 7715 "
   else
   $TRANSFER_COMMAND $transfer_options $machine < \
    $DATA/transquery.input.$host.$$ > $transout 2>&1
   transerror=$?
   echo " HERE IN INGEST_QUERY STATION 301 "

   echo "INGEST_QUERY STATION ; transerror is  $transerror and "
   ls -l $transout
   cp $DATA/transquery.input.$host.$$ $DATA/querytrans.input103.$host.$$ 
   echo "compare $DATA/transquery.input1.$host.$$ and $DATA/transquery.input103.$host.$$ "
   transerror=0
#  Cat out the standard output from the transfer process and remove it
#  -------------------------------------------------------------------
   fi
########################################################

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


      elif [ $TRANSFER_COMMAND = localdiskcp -o $TRANSFER_COMMAND = syncaws ] ; then
		echo " LAST FIX IN INGEST_QUERY , STATION 3001 "
		cp $DATA/aout $DATA/transquery.output.$host.$$
                if [ -s $STMPDIR/FORINGEST/aout ] ; then rm $STMPDIR/FORINGEST/aout ; fi
		transerror=0
echo " QUERYPOINT 7716 "
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

echo " IN INGEST_QUERY STATION 4 "
      cp $DATA/transquery.output.$host.$$ $DATA/querytrans.output104.$host.$$
      cat $DATA/transquery.output.$host.$$ | awk -F" ->" '{print$1}' | \
       awk -F" " '{print $NF}' > $DATA/transquery.testoutput.$host.$$

      cp $DATA/transquery.testoutput.$host.$$ $DATA/querytrans.testoutput101.$host.$$
      ls -l $DATA/transquery.testoutput.$host.$$
      
      mv $DATA/transquery.testoutput.$host.$$ $DATA/transquery.output.$host.$$
      cp $DATA/transquery.output.$host.$$ $DATA/querytrans.output104.$host.$$
      echo " COMPARE $DATA/querytrans.output101.$host.$$ and $DATA/querytrans.output104.$host.$$ "

      ls -l $DATA/transquery*
      ls -l $DATA
   fi

   [ ! -s $DATA/transquery.output.$host.$$ ]  &&  transerror=1
   itries=`expr $itries + 1`
done
itries=`expr $itries - 1`
fi

echo "QUERYPOINT 77161 ; TRANSFER_COMMAND IS $TRANSFER_COMMAND "
itries=1
set +x
echo
echo "Time is now $(date -u)."
echo
[ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x

#  If there was an error in the transfer (including if no listing was produced)
#   then exit w/ return code 1
#  ----------------------------------------------------------------------------

echo "QUERYPOINT 77162 ; TRANSFER_COMMAND IS $TRANSFER_COMMAND "
if [ $TRANSFER_COMMAND = localdiskcp -o $TRANSFER_COMMAND = syncaws ] ; then
		cp $DATA/aout $DATA/transquery.output.$host.$$
echo " QUERYPOINT 7717 "
		transerror=0
fi


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

echo " QUERYPOINT 77171 "
msg="QUERY OF $3 FILES successful on try no. ${itries}."
$UTILROOT/ush/postmsg "$jlogfile" "$msg"

#  Check to see if the lines returned by the 'ls' command include the full
#   directory path - if not add the directory in front of the filename here
#   while creating $DIRFILE -- UNLESS $REMOTEDIRGRP is set (i.e., not the
#   default of '.'), in which case only the filenames are used in the creation
#   of $DIRFILE.
#  ---------------------------------------------------------------------------

echo " QUERYPOINT 77172 "
ls -l $DATA

echo " QUERYPOINT 77173 "
cd $DATA
if [ $TRANSFER_COMMAND != syncaws ] ; then 

echo `head -n1 ./transquery.output.$host.$$` | grep /
err_grep=$?

echo " QUERYPOINT 77174 "
else

echo " QUERYPOINT 77175 "
echo `head -n1 ./transquery.output.$host.$$` 
transerror=0
err_grep=1
ls -l ./transquery.output.$host.$$
echo " transerror is $transerror and err_grep is $err_grep and transfercommand is $TRANSFER_COMMAND "
echo " QUERYPOINT 77176 "
fi

############################

echo " QUERYPOINT 7718 "
if [ $err_grep -eq 0 -o $REMOTEDIRGRP != '.' ]; then
   cp $DATA/transquery.output.$host.$$ $DIRFILE
   cp $DIRFILE $DATA/FILEDIR1
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
      cp $DIRFILE $DATA/FILEDIR2
      echo " filename is $filename and directfilename is $directfilename "
      set -x
fi
 
echo " QUERYPOINT 7719 "
if [ $TRANSFER_COMMAND = localdiskcp -o $TRANSFER_COMMAND = syncaws ] ; then
	cloudfilename=$filename
	echo " IN INGEST_QUERY STATION 107 "
	echo " cloudfilename is $cloudfilename and filename is $filename "
echo " QUERYPOINT 7720 "
fi

echo "STATION 108 in ingest_query.sh "

cp $DATA/transquery.output.$host.$$ $DATA/OUTPUT_TRANSQUERY.$$
cp $DATA/transquery.input.$host.$$ $DATA/INPUT_TRANSQUERY.$$
rm $DATA/transquery.output.$host.$$
rm $DATA/transquery.input.$host.$$
echo " QUERYPOINT 7721 "
ls -l $DATA

exit 
