#!/bin/ksh
# Run under ksh

####  UNIX Script Documentation Block
#
# Script name:   ingest_copy.sh
#
# RFC contact:  Nadiga          Org: NP22        Date: 2019-10-09
#
# Abstract: This script copies a file previously ingested from a remote server
#   into the NCEP /dcom tank database on the NCEP supercomputers.  No
#   conversion is performed.  It can (optionally) also cut the yyyymmdd date
#   qualifier out of the filename and insert it as a sub-directory in the
#   output filename path (giving it a date-specific directory path).  It can
#   also (optionally) rename the output file by appending (as a suffix) a user-
#   specified string to the original, input filename. It can also (optionally)
#   remove files that are older than a specified number of days from the
#   directory path.
#
# Script history log:
# 2006-08-23  Patrick O'Reilly  Original version for implementation as
#                               ingest_avhrr.sh.
# 2009-12-22  Patrick O'Reilly  Original version for implementation as
#                               ingest_smoke.sh.
# 2013-01-08  Keyser            Minor modifications for WCOSS (as
#                               ingest_avhrr.sh).
# 2012-12-12  Keyser/Melchior   Minor modifications for WCOSS (as
#                               ingest_smoke.sh).
# 2014-08-06  Jianbin Yang      Cut date from proper place in filename for new
#                               MYD dust files (as ingest_smoke.sh).
# 2015-07-24  Keyser            Renamed from ingest_avhrr.sh, merged in logic
#                               from ingest_smoke.sh, and generalized to handle
#                               files from each plus almost any type of file.
# 2016-07-18  Keyser            Looks for imported environment variable
#                               $RENAME_FILE_SUFFIX and, if found, appends it
#                               to original, input filename (as a suffix) in
#                               order to rename the output file.
#                               BENEFIT: Allows same input file to be processed
#                                        by more than one ingest "family" when
#                                        the applicable families all execute
#                                        this script with the same values for
#                                        TANKDIR and TANKFILE (output directory
#                                        path).
# 2018-07-18  Y. Ling           Modified cutting date from proper place in filename
#                               for new HMS files.
# 2019-05-06  S. Nadiga         Modified cutting date from proper place in filename
#                               for new VIIRS Active Fire data files.
# 2019-10-09  S. Nadiga         Modified cutting date from proper place in filename
#                               for new MODIS fire data files.
# 2021-08-23  S. Nadiga         Modified cutting date from proper place in filename
#                               for MODIS fire data files with new filenames.
# 2022-01-18  S. Stegall        Replaced $DATA/ before calling utility scripts and 
#                               instead used $UTILROOT/ush/ to properly leverage the 
#                               prod_util module.
#
# Usage: ingest_copy.sh  <dummy>  <raw_file>
#
#   Script parameters:
#                 $1: dummy      - dummy (not used here)
#                 $2: raw_file   - input filename (without path)
#
#   Modules and files referenced:
#     scripts    : $UTILROOT/ush/postmsg
#     data cards : none
#     executables: none
#
# Remarks:
#
#   Invoked by the script ingest_translate_orbits.sh.
#
#   Imported Variables that must be passed in:
#      DATA     - path to current working directory
#      TANKDIR  - root of directory path to output file (e.g.,
#                 "/dcom/us007003" or "$DCOMROOT/${envir}")
#      TANKFILE - single or multiple sub-directory path in ${TANKDIR}${PDY_dir}
#                 containing output file (e.g., "avhrr" or "wgrdbul/smoke")
#                 (see below for definition of $PDY_dir)
#      ndayarch - number of days that input files transferred from the remote
#                 machine will be kept in directory 
#                 ${TANKDIR}${PDY_dir}/$TANKFILE if CLEAN=YES (see below)
#                 (see below for definition of $PDY_dir)
#      jlogfile - path to joblog file
#
#   Imported Variables that can be passed in:
#      RENAME_FILE_SUFFIX -
#                 string to append to original, input filename (as a suffix)
#                 when writing it to output path, thus allowing output filename
#                 to be different than input filename (if not passed in, output
#                 filename remains the same as input filename)
#      CUT_DATE - switch to cut the yyyymmdd date qualifier out of the
#                 filename and insert it as a sub-directory between $TANKDIR
#                 and $TANKFILE when creating the directory path to the output
#                 file (default = 'YES' if string containing first 7 characters
#                 of imported value for $TANKFILE is 'wbufbul', 'wgrbbul',
#                 'wgrdbul' or 'wtxtbul', otherwise default is 'NO')
#                 Note: If CUT_DATE is imported as "YES", then files are copied
#                         to a date-specific directory path, where that date
#                         matches the date in the filename.  In this case the
#                         variable $PDY_dir is defined as "/<yyyymmdd>".
#                       If CUT_DATE is imported as "NO", then files are copied
#                         to a date-independent directory path.  In this case
#                         the variable $PDY_dir is defined as "" (null).
#      CLEAN    - switch to remove files stored in directory
#                 ${TANKDIR}${PDY_dir}/$TANKFILE that are more than $ndayarch
#                 days old (default = 'NO' if string containing first 7 characters
#                 of imported value for $TANKFILE is 'wbufbul', 'wgrbbul',
#                 'wgrdbul' or 'wtxtbul', otherwise default is 'YES')
#                 Note: Normally CLEAN is imported as "NO" when "CUT_DATE" is
#                       imported as "YES" and vice-versa since there is
#                       expected to be no accumulation of old files in a date-
#                       dependent directory path.
#
#   Condition codes:
#     0 - no problem encountered
#   > 0 - some problem encountered
#     Specifically:   1 - input file not found
#                    10 - error copying file to target directory
#                         ${TANKDIR}${PDY_dir}/$TANKFILE
#
# Attributes:
#   Language: ksh script
#   Machine:  NCEP WCOSS
#
####

set -aux
cd $DATA

pwd

#  Set environment variables for processing
#  ----------------------------------------

infile=$DATA/$2
if [ `echo $TANKFILE | cut -c 1-7` = wbufbul -o \
     `echo $TANKFILE | cut -c 1-7` = wgrbbul -o \
     `echo $TANKFILE | cut -c 1-7` = wgrdbul -o \
     `echo $TANKFILE | cut -c 1-7` = wtxtbul -o \
     `echo $TANKFILE | cut -c 1-7` = wf_abba -o \
     `echo $TANKFILE | cut -c 1-7` = modis_f -o \
     `echo $TANKFILE | cut -c 1-8` = af_viirs ]; then
   CUT_DATE=${CUT_DATE:-YES}
   CLEAN=${CLEAN:-NO}
else
   CUT_DATE=${CUT_DATE:-NO}
   CLEAN=${CLEAN:-YES}
fi

set +x
echo
echo "infile = $DATA/$2"
echo "CUT_DATE = $CUT_DATE"
[ -n "$RENAME_FILE_SUFFIX" ] && echo "RENAME_FILE_SUFFIX = $RENAME_FILE_SUFFIX"
echo "CLEAN = $CLEAN"
echo
set -x


PDY_dir=""
if [ $CUT_DATE = YES ]; then

#  Check to see what the filename looks like. If it's the ASDTA SMOKE cut the
#  date from one place in the file, if it's the MYD DUST cut from another, if
#  its' he VIIRS Active Fire cut from another, otherwise, if its the HMS or
#  HMS Hysplit, cut from yet another.

   fl=`echo $2 | cut -c1`
   f2=`echo $2 | cut -c2`
   f3=`echo $2 | cut -c10`

   if  [ "$fl" == "G" ] ; then # GWHI, GW, G13
     export PDY=`echo $2 | cut -f2 -d"." | cut -c1-8`
     echo "  fname=$2 and PDY=$PDY "
   elif  [ "$fl" == "M" ] ; then # MYD DUST OR MODIS FIRE
     if  [ "$f2" == "O" ] ; then # MODIS FIRE
      if  [ "$f3" == "G" ] ; then # MODIS FIRE OLD VERSION
       export PDYJ=`echo $2 | cut -c29-35`
       export PDY=`$UTILROOT/ush/date2jday.sh $PDYJ`
       echo " old filename is $2 ;  PDYJ is $PDYJ and PDY is $PDY "
	else # MODIS FIRE NEW VERSION
       export PDYJ=`echo $2 | cut -c31-37`
       export PDY=`$UTILROOT/ush/date2jday.sh $PDYJ`
       echo " new filename is $2 ;  PDYJ is $PDYJ and PDY is $PDY "
       fi
     else
        export PDY=`echo $2 | cut -f6 -d"." | cut -c1-8`
        echo "  fname=$2 and PDY=$PDY "
     fi
   elif  [ "$fl" == "A" ] ; then  # VIIRS AF
     export PDY=`echo $2 | cut -c14-21`
     echo " ACTIVE FIRE DATA , fname=$2 and PDY=$PDY "
   elif  [ "$fl" == "f" ] ; then  # WF_ABBA
     export PDYJ=`echo $2 | cut -c2-8`
       export PDY=`$UTILROOT/ush/date2jday.sh $PDYJ`
     echo " WF_ABBA , fname=$2 ; PDYJ is $PDYJ and PDY is $PDY "
   else  # hms, hmshysplit
     f2=`echo $2 | cut -f1 -d"."`
     export PDY=`echo $f2 | cut -c $(expr ${#f2} - 8 + 1)-`
     echo " fname=$2 and PDY=$PDY "
   fi

   echo
   echo "Date is $PDY"
   echo

   PDY_dir=/$PDY
fi

#  Check for existence of input file
#  ---------------------------------

if [ -s $infile ] ; then
  set +x
  echo
  echo "Input data file $infile exists"
  echo
  set -x
else
  set +x
  echo
  echo "Input data file $infile NOT FOUND - ABORTING"
  echo
  set -x
  exit 1
fi


#----------------------------------------------------------
#

cd $TANKDIR
if [ ! -d ${TANKDIR}${PDY_dir}/$TANKFILE ] ; then
   mkdir -m 775 -p ${TANKDIR}${PDY_dir}/$TANKFILE
fi
cd $DATA

newfile=$(basename $infile)
if [ -n "$RENAME_FILE_SUFFIX" ]; then
   newfile=${newfile}${RENAME_FILE_SUFFIX}
   msg="Output file RENAMED from \"$(basename $infile)\" to \"$newfile\""
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
fi
msg="copying $newfile"
$UTILROOT/ush/postmsg "$jlogfile" "$msg"

if [ ! -e ${TANKDIR}${PDY_dir}/$TANKFILE ] ; then
   mkdir -m 775 -p ${TANKDIR}${PDY_dir}/$TANKFILE
fi

cp $infile ${TANKDIR}${PDY_dir}/$TANKFILE/$newfile

retc=$?
if [ $retc -ne 0 ] ; then
   set +x
   echo
   echo "###### FAILURE IN FINAL COPY ---- ABORT"
   echo
   set -x
   exit 10
else
   set +x
   echo
   echo "FILE SUCCESSFULLY COPIED TO \
${TANKDIR}${PDY_dir}/$TANKFILE/$newfile"
   echo
   set -x
fi

if [ $CLEAN = YES ]; then

   orig_num=`ls -1 ${TANKDIR}${PDY_dir}/$TANKFILE | wc -l`

#  Clean out the ${TANKDIR}${PDY_dir}/$TANKFILE (e.g., /dcom/us007003/avhrr or
#  $DCOMROOT/${envir}/avhrr) subdirectory of files greater than
#  $ndayarch days old
#  --------------------------------------------------------------------------
   find ${TANKDIR}${PDY_dir}/$TANKFILE -type f -mtime +$ndayarch -exec rm {} \;

   new_num=`ls -1 ${TANKDIR}${PDY_dir}/$TANKFILE | wc -l`
   set +x
   echo
   echo "Number of old files removed = `expr $orig_num - $new_num`"
   echo
   set -x

fi

#  End of script
#  -------------

exit 0
