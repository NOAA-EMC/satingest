#!/bin/ksh
# Run under ksh

####  UNIX Script Documentation Block
#
# Script name:   ingest_script_mls.sh
#
# RFC contact:  Keyser      Org: NP22        Date: 2014-01-17
#
# Abstract: This script reads Microwave Limb Sounder (MLS) Ozone Mixing Ratio
#   files (ML2O3) in HDF5 format) from the NASA Aura POES satellite, converts
#   them into NCEP BUFR format, and writes them into the /dcom tank database on
#   the NCEP WCOSS machines via bufr_tranjb.sh.
#
# Script history log:
# 2009-01-16  Dennis Keyser -- Original version for implementation
# 2011-11-02  Greg Krasowsk -- Undocumented update.
# 2012-11-29  Dennis Keyser -- Modified to run on WCOSS.  Specifically,
#                              replaces CCS script variables XLFUNIT_n with
#                              FORTn (where n is the unit number connected to
#                              the filename defined by the variable FORTn) -
#                              needed because ifort uses FORTn.  Also, "timex"
#                              replaced with "time -p".  This script is now set
#                              to run under ksh shell as the default.
# 2014-01-17  Diane Stokes/Dennis Keyser -- Now includes hostname as well as
#                              process id in temporary filenames where only
#                              process id was present before.  USH script
#                              tranjb renamed to bufr_tranjb.sh and moved from
#                              directory path $USHbufr to directory path
#                              $USHobsproc_satingest.  $EXECobsproc_satingest
#                              replaces $EXECbufr as the environment variable
#                              representing the directory path to the
#                              executables.  Updated some existing comments.
# 2021-12-19  Sudhir Nadiga    Modified to use bufr_tranjb module variables.
# 2022-01-18  S. Stegall       Replaced $DATA/ before calling utility scripts and instead 
#                              used $UTILROOT/ush/ to properly leverage the prod_util module.
#
#
# Usage: ingest_script_mls.sh  <bufrtable>  <raw_file>
#
#   Script parameters:
#                 $1: bufrtable - full path definition for BUFR mnemonic table
#                 $2: raw_file  - file name (only) definition for input mls
#                                 file
#
#   Modules and files referenced:
#                   $UTILROOT/ush/prep_step
#                   $UTILROOT/ush/postmsg
#                   $USHobsproc_satingest/bufr_tranjb.sh
#     executables : $EXECobsproc_satingest/bufr_tranmls
#
# Remarks:
#
#   Invoked by the script ingest_translate_orbits.sh.
#
#   Imported Variables that must be passed in:
#      DATA                  - path to current working directory
#      USHobsproc_satingest  - path to obsproc_satingest ush directory
#                              containing bufr_tranjb.sh
#      EXECobsproc_satingest - path to obsproc_satingest executable directory
#      jlogfile              - path to joblog file
#      TANKDIR               - root of directory path to output BUFR database
#                              tank file (e.g., "/dcom/us007003")
#
#   Condition codes:
#     0 - no problem encountered
#   > 0 - some problem encountered
#     Specifically:   1 = bufr_tranmls: cannot open HDF5 interface
#                     2 = bufr_tranmls: cannot open HDF5 file (note: this
#                                       error will trigger one attempted re-run
#                                       of ingest processing for mls ozone
#                                       family)
#                     3 = bufr_tranmls: cannot open a groupname
#                     4 = bufr_tranmls: cannot open an attribute
#                     5 = bufr_tranmls: cannot read attribute-based variable
#                     6 = bufr_tranmls: invalid number of along-track scan
#                                       lines in file
#                     7 = bufr_tranmls: cannot close a groupname
#                     8 = bufr_tranmls: error returned from an HDF5 interface
#                                       routine
#                   103 - input mls data file not found
#                   253 - no mls reports processed by 
#                           $EXECobsproc_satingest/bufr_tranmls
#                   xxx - an error coming out of program BUFR_TRANJB
#
# Attributes:
#   Language: ksh script
#   Machine:  NCEP WCOSS
#
####

set -aux

host=$(hostname -s)

cd $DATA
pwd


#  Set environment variables for processing
#  ----------------------------------------

table=$1
file=$DATA/$2

set +x
echo
echo "table = $table"
echo
echo "file = $DATA/$2"
echo
set -x


#  Check for existence of input mls data file
#  ------------------------------------------

if [ -s $file ] ; then
  set +x
  echo
  echo "Input mls data file $file exists"
  echo
  set -x
else
  set +x
  echo
  echo "Input mls data file $file NOT FOUND - ABORTING"
  echo
  set -x
  exit 103
fi


#  Execute the program
#  -------------------

pgm=bufr_tranmls
if [ -s $UTILROOT/ush/prep_step ]; then
  set +u
  . $UTILROOT/ush/prep_step
  set -u
else
  [ -f errfile ] && rm errfile
  export FORT01=0
  unset `env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}'`
fi

msg="$pgm has BEGUN"
$UTILROOT/ush/postmsg "$jlogfile" "$msg"

export FORT31=$table
export FORT51=$DATA/mls_bufr.$host.$$
time -p $EXECobsproc_satingest/bufr_tranmls $file 2> errfile
rcsave=$?
####################cat $DATA/mls_output
cat errfile
if [ $rcsave -eq 0 ] ; then
  set +x
  echo
  echo "Program bufr_tranmls completed successfully"
  echo
  set -x
else
  set +x
  echo
  echo "PROBLEM IN PROGRAM bufr_tranmls - ABORT with return code  $rcsave"
  echo
  set -x
  if [ $rcsave -eq 2 ]; then

#  A return code of 2 from bufr_tranmls (meaning cannot open HDF5 file) will
#   trigger one attempted re-run of ingest processing for mls ozone family
#   (the executing script ingest_translate_orbits.sh will not generate an
#   errlog message in this case UNLESS this is the second attempt and r.c.= 2
#   has once again occurred)
#  --------------------------------------------------------------------------

     if [ -s $DATA/run_this_again ]; then
        rcsave=20  # change r.c. from 2 to 20 if this is second attempt to
                   # ingest mls data - this signals ingest_translate_orbits.sh
                   # to go ahead and generate an errlog message in this case
     else
          # writing the string below into the file $DATA/run_this_again will
          # later signal the job script to make one more attmept to re-run
          # the ingest processing for the mls ozone family

        echo "run this again" > $DATA/run_this_again
     fi
  fi
  exit $rcsave
fi


#  Store the mls BUFR file into the database using BUFR_TRANJB processing
#  ----------------------------------------------------------------------

if [ -s $DATA/mls_bufr.$host.$$ ]; then
   give_rc="YES"
   cword="no"
   sh $TRANush $TANKDIR $DATA/mls_bufr.$host.$$
   ier=$?
else
   set +x
   echo
   echo "NO mls REPORTS PROCESSED by bufr_tranmls - EXIT SCRIPT WITH return \
code 253"
   echo
   set -x
   exit 253
fi

#  End of script
#  -------------

exit $ier
