#!/bin/ksh
# Run under ksh

####  UNIX Script Documentation Block
#
# Script name:  ingest_script_atovs1b.sh
#
# RFC contact:  Ling         Org: NP22        Date: 2018-12-08
#
# Abstract: This script reads ATOVS HIRS-3, HIRS-4, AMSU-A, AMSU-B and MHS
#   instrument 1B files (in NESDIS orbit-by-orbit 1B format) from the NOAA-
#   series and METOP-series satellites, converts them into NCEP BUFR format,
#   and writes them into the /dcom tank database on the NCEP CCS machines.
#   Two BUFR tanks can be written, one containing the antenna temperatures (Ta)
#   and one containing the processed brightness temperatures (Tb).  Only the
#   AMSU-A instrument actually writes Ta data into a BUFR tank.
#
# Script history log:
# 1998-06-19  Bert Katz      original version for implementation
# 2000-08-24  Larry Sager    modified to convert into NCEP BUFR (as well as
#                            IEEE) format and to write the BUFR files into
#                            /dcom using TRANJB
# 2001-03-15  Larry Sager    modified to add a step prior to TRANJB to execute
#                            BUFR_COMPRESS to compress BUFR messages
# 2004-01-28  Dennis Keyser  added variable "COMPRESS" to namelist "input"
#                            in stdin parm herefile; removed step which executed
#                            obsolete program BUFR_COMPRESS; modified to look
#                            more like corresponding ush script
#                            ingest_script_1b.sh (for eventual unification of
#                            the two scripts); improved comments and echoes
# 2005-04-29  Dennis Keyser  added processing of antenna temperatures (Ta)
#                            to a BUFR file unique from brightness temperatures
#                            (Tb), added variables "PROCESS_Tb" and "PROCESS_Ta"
#                            to namelist "input" in stdin parm herefile (note:
#                            only AMSU-A actually processes Ta data into a
#                            BUFR file)
# 2005-06-21  Dennis Keyser  modified to handle NOAA-18 instruments AMSU-A, MHS
#                            (which replaces AMSU-B for N-18) and HIRS-4 (which
#                            replaces HIRS-3 for N-18), the NOAA-18 data are not
#                            written into IEEE files
# 2006-05-12  Dennis Keyser  added check for imported variable CLEAN, if YES
#                            (default) cleans out old IEEE files stored in non-
#                            production directory TANKDIR, otherwise does not
#                            clean out old files - before, always cleaned out
#                            old files (this change does not affect production
#                            where TANKDIR begins with "/dcom" - old files are
#                            never cleaned out); improved Docblock, comments
# 2007-02-09  Dennis Keyser  added ability to handle METOP-2 satellite
# 2009-05-08  Greg Krasowski added NOAA-19 conditions
# 2012-10-18  Dennis Keyser  Added ability to handle METOP-1(B) satellite.
#                            Removed legacy IEEE processing (no longer used by
#                            production). Modified to run on WCOSS.
#                            Specifically, replaces CCS script variables
#                            XLFUNIT_n with FORTn (where n is the unit number
#                            connected to the filename defined by the variable
#                            FORTn) - needed because ifort uses FORTn.  Also,
#                            "banner" no longer works (replaced with "echo"),
#                            and "timex" replaced with "time -p".  This script
#                            is now set to run under ksh shell as the default.
# 2014-01-17  Diane Stokes/Dennis Keyser  USH script tranjb renamed to
#                            bufr_tranjb.sh and moved from directory path
#                            $USHbufr to directory path $USHobsproc_satingest.
#                            $EXECobsproc_satingest replaces $EXECbufr as the
#                            environment variable representing the directory
#                            path to the executables.  $FIXobsproc_satingest
#                            replaces $FIXbufr as the environment variable
#                            representing the directory path to the fixed
#                            files.  Updated some existing comments.
# 2018-12-08  Yangrong Ling  Added ability to handle METOP-3(C) satellite.
# 2021-12-19  Sudhir Nadiga  Modified to use bufr_tranjb module variables.
# 2022-01-18  S. Stegall     Replaced $DATA/ before calling utility scripts and instead 
#                            used $UTILROOT/ush/ to properly leverage the prod_util module.
#
#
# Usage: ingest_script_atovs1b.sh  <bufrtable>  <raw_file>
#
#   Script parameters:
#                 $1: bufrtable - full path definition for BUFR mnemonic table
#                 $2: raw_file  - full path definition for ATOVS 1B file
#
#   Modules and files referenced:
#     scripts     : $UTILROOT/ush/prep_step
#                   $UTILROOT/ush/postmsg
#                   $USHobsproc_satingest/bufr_tranjb.sh
#     executables : $EXECobsproc_satingest/$executable (where $executable is
#                                                       either bufr_tranamsua,
#                                                       bufr_tranamsub,
#                                                       bufr_tranmhs or
#                                                       bufr_tranhirs3)
#
# Remarks:
#
#   Invoked by the script ingest_translate_orbits.sh.
#
#   Imported Variables that must be passed in:
#      DATA                  - path to current working directory
#      USHobsproc_satingest  - path to obsproc_satingest ush directory
#                              containing bufr_tranjb.sh
#      FIXobsproc_satingest  - path to obsproc_satingest fix directory
#      EXECobsproc_satingest - path to obsproc_satingest executable directory
#      TANKDIR               - root of directory path to output BUFR database
#                              tank file (e.g., "/dcom/us007003")
#
#   Imported Variables that can be passed in:
#      COMPRESS - switch indicating whether or not messages in output
#                 BUFR files should be compressed (default = 'YES')
#      PROCESS_Tb
#               - switch indicating whether or not to process brightness
#                 temperature (Tb) reports into BUFR (default = 'YES')
#      PROCESS_Ta
#               - switch indicating whether or not to process antenna
#                 temperature (Ta) reports into BUFR (default = 'YES')
#
#   Condition codes:
#     0 - no problem encountered
#   > 0 - some problem encountered
#     Specifically:   1 - $executable: input data file does not contain
#                                      expected type
#                     2 - $executable: error reading coefficient file
#                                      (bufr_tranamsua only)
#                     3 - $executable: error reading header record of file
#                     6 - $executable: unknown satellite id
#                     7 - $executable: unknown satellite instrument
#                   103 - input 1B data file not found
#                   253 - no brightness temperature (Tb) reports processed by
#                         $executable (when PROCESS_Tb=YES) or
#                         no antenna temperature (Ta) reports processed by
#                         $executable (when PROCESS_Ta=YES)
#
# Attributes:
#   Language: ksh script
#   Machine:  NCEP WCOSS
#
####

set -aux


pwd


#  Set environment variables for processing
#  ----------------------------------------

table=$1
file=$DATA/$2
COMPRESS=${COMPRESS:-YES}
PROCESS_Tb=${PROCESS_Tb:-YES}
PROCESS_Ta=${PROCESS_Ta:-YES}

set +x
echo
echo "table = $table"
echo
echo "file = $DATA/$2"
echo
echo "COMPRESS = $COMPRESS"
echo
set -x


#  Check for existence of input 1B data file
#  -----------------------------------------

if [ -s $file ] ; then
  set +x
  echo
  echo "Input data file $file exists"
  echo
  set -x
else
  set +x
  echo
  echo "Input data file $file NOT FOUND - ABORTING"
  echo
  set -x
  exit 103
fi


#  Get instrument type and satellite id from file name, and do some
#  things specific to each intrument/satellite combination
#  ----------------------------------------------------------------

echo $2 | IFS="." read dum1 INSTRTYPE SATTYPE dum2

dum=$2
coefile="/dev/null"

if [ $SATTYPE = NK ]; then    # NOAA-15
  satnum=15
elif [ $SATTYPE = NL ]; then  # NOAA-16
  satnum=16
elif [ $SATTYPE = NM ]; then  # NOAA-17
  satnum=17
elif [ $SATTYPE = NN ]; then  # NOAA-18
  satnum=18
elif [ $SATTYPE = NP ]; then  # NOAA-19
  satnum=19
elif [ $SATTYPE = M2 ]; then  # METOP-A
  satnum=2
elif [ $SATTYPE = M1 ]; then  # METOP-B
  satnum=1
elif [ $SATTYPE = M3 ]; then  # METOP-C
  satnum=3
else
  satnum=??
fi

if [[ $INSTRTYPE = AMAX ]] ; then
  set +x
  echo
  echo
  if [ $satnum = 2 -o $satnum = 1 -o $satnum = 3 ]; then
####banner "METOP-$satnum" "AMSU-A 1B"
    echo "METOP-$satnum" "AMSU-A 1B"
  else
####banner "NOAA-$satnum" "AMSU-A 1B"
    echo "NOAA-$satnum" "AMSU-A 1B"
  fi
  echo
  echo
  set -x
  executable=bufr_tranamsua

#  NOAA-15 and -16 have different coefficients from other satellites
#   (other satellite coefficients are internal to program)
#  -----------------------------------------------------------------

  [ $satnum = 15 -o  $satnum = 16 ]  &&  \
   coefile=$FIXobsproc_satingest/bufr_amsua${satnum}_ta2tb.txt

elif [[ $INSTRTYPE = AMBX ]] ; then
  set +x
  echo
  echo
##banner "NOAA-$satnum" "AMSU-B 1B"
  echo "NOAA-$satnum" "AMSU-B 1B"
  echo
  echo
  set -x
  executable=bufr_tranamsub

elif [[ $INSTRTYPE = MHSX ]] ; then
  set +x
  echo
  echo
  if [ $satnum = 2 -o $satnum = 1 -o $satnum = 3 ]; then
####banner "METOP-$satnum" "MHS 1B"
    echo "METOP-$satnum" "MHS 1B"
  else
####banner "NOAA-$satnum" "MHS 1B"
    echo "NOAA-$satnum" "MHS 1B"
  fi
  echo
  echo
  set -x
  executable=bufr_tranmhs

elif [[ $INSTRTYPE = HIRX ]] ; then
  set +x
  echo
  echo
  if [ $satnum != 18 -a $satnum != 19 ] ; then
    if [ $satnum = 2 -o $satnum = 1 -o $satnum = 3 ]; then
######banner "METOP-$satnum" "HIRS-4 1B"
      echo "METOP-$satnum" "HIRS-4 1B"
    else
######banner "NOAA-$satnum" "HIRS-3 1B"
      echo "NOAA-$satnum" "HIRS-3 1B"
    fi
  else
####banner "NOAA-$satnum" "HIRS-4 1B"
    echo "NOAA-$satnum" "HIRS-4 1B"
  fi
  echo
  echo
  set -x
  executable=bufr_tranhirs3
   
fi


#########################################################################
#  NOTE: The program $executable (either BUFR_TRANAMSUA, BUFR_TRANAMSUB,
#        BUFR_TRANMHS or BUFR_TRANHIRS3) is designed to process BOTH Ta
#        and Tb into separate BUFR files in a single run.  However,
#        BUFRLIB routine WRITCP (which writes COMPRESSED messages) cannot
#        operate on two output files at the same time.  If it is
#        ever modified to do so (like WRITSB which writes uncompressed
#        BUFR messages), then the code to ready to handle this.  In
#        the meantime, this code must be executed twice to process
#        both Ta and Tb (once with PROCESS_Ta=YES and PROCESS_Tb=NO and
#        again with PROCESS_Ta=NO and PROCESS_Tb=YES).
#        Recall that only AMSU-A can actually process Ta into BUFR files.
#########################################################################

PROCESS_Ta_save_here=$PROCESS_Ta
PROCESS_Tb_save_here=$PROCESS_Tb

indx=0
while [ $indx -lt 2 ]; do
   indx=`expr $indx + 1`

   if [ $indx -eq 1 ]; then
# indx=1 means attempt to process Tb
      [ $PROCESS_Tb_save_here != 'YES' ]  && continue # skip if Tb not selected
      PROCESS_Ta=NO                      # must temporarily skip Ta processing
      outfile_52=$DATA/bufr_Tb           # only valid output file is unit 52
      cp /dev/null $DATA/dev_null_53
      outfile_53=$DATA/dev_null_53
   elif [ $indx -eq 2 ]; then
# indx=2 means attempt to process Ta
      [ $PROCESS_Ta_save_here != 'YES' ]  && continue # skip if Ta not selected
      PROCESS_Tb=NO                      # must temporarily skip Tb processing
      cp /dev/null $DATA/dev_null_52
      outfile_52=$DATA/dev_null_52
      outfile_53=$DATA/bufr_Ta           # only valid output file is unit 53
   fi                                    # for Ta - always set to /dev/null)

#  Form parm file input to program
#  -------------------------------

cat <<EOF_parm > parm
 &input
  infile='$file',           ! Path to input 1B data file
  compress='$COMPRESS',     ! BUFR compression switch
  coefile='$coefile',       ! Path to input coefficient file
  process_Tb='$PROCESS_Tb', ! Process bright. temps into BUFR?
  process_Ta='$PROCESS_Ta'  ! Process antenna temps into BUFR?
 /
EOF_parm

PROCESS_Tb=$PROCESS_Tb_save_here
PROCESS_Ta=$PROCESS_Ta_save_here

#  Link to low topography and HIRS coefficient files
#  -------------------------------------------------

ln -sf $FIXobsproc_satingest/bufr_hirsrtcf_ibm.dat hirsrtcf_ibm.dat \
 1>/dev/null 2>&1
ln -sf $FIXobsproc_satingest/bufr_lowtopog.dat lowtopog.dat 1>/dev/null 2>&1

#  Execute the program
#  -------------------

pgm=$executable
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

export FORT12=$table
export FORT52=$outfile_52
export FORT53=$outfile_53
time -p $EXECobsproc_satingest/$executable < parm 2> errfile
rcsave=$?
cat errfile
if [ $rcsave -eq 0 ] ; then
  set +x
  echo
  echo "Program $executable completed successfully"
  echo
  set -x
else
  set +x
  echo
  echo "PROBLEM IN PROGRAM $executable - ABORT with return code  $rcsave"
  echo
  set -x
  exit $rcsave
fi

done

#########################################################################
#########################################################################

if [ $PROCESS_Tb = YES ]; then
   if [ `cat Tb` = YES ]; then  # this is set in $executable if at least one
                                # Tb record was written

#  Store the Tb BUFR file into the database using BUFR_TRANJB processing
#  ---------------------------------------------------------------------

      if [ -s $DATA/bufr_Tb ]; then
         export cword=no
         $TRANush $TANKDIR $DATA/bufr_Tb
      fi
   else
     set +x
     echo
     echo "NO BRIGHTNESS TEMPERATURE (Tb) REPORTS PROCESSED by $executable - \
EXIT SCRIPT WITH return code 253"
     echo
     set -x
     exit 253
   fi
fi

if [ $PROCESS_Ta = YES ]; then
   if [ `cat Ta` = YES ]; then  # this is set in $executable if at least one
                                # Ta record was written

#  Store the Ta BUFR file into the database using BUFR_TRANJB processing
#   (note this can only occur if script did not exit above in Tb processing)
#  -------------------------------------------------------------------------

      if [ -s $DATA/bufr_Ta ]; then
         export cword=no
         $TRANush $TANKDIR $DATA/bufr_Ta
      fi
   else
     set +x
     echo
     echo "NO ANTENNA TEMPERATURE (Ta) REPORTS PROCESSED by $executable - \
EXIT SCRIPT WITH return code 253"
     echo
     set -x
     exit 253
   fi

fi

#  End of script
#  -------------

exit 0
