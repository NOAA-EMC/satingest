#!/bin/ksh
# Run under ksh

####  UNIX Script Documentation Block
#
# Script name:   ingest_translate_orbits.sh
#
# RFC contact:  Ling       org: NP22        date: 2018-12-06
#
# Abstract: Runs some type of translation processing on time-stamped data files
#   previously received from a remote unix machine.  This may consist of
#   generating BUFR messages to append to a BUFR tank file, or possibly
#   generating GRIB files from ASCII or binary format files. It may also be as
#   simple as copying the file into a specified location on the local machine.
#
# Script history log:
# 1996-10-03  Bert Katz   Original version for implementation.
# 1996-11-29  Bert Katz   Unified the output file.
# 1997-01-09  Bert Katz   Output file now goes to stdout, reduced size of
#     cumulative output file, reduced size of errlog file, added debug option,
#     reduced number of attempts to process an orbit before giving up from 10
#     to 5.
# 1997-02-27  Bert Katz   Added fix to limit to 5 the number of attempts to
#     process a zero-length file.
# 1997-03-20  Bert Katz   Moved error handling out of this script and into
#     scripts "process_orbits" and "unixproc_orbits".
# 1997-06-20  Bert Katz   Added option of using a BUFR table to unpack a
#     "foreign" input BUFR file.
# 1997-07-17  Bert Katz   Added code to limit the "errlog" file to 3000 lines,
#     and to add the newest lines to the head of the file.
# 1997-09-12  Bert Katz   Changed input file assign from -Fsystem to
#     -sunblocked (-Fcos and -Ff77 remain as they were).
# 1997-09-18  Bert Katz   Added logic to the "foreign" input BUFR file
#     processing option to point to a foreign BUFR table in /nwprod/bufr/parm
#     if no such BUFR table exists in TABLEDIR.
# 1997-10-20  Bert Katz   Modified to permit multiple input file families.
# 1998-12-23  Bert Katz   Program tranjb is now used to append BUFR messages to
#     tanks.  The "trans" executables now make a temporary file for input into
#     tranjb.
# 2001-03-15  R. Treadon  Add special logical to handle file renaming for TRMM
#     TMI hdf format files.
# 2006-05-12  D. Keyser   Incorporates special processing for SSM/I data which
#     had been in previous script ingest_transsmi.sh (which is now removed).
#     Improved documentation and comments.  Increased length of output files
#     from max/min 60000/50000 to max/min 130000/120000.
# 2007-05-14  D. Keyser   Incorporates special processing for new AVHRR data.
#     Prints message to joblog file for case where no eligible executable or
#     script is found (will be used later to stop executing job with a FATAL
#     error).
# 2009-09-30  D. Keyser   Added script variable UPDATE_ERRLOG which directs
#     script to write information to errlog file when there is an error coming
#     out of the executing program or script - this is always YES (write to
#     errlog) except when EXECUTE is ingest_script_omi.sh and error is 2,
#     meaning the program BUFR_TRANOMI could not open the input HDF5 file -
#     this is not a true error because it is not yet ready for processing and
#     will be opened and processed the next time the ingest processing for the
#     OMI ozone family runs (this error happens often, this change will prevent
#     the errlog file from being updated)
# 2013-01-07  D. Keyser   Modified to run on WCOSS. Specifically, replaces
#     CCS script variables XLFUNIT_n with FORTn (where n is the unit number
#     connected to the filename defined by the variable FORTn) - needed because
#     ifort uses FORTn. Also, since WCOSS default script (sh) is bash, all
#     child scripts are executed under ksh (the CCS default) rather than under
#     sh (some script commands tranferred from CCS version do not work under
#     bash). This script is now set to run under ksh shell as the default.
# 2014-01-21  Diane Stokes/D. Keyser  Renamed to add suffix .sh qualifier.  Now
#     includes hostname as well as process id in temporary filenames where only
#     process id was present before.  $EXECobsproc_satingest replaces $EXECbufr
#     as the environment variable representing the directory path to the
#     executables.  $FIXobsproc_satingest replaces $FIXbufr as the environment
#     variable representing the directory path to the fixed files (does not
#     apply to bufrtab.XXX files which are still looked for in $FIXbufr).
#     $PARMobsproc_satingest replaces $PARMbufr as the environment variable
#     representing the directory path to the fixed files.  USH script tranjb
#     renamed to bufr_tranjb.sh and moved from directory path $USHbufr to
#     directory path $USHobsproc_satingest.  Removed logic in "foreign" input
#     BUFR file processing which pointed to a foreign BUFR table in $TABLEDIR
#     (= $FIXbufr) as a first choice with $PARMbufr as a second choice (if
#     foreign BUFR table not found in $TABLEDIR), and instead now points only
#     to directory path $FIXobsproc_satingest to obtain this file (foreign BUFR
#     table files will no longer ever reside in a parm directory, only in a fix
#     directory).  NCEP BUFR tables bufrtab.XXX are now first looked for in
#     directory path $FIXobsproc_satingest rather than in directory path
#     $TABLEDIR, and if they are not found there they are looked for in
#     directory path $FIXbufr (which is now defined simply as the path to non-
#     obsproc fix directory).  $TABLEDIR is no longer referenced in this script
#     ($FIXbufr and $TABLEDIR were always exported with the same value before).
#     First choice for looking for scripts to execute is now directory path
#     $USHobsproc_satingest, with last resort now being $USHbufr.  Before,
#     first choice was $USHbufr.  Logic added to do special processing if
#     NCO/SIB script ingest_cloud_NASA.sh is being executed: switches priority
#     for looking for bufrtab.012 in both ingest_cloud_NASA.sh and bufr_tranjb
#     to $FIXbufr path (latter accomplished by exporting $RUN_TYPE as 'decoder'
#     rather than as 'satingest'); forces newer versions of
#     $USHbufr/ingest_cloud_NASA.sh to execute
#     $USHobsproc_satingest/bufr_tranjb.sh, and forces older versions of
#     $USHbufr/ingest_cloud_NASA.sh to execute
#     $EXECobsproc_satingest/bufr_tranjb (even though older versions of
#     ingest_cloud_NASA.sh are hardwired to execute parent script as
#     $USHbufr/tranjb; the script ingest_cloud_NASA.sh will be found in the
#     last resort directory path $USHbufr since it is under NCO/SIB control.
#     All references to "GMT" changed to "UTC".  Progam bufr_transsmi now reads
#     land-sea tags (from unit 33) from location
#     $FIXobsproc_satingest/nesdis.lstags_transsmi rather than from location
#     $utilparm/nesdis.lstags.  The content is not changed.  Removed reference
#     to unit 38 in program bufr_tranavhrr as this file
#     (sst2dvar_aoi.61.90.clim.halfdeg, sea-surface temperature climatology on
#     0.5 degree grid) has not been read by this program for a long time.
#     Added information to docblock and new comments.  Updated some existing
#     comments.
# 2014-09-14  D. Keyser   Accounts for highly likely possibility that parent
#     job script JIGOES_RADSND is now exporting $FIXbufr as "/dev/null".  As
#     such, if NCO/SIB script ingest_cloud_NASA.sh is being executed and
#     $FIXbufr is imported as "/dev/null", this script temporarily resets
#     $FIXbufr to either /nwprod/decoders/decod_shared/fix (as a first choice),
#     or /nwprod/fix (as a second choice, if the first choice location is not
#     found).  This is needed because in the ingest_cloud_NASA.sh processing,
#     $FIXbufr is the first choice for the location of the bufrtab.012 files in
#     both ingest_cloud_NASA.sh and bufr_tranjb.  This prevents a failure
#     here when $FIXbufr is imported as "/dev/null".
# 2015-08-07  D. Keyser   No longer executes gzip -d (i.e., gunzip) if program
#     bufr_trantmi is about to be executed since the file coming into this
#     program is now expected to have been uncompressed upstream in
#     ingest_process_orbits.sh.  This type is no longer processed in
#     production.  If it is ever processed again either in production or in a
#     historical run, job script JITMI would have to be resurrected and updated
#     to set UNCOMPRESS_UNIX to its default value YES for the "TRMMTMI" family.
# 2015-11-05  D. Keyser   If the file being translated here was generated from
#     a concatenation of individual BUFR files pulled from the remote server,
#     the names of these individual files that contributed to the concatenation
#     will be listed in the stdout when this script is executed (applicable
#     only to satingest run type).
# 2016-05-11  NCO/PMB     Added script variable GETGES_COM to override the
#     getges.sh default of "/com" when searching gfs/gdas output directories
#     for an sst file needed for SSMI SDR processing.  If not imported here, 
#     defaults to "/com2".  (Implemented with the move of the GFS to Phase 2.)
# 2016-05-27  D. Stokes   Pick up the sst file needed for SSMI SDR processing
#     from its original source rather than searching gfs/gdas directories for a
#     copy of the same data.  (The GFS no longer uses this sst file so we
#     should not rely on its continued availability in gfs/gdas directories).
# 2018-12-06  Y. Ling  Updated to run on phase 3 machine.
# 2020-04-03  S. Nadiga OMI datafiles are now being ingested from the PDA server
#     and do not suffer from the problem of incompleteness, so the lines to process
#     incomplete files have been commented out.
# 2021-12-19  S. Nadiga Modified to use bufr_tranjb module variables.
# 2022-01-18  S. Stegall  Replaced $DATA/ before calling utility scripts and instead 
#      used $UTILROOT/ush/ to properly leverage the prod_util module.
#
#
# Usage: ingest_translate_orbits.sh
#
#   Script parameters: none
#
#   Modules and files referenced:
#     scripts    : $USHobsproc_satingest/bufr_tranjb.sh
#                  $UTILROOT/ush/postmsg
#                  $UTILROOT/ush/prep_step
#                  $UTILROOT/ush/finddate.sh
#                  $USHobsproc_satingest/$EXECUTE
#                                (ush script to perform translation - executed
#                                 only if program
#                                 $EXECobsproc_satingest/$EXECUTE not found)
#                  $USERDIR/$EXECUTE
#                                (ush script to perform translation - executed
#                                 only if program
#                                 $EXECobsproc_satingest/$EXECUTE and ush
#                                 script $USHobsproc_satingest/$EXECUTE not
#                                 found and $USERDIR/$EXECUTE is a ush script
#                                 and not a program)
#                  $USHbufr/$EXECUTE
#                                {ush script to perform translation - executed
#                                 only if program
#                                 $EXECobsproc_satingest/$EXECUTE, ush script
#                                 $USHobsproc_satingest/$EXECUTE and ush script
#                                 or program $USERDIR/$EXECUTE -ALL- not found
#                                 (e.g., ingest_cloud_NASA.sh)}
#     data cards : none
#     executables: $EXECobsproc_satingest/$EXECUTE
#                                (program to perform translation)
#                  $USERDIR/$EXECUTE
#                                (program to perform translation - executed
#                                 only if program
#                                 $EXECobsproc_satingest/$EXECUTE and ush
#                                 script $USHobsproc_satingest/$EXECUTE not
#                                 found and $USERDIR/$EXECUTE is a program and
#                                 not a ush script)
#                  $GRBINDEX 
#
# Remarks: Invoked by the script ingest_process_onetype_neworbits.sh which is
#          sourced by the script ingest_process_orbits.sh.
#
#   Imported Variables that must be passed in:
#      DATA                  - path to current working directory
#      jlogfile              - path to joblog file
#      DEBUGSCRIPTS          - if set to "ON" or "YES", will run with "set -x"
#                              on (intended for debugging)
#      EXECobsproc_satingest - path to obsproc_satingest executable directory
#                              possibly containing $EXECUTE (as a program)
#      USHobsproc_satingest  - path to obsproc_satingest ush directory
#                              containing bufr_tranjb.sh and possibly
#                              containing $EXECUTE (as a script) if it is not
#                              found in as a program in $EXECobsproc_satingest
#      FIXobsproc_satingest  - path to obsproc_satingest fix directory; first
#                              choice to path to directory containing BUFR
#                              mnemonic table files bufrtab.XXX; path to
#                              directory containing "foreign" BUFR mnemonic
#                              table 
#      USERDIR               - path to directory possibily containing $EXECUTE
#                              (as a script or a program) if it is not found
#                              either as a program in $EXECobsproc_satingest or
#                              as a script in $USHobsproc_satingest
#      OUTDIR                - path to directory containing output listing
#                              (usually same as $TANKDIR)
#      BUFRTABLE             - the file in either $FIXobsproc_satingest or
#                              $FIXbufr holding the BUFR mnemonic table which
#                              controls the creation of BUFR messages to be
#                              added to the tank file
#                              (Note: if imported as either 'none', 'nulltable'
#                                     or 'nobufrtable' then there is no BUFR
#                                     mnemonic table)
#      FORGNTABLE            - the file in $FIXobsproc_satingest holding the
#                              BUFR mnemonic table which controls the reading
#                              of "foreign" BUFR messages
#      TANKFILE              - path to directory and tank file in
#                              $TANKDIR/<YYYYMMDD> (see below) to be created/
#                              appended (e.g., "b001/xx002")
#      TANKPROTECT           - switch to copy (if "YES") the tank file to a
#                              temporary file before updating it (after which
#                              the temporary tank file will be moved over to
#                              the permanent tank file location; otherwise. the
#                              tank file will be updated in situ)
#      $UTILROOT/ush         - path to utility ush script directory containing
#                              finddate.sh
#      EXECUTE               - the name of a program or ush script used to
#                              translate file
#      dsname                - name of file being processed
#      dsnamelist            - name of file being processed (same as $dsname)
#
#   Imported Variables that must be passed in under certain conditions:
#      USHbufr               - path to non-obsproc ush directory possibly
#                              containing $EXECUTE (as a script) if it is not
#                              found either as a program in
#                              $EXECobsproc_satingest, as a script in
#                              $USHobsproc_satingest, or as a script or a
#                              program in $USERDIR (e.g., ingest_cloud_NASA.sh)
#                              (invoked only conditions above occur)
#      FIXbufr               - path to non-obsproc fix directory, here second
#                              choice to path to directory containing BUFR
#                               mnemonic table files bufrtab.XXX
#                              (invoked only if first choice path,
#                              $FIXobsproc_satingest, does not yield table;
#                              does not have to be passed in if BUFRTABLE is
#                              imported as either 'none', 'nulltable' or
#                              'nobufrtable')
#      COM_SSTOI             - leading portion of dated directory containing sst
#                              file (only needed for SSMI SDR processing)
#
#   Imported Variables that can be passed in:
#      TANKDIR               - root of directory path to output file or BUFR
#                              database tank file (e.g., "/dcom/us007003")
#                              (default = '$DATA')
#      PDY                   - preferred date of sst file used for SSMI SDR 
#                              processing.
#
# Condition codes:
#     0 - no problem encountered
#   > 0 - some problem encountered
#     Specifically: 191 - bad argument list       
#                   195 - directory $DATA does not exist
#                   196 - directory $TANKDIR does not exist
#                   198 - input file $DATA/$dsname does not exist
#                   244 - Neither $EXECobsproc_satingest/$EXECUTE nor
#                         $USHobsproc_satingest/$EXECUTE nor $USERDIR/$EXECUTE
#                         nor $USHbufr/$EXECUTE exists
#
# Attributes:
#   Language: ksh script
#   Machine:  NCEP WCOSS
#
####

set -au

echo
echo "#######################################################################"
echo "                START INGEST_TRANSLATE_ORBITS                          "
echo "#######################################################################"
echo

if [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ] ; then
  set -x
fi

host=$(hostname -s)

TANKDIR=${TANKDIR:-$DATA}
UPDATE_ERRLOG=YES  # UPDATE_ERRLOG is initialized to YES, write to errlog file
                   # if there is a problem
if [ ! -d $DATA ] ; then
   set +x
   echo
   echo "$0: Directory $DATA doesn't exist." 
   echo
   exit 195
fi
if [ ! -d $TANKDIR ] ; then
   set +x
   echo
   echo "$0: Directory $TANKDIR doesn't exist." 
   echo
   exit 196
fi
cd $TANKDIR

if [ -s $EXECobsproc_satingest/$EXECUTE ] ; then
   texec=$EXECobsproc_satingest/$EXECUTE
   outname=${EXECUTE}
   EXECTYPE=executable
   set +x
   echo
   echo "------------------------------------------------------------------\
------------------------------------------------------------------"
   echo "ingest_translate_orbits.sh executes PROGRAM $texec to perform \
translation." 
   echo "------------------------------------------------------------------\
------------------------------------------------------------------"
   echo
   set -x
elif [ -s $USHobsproc_satingest/$EXECUTE ] ; then
   texec=$USHobsproc_satingest/$EXECUTE
   outname=${EXECUTE%%.sh}
   EXECTYPE=script
   set +x
   echo
   echo "------------------------------------------------------------------\
------------------------------------------------------------------"
   echo "ingest_translate_orbits.sh executes SCRIPT $texec to perform \
translation." 
   echo "------------------------------------------------------------------\
------------------------------------------------------------------"
   echo
   set -x
elif [ -s $USERDIR/$EXECUTE ] ; then
   texec=$USERDIR/$EXECUTE
   outname=${EXECUTE%%.sh}
   EXECTYPE=script
   set +x
   echo
   echo "------------------------------------------------------------------\
------------------------------------------------------------------"
   echo "ingest_translate_orbits.sh executes SCRIPT $texec to perform \
translation." 
   echo "------------------------------------------------------------------\
------------------------------------------------------------------"
   echo
   set -x
   if [ $outname = $EXECUTE ] ; then
      outname=${EXECUTE}
      EXECTYPE=executable
      set +x
      echo
      echo "------------------------------------------------------------------\
------------------------------------------------------------------"
      echo "ingest_translate_orbits.sh executes PROGRAM $texec to perform \
translation." 
      echo "------------------------------------------------------------------\
------------------------------------------------------------------"
      echo
      set -x
   fi
elif [ -s $USHbufr/$EXECUTE ] ; then
   texec=$USHbufr/$EXECUTE
   outname=${EXECUTE%%.sh}
   EXECTYPE=script
   set +x
   echo
   echo "------------------------------------------------------------------\
------------------------------------------------------------------"
   echo "ingest_translate_orbits.sh executes SCRIPT $texec to perform \
translation." 
   echo "------------------------------------------------------------------\
------------------------------------------------------------------"
   echo
   set -x
else
   msg="##### ingest_translate_orbits.sh could find no eligible executable or \
script named $EXECUTE."
   set +x
   echo
   echo $msg
   echo
   set -x
cd $DATA   
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
   cd $TANKDIR
   exit 244
fi

outfile=$OUTDIR/$outname.out; tmpout=$DATA/$outname.tempout.$host.$$
file=$DATA/$dsname

# 1st choice path to BUFR table is $FIXobsproc_satingest, 2nd choice is FIXbufr
# -----------------------------------------------------------------------------
if [ $BUFRTABLE != none -a $BUFRTABLE != nulltable -a \
     $BUFRTABLE != nobufrtable ] ; then
   table=$FIXobsproc_satingest/$BUFRTABLE
   [ ! -s $table ]  &&  table=$FIXbufr/$BUFRTABLE
else
   table=/dev/null
fi

if [ $FORGNTABLE != none -a $FORGNTABLE != nulltable -a \
     $FORGNTABLE != noforgntable ] ; then
   forgntable=$FIXobsproc_satingest/$FORGNTABLE
else
   forgntable=/dev/null
fi

errlog=$OUTDIR/$outname.errlog
tmperr=$DATA/$outname.temperr.$host.$$

set $dsnamelist
nargs=$#
iargs=0
lenstring=0
lenerror=0
outstring=""

if [ $EXECTYPE = executable ] ; then
   if [ ${#TANKFILE} -eq 0 ] ; then
      typsubdir="."
   else
      typsubdir=$(dirname $TANKFILE)
   fi
   if [ $typsubdir != "." ] ; then
      subtypfil=$(basename $TANKFILE)
   else
      set +x
      echo
      echo "    <tankfile> must be given as : subdirectory/tankfile>" 
      echo "    in the following form:                bYYY/xxZZZ ."
      echo "    Last 3 digits of subdirectory are BUFR type." 
      echo "    Last 3 digits of tankfile are BUFR sub-type." 
      echo "    BUFR type and sub-type may be coded as three" 
      echo "    question marks if translation code provides" 
      echo "    BUFR type and sub-type." 
      echo
      exit 191
   fi
fi

while [ $iargs -lt $nargs ] ; do
   iargs=$(($iargs+1))
   eval dsname=\$$iargs
   if [ $EXECUTE = "bufr_trangoessst" ] ; then
#---------------------------------------------------------------------------
# Might this not be handled via the UNCOMPRESS_UNIX switch like other types?
#  (for consistency and better generalization)
#---------------------------------------------------------------------------

      if [ $(echo $DATA/$dsname | grep \.gz$) ]; then
         gunzip $DATA/$dsname
         err=$?
         if [ $err -eq 0 ];then
            dsname=${dsname%\.gz}
         else
            msg="***WARNING: Could not gunzip file $DATA/$dsname. Skip"
            $UTILROOT/ush/postmsg "$jlogfile" "$msg"
            exit $err
         fi
      fi
   fi
   eval outstring$iargs='"$EXECUTE will run on file $dsname"'
   eval len=\${#outstring$iargs}
   [ $len -gt $lenstring ]  &&  lenstring=$len
   file=$DATA/$dsname
   if [ ! -s $file ] ; then
      outstring="$dsname does not exist or has zero length"
      len=${#outstring}
      [ $len -gt $lenerror ]  &&  lenerror=$len
      echo $outstring >> $tmperr
   else
      if [ $EXECTYPE = executable ] ; then
         iunit=$((10+$iargs))
         export FORT$iunit="$file"
      fi
   fi
done

if [ ! -s $table ] ; then
   if [ $BUFRTABLE != none -a $BUFRTABLE != nulltable -a \
        $BUFRTABLE != nobufrtable ] ; then
      outstring="$table does not exist or has zero length"
      len=${#outstring}
      [ $len -gt $lenerror ]  &&  lenerror=$len
      echo $outstring  >> $tmperr
   fi 
else
   [ $EXECTYPE = executable ]  &&  export FORT20="$table"
fi

if [ ! -s $forgntable ] ; then
   if [ $FORGNTABLE != none -a $FORGNTABLE != nulltable -a \
        $FORGNTABLE != noforgntable ] ; then
      outstring="$forgntable does not exist or has zero length"
      len=${#outstring}
      [ $len -gt $lenerror ]  &&  lenerror=$len
      echo $outstring  >> $tmperr
   fi 
else
   [ $EXECTYPE = executable ]  &&  export FORT19="$forgntable"
fi

dashes="---------------------------------------------------------------"

if [ -s $outfile ] ; then
   lenoutfile=$(cat $outfile | wc -l)
else
   lenoutfile=0
fi

if [ -s $errlog ] ; then
   lenerrlog=$(cat $errlog | wc -l)
else
   lenerrlog=0
fi

if [ $lenerror -ne 0 ] ; then
   if [ $lenoutfile -gt 130000 ] ; then
      head -n 120000 $outfile > $outfile.head.$host.$$
      cat $tmpout $tmperr $outfile.head.$host.$$ > $outfile
      rm $outfile.head.$host.$$
   else
      cat $tmpout $tmperr $outfile > $outfile.temp.$host.$$
      mv $outfile.temp.$host.$$ $outfile
   fi
   dashstring=$(echo $dashes$dashes | cut -c1-$lenstring)
   echo $dashstring >> $tmpout
   iargs=0
   while [ $iargs -lt $nargs ] ; do
      iargs=$(($iargs+1))
      eval echo \$outstring$iargs >> $tmpout
   done
   echo $dashstring >> $tmpout
   cat $tmpout $tmperr
   rm $tmpout 
   dashstring=$(echo $dashes$dashes | cut -c1-$lenerror)
   echo $dashstring >> $tmperr
   if [ $UPDATE_ERRLOG = YES ]; then
      if [ $lenerrlog -gt 4000 ] ; then
         head -n 3000 $errlog > $errlog.head.$host.$$
         echo $dashstring | cat - $tmperr $errlog.head.$host.$$ > $errlog
         rm $errlog.head.$host.$$
      else
         echo $dashstring | cat - $tmperr $errlog > $errlog.temp.$host.$$
         mv $errlog.temp.$host.$$ $errlog
      fi
   fi
   rm $tmperr $file 
   exit 198  
fi

datestring="$EXECUTE will run on $(date -u '+%Y/%m/%d at %H:%M:%S') UTC"

lendatestring=${#datestring}
[ $lendatestring -gt $lenstring ]  &&  lenstring=$lendatestring

dashstring=$(echo $dashes$dashes | cut -c1-$lenstring)
echo $dashstring >> $tmpout

iargs=0
while [ $iargs -lt $nargs ] ; do
   iargs=$(($iargs+1))
   eval echo \$outstring$iargs >> $tmpout
done

if [ -s $DATA/orbitlist_fname_cat ]; then
   echo "   - this is a concatenation of files:" >> $tmpout
   cat $DATA/orbitlist_fname_cat | {
   read line
   rc=$?
   while [ $rc -eq 0 ] ; do
      set -A FILEINFO $line
      echo "     $FILEINFO" >> $tmpout
      read line
      rc=$?
   done }
fi

echo $datestring >> $tmpout
echo $dashstring >> $tmpout
cp $tmpout $tmperr

ymdh=$(date -u '+%Y%m%d%H')
if [ $TANKPROTECT = YES ] ; then
   apndstring=_A_P_N_$dsname
else
   apndstring="' '"
fi

if [ $EXECTYPE = executable ] ; then

#  ----------------------------------------
#  Come here if a PROGRAM is to be executed
#  ----------------------------------------

   pgm=$EXECUTE
cd $DATA   
   set +u
#  Note - must use "$UTILROOT/ush/prep_step" here not ". $UTILROOT/ush/prep_step" because the
#         latter would unset the FORT* variables that have previously been
#         been set.  These may still be used in subsequent programs in this
#         script.
#######   . $UTILROOT/ush/prep_step
   $UTILROOT/ush/prep_step
   set -u
   cd $TANKDIR

   export FORT51="$DATA/$typsubdir.$subtypfil.$dsname.tmpout.$host.$$"

   stdin="$typsubdir $subtypfil $apndstring $ymdh"

   second_tranjb_run=NO

   ssmi_sdr=1
   ssmi_sdr_err=0
   if [ $EXECUTE = bufr_transsmi ]; then
      echo $dsname | grep -q SDR
      ssmi_sdr=$?
      if [ ssmi_sdr -eq 0 ];then
 
         second_tranjb_run=YES

#  Special processing for SSMI SDR type
#  get sstgrb & sstgrb index & input into bufr_transsmi, along w/ land/sea tags
#  also processes NN3 products, output in unit 52
#  ----------------------------------------------------------------------------

         PDY=${PDY:-$(cat $COMROOT/date/t00z | cut -c7-14)}
         CHECK_DATES="$PDY $($UTILROOT/ush/finddate.sh $PDY s-9)"
         sst_found=false
# loop through dates to look for latest available version of this sst file
         for DDATE in $CHECK_DATES;do
            sstgrb=${COM_SSTOI:?}.$DDATE/sstoi_grb
            if [ -s $sstgrb ];then
               rm -f errfile $DATA/sstgrb $DATA/sstgrb.index
               cp $sstgrb $DATA/sstgrb
               $GRBINDEX $DATA/sstgrb $DATA/sstgrb.index 2>errfile
               err2=$?
               if [ $err2 -eq 0 ];then
                 sst_found=true
                 break
               else
                 cat errfile
                 echo grbindex failed for this file.  Try another.
               fi
            fi
         done
         rm -f errfile
         if [ $sst_found != true ]; then
            msg="***WARNING: NO USABLE SST FILE FOUND!!!"
            $UTILROOT/ush/postmsg "$jlogfile" "$msg"
            ssmi_sdr_err=99
         else
            export FORT31="$DATA/sstgrb"
            export FORT32="$DATA/sstgrb.index"
            export FORT33="$FIXobsproc_satingest/nesdis.lstags_transsmi"
            export FORT52=\
"$DATA/$typsubdir.second_tranjb_file.$dsname.tmpout.$host.$$"
            stdin="$typsubdir $subtypfil $apndstring $ymdh \nNOTIMLIM \
\n$typsubdir xx103 $apndstring $ymdh"
         fi
      fi

   elif [ $EXECUTE = "bufr_trantmi" ] ; then

#  Special processing for TRMM/TMI type (expects input file named tmi.hdf)
#  -----------------------------------------------------------------------

      rm -f $DATA/tmi.hdf
      cp $file $DATA/tmi.hdf
      export FORT$iunit="$DATA/tmi.hdf"

   elif [ $EXECUTE = "bufr_tranavhrr" ] ; then

#  Special processing for AVHRR type (uses land sea mask, runs BUFR_TRANJB
#  twice)
#  -----------------------------------------------------------------------

      second_tranjb_run=YES
      if [ $subtypfil = xx051 ]; then
         subtypfil_p1=xx052
      else
         subtypfil_p1=xx054
      fi
      export FORT37="$FIXobsproc_satingest/lnd_sea_mask_dat"
      export FORT52=\
"$DATA/$typsubdir.second_tranjb_file.$dsname.tmpout.$host.$$"
      stdin="$typsubdir $subtypfil $apndstring $ymdh \nNOTIMLIM \
\n$typsubdir $subtypfil_p1 $apndstring $ymdh"

   fi

   if [ $ssmi_sdr_err -eq 0 ]; then

      msg="$pgm has BEGUN"
      cd $DATA
      $UTILROOT/ush/postmsg "$jlogfile" "$msg"
##### cd $TANKDIR # if commented out prm will run in $DATA, not $TANKDIR

      echo -e $stdin|time -p $texec >> $tmpout 2>&1
      ier=$?
      if [ $ier -eq 0 ] ; then
         [ $EXECUTE = "bufr_trantmi" ]  &&  rm -f tmi.hdf
         give_rc="YES"
         cword="no"
         ksh $TRANush $TANKDIR \
          $DATA/$typsubdir.$subtypfil.$dsname.tmpout.$host.$$
         ier=$?
         if [ $second_tranjb_run = YES ]; then

#  SSMI SDR type runs a second BUFR_TRANJB to ingest NN3 products
#   (first BUFR_TRANJB run ingests brightness temperature data)
#  AVHRR type runs a second BUFR_TRANJB to ingest land or cloudy data
#   (first BUFR_TRANJB run ingests sea and clear data)
#  ------------------------------------------------------------------

            ksh $TRANush $TANKDIR \
$DATA/$typsubdir.second_tranjb_file.$dsname.tmpout.$host.$$
            rc2=$?
            retcode=$(($ier+$rc2))
            ier=$retcode
         fi
      fi
   else
      ier=$ssmi_sdr_err
   fi
else

#  ---------------------------------------
#  Come here if a SCRIPT is to be executed
#  ---------------------------------------

   if [ $EXECUTE = "ingest_cloud_NASA.sh" ]; then

#  Special processing for GOES_RADSND LGYCLD[1][2] type - this executes an
#  NCO/SIB script in $USHbufr directory path (ingest_cloud_NASA.sh) which
#  itself executes program DECOD_DCNCLD, whose path is defined in $DCNCLD
#  (which should be set in parent job script JIGOES_RADSND)

#    - this processing should obtain bufrtab.012 for use by DECOD_DCNCLD
#      from $FIXbufr as a first choice (with $FIXobsproc_satingest as a
#      second choice if bufrtab.012 not found in $FIXbufr) since message type
#      NC012150 is under NCO/SIB control
#      ==> $FIXbufr may now be exported from parent job script JIGOES_RADSND
#          with value "/dev/null" (see JIGOES_RADSND for more information).
#          If so, (or if it is not imported at all) it must be temporarily
#          reset to avoid a failure in this processing.  There are two choices
#          for $FIXbufr:
#           1st choice is directory /nwprod/decoders/decod_shared/fix and
#           2nd choice (if /nwprod/decoders/decod_shared/fix does not exist) is
#           directory /nwprod/fix
#      ------------------------------------------------------------------------
      export table_orig=$table
      export FIXbufr_orig=$FIXbufr
      if [ $FIXbufr = /dev/null -o ! -d $FIXbufr ]; then
         export DECDROOT=${DECDROOT:-/nwprod/decoders}
         export FIXbufr=$DECDROOT/decod_shared/fix
         [ ! -d $FIXbufr ] && export FIXbufr=/nwprod/fix # remove when bufrtab's no longer in this directory
      fi
      export table=$FIXbufr/$BUFRTABLE
      [ ! -s $table ]  &&  export table=$FIXobsproc_satingest/$BUFRTABLE

#    - newer versions of ingest_cloud_NASA.sh: currently execute
#      $USHbufr/tranjb (rather than $USHobsproc_satingest/bufr_tranjb.sh)
#      unless overidden by $TRANJBush, so export $TRANJBush as
#      $USHobsproc_satingest/bufr_tranjb.sh here (this will in turn execute
#      program $EXECobsproc_satingest/bufr_tranjb) (allows the
#      obsproc_satingest version of ush script bufr_tranjb.sh and program
#      bufr_tranjb to be executed here)
#    - older versions of ingest_cloud_NASA.sh: execute $USHbufr/tranjb (rather
#      than $USHobsproc_satingest/bufr_tranjb.sh) and $USHbufr/tranjb, in turn,
#      will execute the program $EXECbufr/bufr_tranjb unless overidden by
#      $TRANX (unlike $USHobsproc_satingest/bufr_tranjb.sh which executes the
#      program $EXECobsproc_satingest/bufr_tranjb unless also overridden by
#      $TRANX), so, override this in $USHbufr/tranjb by exporting TRANX to be
#      $EXECobsproc_satingest/bufr_tranjb (at least the obsproc_satingest
#      version of the bufr_tranjb program can be executed here)

#       export TRANJBush=$USHobsproc_satingest/bufr_tranjb.sh
#        -- previously defined in bufr_tranjb module
#        -- below only needed for older versions of ingest_cloud_NASA.sh but
#           won't hurt anything to add it for newer versions too
#       export TRANX=$EXECobsproc_satingest/bufr_tranjb
#        -- previously defined in bufr_tranjb module

#    - export RUN_TYPE as 'decoder' so program bufr_tranjb will likewise obtain
#      bufrtab.012 from $FIXbufr as a first choice (with
#      $FIXobsproc_satingest as a second choice if bufrtab.012 not found in
#      $FIXbufr) since message type NC012150 is under NCO/SIB control

      export RUN_TYPE=decoder

   fi

   ksh $texec $table $dsnamelist >> $tmpout 2>&1
   ier=$?

   if [ $EXECUTE = "ingest_cloud_NASA.sh" ]; then

#  If ingest_cloud_NASA.sh just ran, restore RUN_TYPE to 'satingest', $table
#  to original value and $FIXbufr to original value

      export RUN_TYPE=satingest
      export table=$table_orig
      export FIXbufr=$FIXbufr_orig
   fi
fi

datestring="$EXECUTE : rc = $ier on $(date -u '+%Y/%m/%d at %H:%M:%S') UTC"
lendatestring=${#datestring}

numbadate=$(grep "BAD DATE" $tmpout | wc -l)
if [ $numbadate -gt 100 ] ; then
   grep -v "BAD DATE" $tmpout > $tmpout.nobadate
   echo "$numbadate OCCURRENCES OF BAD DATE REMOVED on \
$(date -u '+%Y/%m/%d at %H:%M:%S') UTC" | cat - $tmpout.nobadate > $tmpout
   rm $tmpout.nobadate
fi

iargs=0
lenstring=0
while [ $iargs -lt $nargs ] ; do
   iargs=$(($iargs+1))
   eval dsname=\$$iargs
   eval outstring$iargs='"$EXECUTE has run on file $dsname"'
   eval len=\${#outstring$iargs}
   [ $len -gt $lenstring ]  &&  lenstring=$len
done

[ $lendatestring -gt $lenstring ]  &&  lenstring=$lendatestring

dashstring=$(echo $dashes$dashes | cut -c1-$lenstring)

[ $ier -ne 0 ]  &&  echo $dashstring >> $tmperr

echo $dashstring >> $tmpout

iargs=0
while [ $iargs -lt $nargs ] ; do
   iargs=$(($iargs+1))
   [ $ier -ne 0 ]  &&  eval echo \$outstring$iargs >> $tmperr
   eval echo \$outstring$iargs >> $tmpout
done

if [ $ier -ne 0 ] ; then
   echo $datestring >> $tmperr
   echo $dashstring >> $tmperr
fi
echo $datestring >> $tmpout
echo $dashstring >> $tmpout

if [ $lenoutfile -gt 130000 ] ; then
   head -n 120000 $outfile > $outfile.head.$host.$$
   cat $tmpout $outfile.head.$host.$$ > $outfile
   rm $outfile.head.$host.$$
else
   cat $tmpout $outfile > $outfile.temp.$host.$$
   mv $outfile.temp.$host.$$ $outfile
fi
cat $tmpout 
rm $tmpout
if [ $ier -ne 0 ] ; then

#### This has been disbled because incomplete files are not on the PDA server ####
#  If script ingest_script_omi.sh returns with r.c. = 2, do not update the
#   errlog (program BUFR_TRANOMI could not open the input HDF5 file - this is
#   not a true error because it is not yet ready for processing and will be
#   opened and processed the next time the ingest processing for the OMI ozone
#   family runs)
#  ---------------------------------------------------------------------------

#   [ $EXECUTE=ingest_script_omi.sh -a $ier -eq 2 ]  &&  UPDATE_ERRLOG=NO
#### This has been disbled because incomplete files are not on the PDA server ####
   if [ $UPDATE_ERRLOG = YES ]; then
      if [ $lenerrlog -gt 4000 ] ; then
         head -n 3000 $errlog > $errlog.head.$host.$$
         cat $tmperr $errlog.head.$host.$$ > $errlog
         rm $errlog.head.$host.$$
      else
         cat $tmperr $errlog > $errlog.temp.$host.$$
         mv $errlog.temp.$host.$$ $errlog
      fi
   fi
   rm $tmperr 
   exit $ier
elif [ $TANKPROTECT = YES ] ; then 
   iertot=0
   for apndfile in $(ls */$typsubdir/$subtypfil$apndstring) ; do
      tank=${apndfile%%$apndstring}
      mv $apndfile $tank 
      iertot=$(($iertot+$?))
   done
   exit $iertot
else
   exit $ier
fi

