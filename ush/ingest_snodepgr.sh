#!/bin/ksh
# Run under ksh 

####  UNIX Script Documentation Block
#
# Script name:   ingest_snodepgr.sh
#
# RFC contact:  Ling      org: NP22        date: 2018-12-06
#
# Abstract: Executes the program SNOW_SNO8GRB which reads in a hemispheric
#   8th mesh USAF snow depth/sea ice file in binary format and converts it to
#   GRIB.  Then copies output to path wgrbbul/snowdepth.grb under date
#   determined by qualifier in input filename.  Also creates GRIB2 version and
#   copies output to path wgrbbul/snowdepth.grb.grib2 under date determined by
#   qualifier in input filename.  Optionally, also copies input file to name
#   indicated by imported variable TARGETFILE in directory indicated by
#   imported variable TANKFILE, again under date determined by qualifier in
#   input filename.
#
# Script history log:
# 1998-12-08  Bert Katz   Original version for implementation.
# 2006-05-12  D. Keyser   Improved DOCBLOCK, comments, stdout, messages posted
#                         to joblog, message posted to file-processing history
#                         file; tests imported TARGETFILE, if not "NO", will
#                         copy input file to TARGETFILE name in directory
#                         TANKFILE under date determined by qualifier in input
#                         filename.
# 2006-10-27  P. O'Reilly Modified script to have bufr_sno8grb write output
#                         file to working directory instead of directly to
#                         /dcom, then write output to /dcom if no error occurs.
#                         Also, added DBNet alert for snowdepth.grb file.
# 2008-01-25  P. O'Reilly Modified to begin creating and alerting file
#                         snowdepth.grb in GRIB2 format.  This is in support of
#                         transition to GRIB2 datasets on NWS and NCEP ftp
#                         servers.
# 2012-12-06  D.C. Stokes Minor modifications for WCOSS and use NCO util to
#                         compute date from day of year. Executable renamed
#                         from bufr_sno8grb to snow_sno8grb.
# 2012-12-06  D. Keyser   Improved comments, stdout, messages posted to joblog,
#                         message posted to file-processing history file.  For
#                         cases where a new GRIB or GRIB2 file was successfully
#                         created and written to its appropriate date's wgrbbul
#                         directory, now checks to see if the current day is
#                         actually one day later than this directory date - if
#                         so, and if there is a GRIB or GRIB2 file already in
#                         the wgrbbul directory for the current day, then
#                         copies this new file forward from yesterday to the
#                         current day, the logic being that it is replacing a
#                         "stale" file which is more than one day old.
# 2013-12-06  D.C. Stokes/D.A. Keyser Moved directory path to utility
#                         executable cnvgrib from hardwired /nwprod/util/exec
#                         to environment variable $utilexec set in upstream job
#                         script (now same as utility executable ndate which
#                         previously used $utilexec as its directory path).
#                         All references to "GMT" changed to "UTC".  Imported
#                         variable $timetype now checked for value of "UTC"
#                         rather than "GMT" ($timetype redefined to be "UTC"
#                         when time type is Greenwich).  $EXECobsproc_satingest
#                         replaces $EXECbufr as the environment variable
#                         representing the directory path to the executables.
#                         Added information to docblock and new comments.
#                         Updated some existing comments.
# 2015-11-09  D. Keyser   HISTLENMIN and HISTLENMAX now used to define minimum/
#                         maximum length (in lines) of the snowdepth.grb.history
#                         and snowdepth.grb.grib2.history files.  These history
#                         files had been growing into perpetuity.
# 2018-12-06  Y. Ling     Updated to run on phase 3 machine.
# 2020-11-10  S. Stegall  $SENDDBN tests changed to be enclosed in quotes.
#                         References to SENDDBN_GB2 were removed due to
#                         discontinuation of deliberate separation of grib and
#                         grib2 alerts.
#
# Usage: ingest_snodepgr.sh  <dummy>  <raw_file>
#
#   Script parameters:
#                 $1: dummy    - dummy (not used by this script)
#                 $2: raw_file - name of input USAF snow depth/sea ice binary
#                                file (in form
#                                PRD.SPPROD.SNODEPH.N{S}HMAMAP.Dyyddd)
#
#   Modules and files referenced:
#     scripts    : $UTILROOT/ush/postmsg
#                  $UTILROOT/ush/prep_step
#                  $UTILROOT/ush/date2jday.sh
#     data cards : none
#     executables: $EXECobsproc_satingest/snow_sno8grb
#                  $CNVGRIB
#                  $NDATE
#
# Remarks: Invoked by the script ingest_translate_orbits.sh.
#
#   Imported Variables that must be passed in:
#      DATA                   - path to current working directory
#      jlogfile               - path to joblog file
#      DEBUGSCRIPTS           - if set to "ON" or "YES", will run with "set -x"
#                               on (intended for debugging)
#      TANKDIR                - root of directory path to output file (e.g.,
#                               "/dcom/us007003")
#      TANKFILE               - directory in $TANKDIR/<YYYYMMDD> containing
#                               output file (e.g., "wgrdbul")
#      TARGETFILE             - name of output file in
#                               $TANKDIR/<YYYYMMDD>/$TANKFILE
#                               (e.g., "PRD.SPPROD.SNODEPH.SHMAMAP")
#      USERDIR                - path to directory containing file-processing
#                               history file (e.g., "$TANKDIR/ingest_hist")
#      EXECobsproc_satingest  - path to obsproc_satingest executable  directory
#      SENDDBN                - if set to "YES", issue dbnet_alert for GRIB
#                               and GRIB2 file(s)
#      SENDDBN_GB2            - if set to "YES", issue dbnet_alert for GRIB2
#                               file(s); deprecated to SENDDBN
#      DBNROOT                - root path to dbn_alert (e.g.,
#                               "/iodprod/dbnet_siphon")
#                               (invoked only if either $SENDDBN or
#                               $SENDDBN_GB2 is "YES")
#      timetype               - if set to "LOCAL", all times for processing
#                               will be assumed to be in local time, if set to
#                               "UTC", all times for processing will be assumed
#                               to be in UTC, if set to "USER_SPEC", the
#                               calling program is specifying a "hardwired"
#                               time for processing using the script variable
#                               $user_spec_timedatecurr (see below)
#      user_spec_timedatecurr - hardwired time which will be used as the time
#                               in all processing, it is specified in the form
#                               "yyyymmdd doy dow HHMM", where "yyyy" is year,
#                               "mm" is month, "dd" is day of month, "doy" is
#                               day of year, "dow" is day of week (1=Monday,
#                               ..., 7=Sunday), "HH" is hour and "MM" is
#                               minute.  This is only invoked if timetype is
#                               set to "USER_SPEC" (see above)
#
#   Condition codes:
#     0 - no problem encountered
#   > 0 - some problem encountered
#     Specifically:  99 - Input file PRD.SPPROD.SNODEPH.NHMAMAP not found, or
#                         some type of error in
#                         $EXECobsproc_satingest/snow_sno8grb
#
# Attributes:
#   Language: ksh script
#   Machine:  NCEP WCOSS
####

set -au

echo
echo "#######################################################################"
echo "                   START INGEST_SNODEPGR.SH                            "
echo "#######################################################################"
echo

if [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ] ; then
   set -x
fi

#  Determine data YYYYMMDD based on qualifier in name of USAF file
#  ---------------------------------------------------------------

yy=$(echo $2 | cut -d"." -f5 | cut -c2-3)
ddd=$(echo $2 | cut -d"." -f5 | cut -c4-6)
if [ $yy -gt 60 ]; then
   yyyyddd=19${yy}${ddd}
else
   yyyyddd=20${yy}${ddd}
fi

yyyymmdd=`$UTILROOT/ush/date2jday.sh $yyyyddd`

# change to working directory



bufrerror=0
if [ $TARGETFILE != NO ] ; then
   if [ ! -d $TANKDIR/$yyyymmdd/$TANKFILE ] ; then
      mkdir -m 775 -p $TANKDIR/$yyyymmdd/$TANKFILE
   fi
   cp $2 $TANKDIR/$yyyymmdd/$TANKFILE/$TARGETFILE
   bufrerror=$?
   if [ $bufrerror -eq 0 ] ; then
      echo "$dsname_full RECEIVED FROM REMOTE MACHINE, $TARGETFILE (for \
$yyyymmdd) COPIED AT `date -u +%Y/%m/%d' '%H:%M:%S' UTC'`" >> \
       $USERDIR/$TARGETFILE.history
      msg="$dsname_full received from remote unix machine, $TARGETFILE \
copied for $yyyymmdd"
      $UTILROOT/ush/postmsg "$jlogfile" "$msg"
   fi
else
   bufrerror=99
fi
[ $bufrerror -ne 0 ] && exit $bufrerror

if [ -s $2 ] ; then
   if [ ! -d $TANKDIR/$yyyymmdd/wgrbbul ] ; then
      mkdir -m 775 $TANKDIR/$yyyymmdd/wgrbbul
   fi

   pgm=snow_sno8grb
   set +u
   . $UTILROOT/ush/prep_step
   set -u
   export FORT11="$2"
   export FORT51="$DATA/snowdepth.grb"

   msg="$pgm start for $yyyymmdd data"
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"

   $EXECobsproc_satingest/snow_sno8grb
   err=$?
   #$UTILROOT/ush/err_chk

   if [ $err -eq 0 ]; then
      cp $DATA/snowdepth.grb $TANKDIR/$yyyymmdd/wgrbbul/snowdepth.grb
      msg="$pgm completed normally"
      $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      echo "snowdepth.grb (for $yyyymmdd) CREATED and WRITTEN to \
$TANKDIR/$yyyymmdd/wgrbbul AT `date -u +%Y/%m/%d' '%H:%M:%S' UTC'`" \
       >> $USERDIR/snowdepth.grb.history
      msg="snowdepth.grb CREATED for $yyyymmdd"
      $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      if [ "$SENDDBN" = YES ]; then
         if [ -s $TANKDIR/$yyyymmdd/wgrbbul/snowdepth.grb ]; then
            $DBNROOT/bin/dbn_alert MODEL SNOW_GB $job \
            $TANKDIR/$yyyymmdd/wgrbbul/snowdepth.grb
         fi
      fi
      $CNVGRIB -g12 -p40 $DATA/snowdepth.grb $DATA/snowdepth.grb.grib2
      if [ -s $DATA/snowdepth.grb.grib2 ] ; then
         cp $DATA/snowdepth.grb.grib2 \
          $TANKDIR/$yyyymmdd/wgrbbul/snowdepth.grb.grib2
         echo "snowdepth.grb.grib2 (for $yyyymmdd) CREATED and WRITTEN to \
$TANKDIR/$yyyymmdd/wgrbbul AT `date -u +%Y/%m/%d' '%H:%M:%S' UTC'`" \
          >> $USERDIR/snowdepth.grb.grib2.history
         msg="snowdepth.grb.grib2 CREATED for $yyyymmdd"
         $UTILROOT/ush/postmsg "$jlogfile" "$msg"
         if [ "$SENDDBN" = YES ] ; then
            if [ -s $TANKDIR/$yyyymmdd/wgrbbul/snowdepth.grb.grib2 ]; then
               $DBNROOT/bin/dbn_alert MODEL SNOW_GB_GB2 $job \
                $TANKDIR/$yyyymmdd/wgrbbul/snowdepth.grb.grib2
            fi
         fi
      fi
   fi
   rm -f $2  # remove USAF snow depth/sea ice file from temporary working dcty

#  If a new file was successfully created and written to its appropriate date's
#   wgrbbul directory, check to see if the current day is actually one day
#   later than this directory date - if so, and if there is a file already in
#   the wgrbbul directory for the current day, then copy this new file forward
#   from yesterday to the current day, the logic being that it is replacing a
#   "stale" file which is more than one day old.
#  ----------------------------------------------------------------------------

   if [ $timetype = UTC ] ; then
      currdate=$(date -u '+%Y%m%d')
   elif [ $timetype = LOCAL ] ; then
      currdate=$(date '+%Y%m%d')
   else
      currdate=`echo $user_spec_timedatecurr | cut -f1 -d" "`
   fi
   nextdatehr=$($NDATE 24 ${yyyymmdd}00)
   nextdate=$(echo -e "$nextdatehr/100 \n quit" | bc)
   if [ $currdate = $nextdate ] ; then
      if [ -s $TANKDIR/$currdate/wgrbbul/snowdepth.grb ] ; then
         cp $TANKDIR/$yyyymmdd/wgrbbul/snowdepth.grb \
          $TANKDIR/$currdate/wgrbbul/snowdepth.grb
         echo "snowdepth.grb (for $currdate) COPIED (from $yyyymmdd), \
REPLACING OLDER COPY, AT `date -u +%Y/%m/%d' '%H:%M:%S' UTC'`" \
          >> $USERDIR/snowdepth.grb.history
         msg="&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&snowdepth.grb copied from \
$TANKDIR/$yyyymmdd/wgrbbul to $TANKDIR/$currdate/wgrbbul , replacing older copy"
         set +x
         echo
         echo $msg
         echo
         [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
         $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      fi
      if [ -s $TANKDIR/$currdate/wgrbbul/snowdepth.grb.grib2 ] ; then
         cp $TANKDIR/$yyyymmdd/wgrbbul/snowdepth.grb.grib2 \
          $TANKDIR/$currdate/wgrbbul/snowdepth.grb.grib2
         echo "snowdepth.grb.grib2 (for $currdate) COPIED (from $yyyymmdd), \
REPLACING OLDER COPY, AT `date -u +%Y/%m/%d' '%H:%M:%S' UTC'`" \
          >> $USERDIR/snowdepth.grb.grib2.history
         msg="&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&snowdepth.grb.grib2 copied from \
$TANKDIR/$yyyymmdd/wgrbbul to $TANKDIR/$currdate/wgrbbul , replacing older copy"
         set +x
         echo
         echo $msg
         echo
         [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
         $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      fi
   fi
else
   msg="Input file $2 not found - program SNOW_SNO8GRB was not run \
--> non-fatal"
   $UTILROOT/ush/postmsg "$jlogfile" "$msg"
   exit 99
fi

if [ -s $USERDIR/snowdepth.grb.history ] ; then
   grib_histlen=$(wc -l < $USERDIR/snowdepth.grb.history)
   if [ $grib_histlen -gt $HISTLENMAX ] ; then
      grib_histcut=$(($grib_histlen-$HISTLENMIN))
      awk ' BEGIN   { nlines = 0 }
                    {
                      nlines = nlines + 1;
                      if(nlines>'$grib_histcut') print $0
                    }
          ' $USERDIR/snowdepth.grb.history > $USERDIR/snowdepth.grb.newhist.$host.$$
      mv $USERDIR/snowdepth.grb.newhist.$host.$$ $USERDIR/snowdepth.grb.history
   fi
fi
if [ -s $USERDIR/snowdepth.grb.grib2.history ] ; then
   grib2_histlen=$(wc -l < $USERDIR/snowdepth.grb.grib2.history)
   if [ $grib2_histlen -gt $HISTLENMAX ] ; then
      grib2_histcut=$(($grib2_histlen-$HISTLENMIN))
      awk ' BEGIN   { nlines = 0 }
                    {
                      nlines = nlines + 1;
                      if(nlines>'$grib2_histcut') print $0
                    }
          ' $USERDIR/snowdepth.grb.grib2.history > $USERDIR/snowdepth.grb.grib2.newhist.$host.$$
      mv $USERDIR/snowdepth.grb.grib2.newhist.$host.$$ $USERDIR/snowdepth.grb.grib2.history
   fi
fi

exit
