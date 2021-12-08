#!/bin/ksh
# Run under ksh

####  UNIX Script Documentation Block
#
# Script name:   ingest_sncvgrib96.sh
#
# RFC contact:  Ling      org: NP22        date: 2018-12-06
#
# Abstract: Executes the program SNOW_SNO96GRB which reads in a global IMS
#   96th mesh NESDIS snow cover/sea ice file in ASCII format and converts it to
#   GRIB.  Then copies output to path wgrbbul/imssnow96.grb under date
#   determined by qualifier in input filename. Also creates GRIB2 version and
#   copies output to path wgrbbul/imssnow96.grb.grib2 under date determined by
#   qualifier in input filename.
#
# Script history log:
# 2005-05-16  Geo  Gayno  Original version for implementation (based on
#                         ingest_sncvgrib.sh).
# 2006-05-12  D. Keyser   Improved DOCBLOCK, comments, stdout, messages posted
#                         to joblog, message posted to file-processing history
#                         file.
# 2008-05-29  P. O'Reilly Modified to begin creating and alerting file
#                         imssnow96.grb.grib2 in GRIB2 format.  This is in
#                         support of transition to GRIB2 datasets on NWS and
#                         NCEP ftp servers.
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
# 2012-12-06  D.C. Stokes Minor modifications for WCOSS and use NCO util to
#                         compute date from day of year. Executable renamed
#                         from bufr_sno96grb to snow_sno96grb.
# 2013-12-06  D.C. Stokes/D.A. Keyser Moved directory path to utility
#                         executable cnvgrib from hardwired /nwprod/util/exec
#                         to environment variable $utilexec set in upstream job
#                         script (now same as utility executable ndate which
#                         previously used $utilexec as its directory path).
#                         All references to "GMT" changed to "UTC".  Imported
#                         variable $timetype now checked for value of "UTC"
#                         rather than "GMT" ($timetype redefined to be "UTC"
#                         when time type is Greenwich).  $EXECsatingest
#                         replaces $EXECbufr as the environment variable
#                         representing the directory path to the executables.
#                         Added information to docblock and new comments.
#                         Updated some existing comments.
# 2014-12-12  D.C. Stokes Update for IMSv3 filenames (including call to gunzip).
#                         Use variable $UTILROOT/ush for path to date2jday.sh.
#                         Add call to gunzip if needed.
# 2015-11-09  D. Keyser   HISTLENMIN and HISTLENMAX now used to define minimum/
#                         maximum length (in lines) of the imssnow96.grb.history
#                         and imssnow96.grb.grib2.history files.  These history
#                         files had been growing into perpetuity.
# 2018-12-06  Y. Ling     Updated to run on phase 3 machines.
# 2021-01-21  S.Stegall   Deprecated SENDDBN_GB2 to SENDDBN.
#                         [ $SENDDBN = YES ] changed to [ "$SENDDBN" = YES ]
#                         (quotes added)
#
#
# Usage: ingest_sncvgrib96.sh  <dummy>  <raw_file>
#
#   Script parameters:
#                 $1: dummy    - dummy (not used by this script)
#                 $2: raw_file - name of input IMS snow cover/sea ice ascii
#                                file (in form imsYYYYDDD.asc)
#
#   Modules and files referenced:
#     scripts    : $DATA/postmsg
#                  $DATA/prep_step
#                  $UTILROOT/ush/date2jday.sh
#                  $UTILROOT/ush/finddate.sh
#     data cards : none
#     executables: $EXECsatingest/snow_sno96grb
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
#      USERDIR                - path to directory containing file-processing
#                               history file (e.g., "$TANKDIR/ingest_hist")
#      EXECsatingest  - path to satingest executable directory
#      $UTILROOT/ush          - path to utility script directory containing
#                               date2jday.sh and finddate.sh
#      SENDDBN                - if set to "YES", issue dbnet_alert for GRIB
#                               and GRIB2 file(s)
#      SENDDBN_GB2            - if set to "YES", issue dbnet_alert for GRIB2
#                               file(s); Deprecated to SENDDBN
#      DBNROOT                - root path to dbn_alert (e.g.,
#                               "/iodprod/dbnet_siphon")
#                               (invoked only if either $SENDDBN or $SENDDBN_GB2
#                               is "YES")
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
#     Specifically:  95 - SNOW_SNO96GRB could not read standard input in unit 05
#                         (condition code coming out of
#                          $EXECsatingest/snow_sno96grb)
#                    96 - SNOW_SNO96GRB had bad write to grib file in unit 51
#                         (condition code coming out of
#                          $EXECsatingest/snow_sno96grb)
#                    97 - SNOW_SNO96GRB had bad open to grib file in unit 51
#                         (condition code coming out of
#                          $EXECsatingest/snow_sno96grb)
#                    98 - SNOW_SNO96GRB had unexpected end-of-file reading
#                         in IMS snow cover/sea ice ascii file in unit 11
#                         (condition code coming out of
#                          $EXECsatingest/snow_sno96grb)
#                    99 - SNOW_SNO96GRB found unrecognized data value in IMS
#                         snow cover/sea ice ascii file in unit 11
#                         (condition code coming out of
#                          $EXECsatingest/snow_sno96grb)
#
# Attributes:
#   Language: ksh script
#   Machine:  NCEP WCOSS
####

set -au

echo
echo "#######################################################################"
echo "                   START INGEST_SNCVGRIB96.SH                          "
echo "#######################################################################"
echo

if [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ] ; then
   set -x
fi

#  Determine data YYYYMMDD based on qualifier in name of IMS file
#    eg: NIC.IMS_v3_201434600_24km.asc
#  A 00Z file represents ~22Z previous day, so backdate accordingly
#  ----------------------------------------------------------------
yyyydddhh_fname=$(echo $2 | cut -d"_" -f3 | cut -c1-9)
yyyyddd_fname=$(echo $yyyydddhh_fname | cut -c1-7)
hh_fname=$(echo $yyyydddhh_fname | cut -c8-9)
yyyymmdd_fname=`$UTILROOT/ush/date2jday.sh $yyyyddd_fname`
if [ $hh_fname == 00 ]; then
  yyyymmdd=`$UTILROOT/ush/finddate.sh $yyyymmdd_fname d-1`
  yyyyddd=`$UTILROOT/ush/date2jday.sh $yyyymmdd`
else
  yyyymmdd=$yyyymmdd_fname
  yyyyddd=$yyyyddd_fname
fi

if [ ! -d $TANKDIR/$yyyymmdd/wgrbbul ] ; then
   mkdir -m 775 -p $TANKDIR/$yyyymmdd/wgrbbul
fi

# change to working directory

cd $DATA

if [ $(echo $2 | grep \.gz$) ]; then
  gunzip $2
  err=$?
  if [ $err -ne 0 ];then
     msg="***WARNING: Could not gunzip file $2.  Skip."
     $DATA/postmsg "$jlogfile" "$msg"
     exit $err
  fi
  raw_file=${2%\.gz}
else
  raw_file=$2
fi

pgm=snow_sno96grb
set +u
$DATA/prep_step
set -u
export FORT11="$raw_file"
export FORT51="$DATA/imssnow96.grb"

msg="$pgm start for $yyyymmdd data"
$DATA/postmsg "$jlogfile" "$msg"

echo $yyyyddd | $EXECsatingest/snow_sno96grb
err=$?
#err_chk

if [ $err -eq 0 ]; then
   cp $DATA/imssnow96.grb $TANKDIR/$yyyymmdd/wgrbbul/imssnow96.grb
   msg="$pgm completed normally"
   $DATA/postmsg "$jlogfile" "$msg"
   echo "imssnow96.grb (for $yyyymmdd) CREATED and WRITTEN to \
$TANKDIR/$yyyymmdd/wgrbbul AT `date -u +%Y/%m/%d' '%H:%M:%S' UTC'`" \
    >> $USERDIR/imssnow96.grb.history
   msg="imssnow96.grb CREATED for $yyyymmdd"
   $DATA/postmsg "$jlogfile" "$msg"
   if [ -s $DATA/imssnow96.grb ]; then
       $CNVGRIB -g12 -p40 \
       $TANKDIR/$yyyymmdd/wgrbbul/imssnow96.grb $DATA/imssnow96.grb.grib2
      if [ -s $DATA/imssnow96.grb.grib2 ] ; then
         cp $DATA/imssnow96.grb.grib2 \
          $TANKDIR/$yyyymmdd/wgrbbul/imssnow96.grb.grib2
         echo "imssnow96.grb.grib2 (for $yyyymmdd) CREATED and WRITTEN to \
$TANKDIR/$yyyymmdd/wgrbbul AT `date -u +%Y/%m/%d' '%H:%M:%S' UTC'`" \
          >> $USERDIR/imssnow96.grb.grib2.history
         msg="imssnow96.grb.grib2 CREATED for $yyyymmdd"
         $DATA/postmsg "$jlogfile" "$msg"
      fi
      if [ "$SENDDBN" = YES ]; then
         if [ -s $TANKDIR/$yyyymmdd/wgrbbul/imssnow96.grb ]; then
            $DBNROOT/bin/dbn_alert MODEL IMSSNOW96_GB $job \
             $TANKDIR/$yyyymmdd/wgrbbul/imssnow96.grb
         fi
         if [ -s $TANKDIR/$yyyymmdd/wgrbbul/imssnow96.grb.grib2 ]; then
            $DBNROOT/bin/dbn_alert MODEL IMSSNOW96_GB_GB2 $job \
             $TANKDIR/$yyyymmdd/wgrbbul/imssnow96.grb.grib2
         fi
      fi
   fi
fi

rm -f $raw_file  # remove IMS snow cover/sea ice file from temporary working directory

#  If a new file was successfully created and written to its appropriate date's
#   wgrbbul directory, check to see if the current day is actually one day
#   later than this directory date - if so, and if there is a file already in
#   the wgrbbul directory for the current day, then copy this new file forward
#   from yesterday to the current day, the logic being that it is replacing a
#   "stale" file which is more than one day old.
#  ----------------------------------------------------------------------------

gribrc=$err
if [ $gribrc -eq 0 ] ; then
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
      if [ -s $TANKDIR/$currdate/wgrbbul/imssnow96.grb ] ; then
         cp $TANKDIR/$yyyymmdd/wgrbbul/imssnow96.grb \
          $TANKDIR/$currdate/wgrbbul/imssnow96.grb
         echo "imssnow96.grb (for $currdate) COPIED (from $yyyymmdd), \
REPLACING OLDER COPY, AT `date -u +%Y/%m/%d' '%H:%M:%S' UTC'`" \
          >> $USERDIR/imssnow96.grb.history
         msg="&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&imssnow96.grb copied from \
$TANKDIR/$yyyymmdd/wgrbbul to $TANKDIR/$currdate/wgrbbul , replacing older copy"
         set +x
         echo
         echo $msg
         echo
         [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
         $DATA/postmsg "$jlogfile" "$msg"
      fi
      if [ -s $TANKDIR/$currdate/wgrbbul/imssnow96.grb.grib2 ] ; then
         cp $TANKDIR/$yyyymmdd/wgrbbul/imssnow96.grb.grib2 \
          $TANKDIR/$currdate/wgrbbul/imssnow96.grb.grib2
         echo "imssnow96.grb.grib2 (for $currdate) COPIED (from $yyyymmdd), \
REPLACING OLDER COPY, AT `date -u +%Y/%m/%d' '%H:%M:%S' UTC'`" \
          >> $USERDIR/imssnow96.grb.grib2.history
         msg="&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&imssnow96.grb.grib2 copied from \
$TANKDIR/$yyyymmdd/wgrbbul to $TANKDIR/$currdate/wgrbbul , replacing older copy"
         set +x
         echo
         echo $msg
         echo
         [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x
         $DATA/postmsg "$jlogfile" "$msg"
      fi
   fi
fi

if [ -s $USERDIR/imssnow96.grb.history ] ; then
   grib_histlen=$(wc -l < $USERDIR/imssnow96.grb.history)
   if [ $grib_histlen -gt $HISTLENMAX ] ; then
      grib_histcut=$(($grib_histlen-$HISTLENMIN))
      awk ' BEGIN   { nlines = 0 }
                    {
                      nlines = nlines + 1;
                      if(nlines>'$grib_histcut') print $0
                    }
          ' $USERDIR/imssnow96.grb.history > $USERDIR/imssnow96.grb.newhist.$host.$$
      mv $USERDIR/imssnow96.grb.newhist.$host.$$ $USERDIR/imssnow96.grb.history
   fi
fi
if [ -s $USERDIR/imssnow96.grb.grib2.history ] ; then
   grib2_histlen=$(wc -l < $USERDIR/imssnow96.grb.grib2.history)
   if [ $grib2_histlen -gt $HISTLENMAX ] ; then
      grib2_histcut=$(($grib2_histlen-$HISTLENMIN))
      awk ' BEGIN   { nlines = 0 }
                    {
                      nlines = nlines + 1;
                      if(nlines>'$grib2_histcut') print $0
                    }
          ' $USERDIR/imssnow96.grb.grib2.history > $USERDIR/imssnow96.grb.grib2.newhist.$host.$$
      mv $USERDIR/imssnow96.grb.grib2.newhist.$host.$$ $USERDIR/imssnow96.grb.grib2.history
   fi
fi

exit $err
