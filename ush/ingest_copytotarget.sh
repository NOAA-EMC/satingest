#!/bin/ksh
# Run under ksh

##########################################################################
# THIS SCRIPT IS SOURCED BY USH SCRIPT ingest_process_orbits.sh
#  this allows all variables exported from this script to be used
#  by ingest_process_orbits.sh
#
# THE DOCBLOCK IN ingest_process_orbits.sh also pertains to this script.
##########################################################################

####  UNIX Script Documentation Block
#
# Script name:   ingest_copytotarget.sh
#
# RFC contact:  Nadiga               org: NP22        date: 2019-10-09
#
# Abstract: Runs inside ingest_process_orbits.sh (sourced).  Performs the file
#   processing.
#
# Script history log:
# 2015-11-17  Dennis Keyser  Original version for implementation.  Contains a
#                            large portion of logic moved from
#                            ingest_process_orbits.sh that deals with nitty-
#                            gritty file processing. Sourced inside
#                            ingest_process_orbits.sh at two different
#                            locations based on the value of new environment
#                            variable PROC_MULT_FILES.  Facilitates changes in
#                            ingest_process_orbits.sh needed to handle
#                            processing when PROC_MULT_FILES=YES vs. when
#                            PROC_MULT_FILES=NO.
# 2019-10-09  Sudhir Nadgia  Modified to shift the Y2K windowing technique that
#                            converts 2-digit years to 4-digit.
# 2021-12-19  Sudhir Nadiga  Modified to use bufr_tranjb module variables.
# 2022-01-18  S. Stegall     Replaced $DATA/ before calling utility scripts and 
#                            instead used $UTILROOT/ush/ to properly leverage
#                            the prod_util module.
# 2023-04-02 Sudhir.Nadiga   Copied ingest_process_orbits_subscripts.sh to a new 
#                            script called ingest_copytotarget.sh to process GMASI files
# Usage: . ingest_copytotarget.sh (inside ingest_process_orbits.sh)
#
# Remarks: See Docblock in ingest_process_orbits.sh.
#
# Attributes:
#   Language: ksh script
#   Machine:  NCEP WCOSS
#
####

   set +x
echo
echo "......................................................................."
echo "            START INGEST_COPYTOTARGET.SH (sourced)         "
echo "......................................................................."
echo


EXECUTE=copy_to_target

echo $EXECUTE

   [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x

   if [ $toterr -eq 0 ] ; then

      if [ $EXECUTE = nullexec ] ; then
         bufrerror=0
      elif [ $EXECUTE = copy_to_target ] ; then
         bufrerror=0

#  COPY_TO_TARGET
#  --------------

         if [ $TARGETFILE != NO ] ; then

            if [ $TARGETFILE = same_name ] ; then
               if [ ! -d $TANKDIR/$TANKFILE ] ; then
                  mkdir -m 775 -p $TANKDIR/$TANKFILE
               fi
               cp $DATA/$dsname $TANKDIR/$TANKFILE/$dsname
#               cp $dsname $TANKDIR/$TANKFILE/$dsname
               bufrerror=$?
               if [ $bufrerror -eq 0 ] ; then
                  echo "$dsname_full SIMPLY COPIED TO $TANKDIR/$TANKFILE AT \
`date -u +%Y/%m/%d' '%H:%M:%S' UTC'`" >> ${ORBITLIST}.${TANKFILE}.history
                  msg="$dsname_full received from remote unix machine and \
simply copied to $TANKDIR/$TANKFILE/$dsname"
                  $UTILROOT/ush/postmsg "$jlogfile" "$msg"
               fi

            else

#  Need to obtain YYYYMMDD from file name, first look for a qualifier with the
#   date in the form Dyyddd
#  ---------------------------------------------------------------------------

               num_qual=`echo $dsname | awk -F"." '{print NF}'`
               i=1
               while [ $i -le $num_qual ]; do
                  qual=$(echo $dsname | cut -d"." -f${i})
                  if [ `echo $qual | cut -c1` = D ]; then
                     inum=${#qual}
                     if [ $inum -eq 6 ]; then
                        qual1=`echo $qual | cut -c2-6`
                        if [[ ! -z $( echo $qual1 | grep "^[0-9]*$" ) ]]; then
                           if [ `echo $qual | cut -c2-3` -ge 0 -a \
                                `echo $qual | cut -c2-3` -le 99 ]; then
                              if [ `echo $qual | cut -c4-6` -ge 0 -a \
                                   `echo $qual | cut -c4-6` -le 366 ]; then
                                 yy=$(echo $qual | cut -c2-3)
                                 ddd=$(echo $qual | cut -c4-6)
                                 break
                              fi
                           fi
                        fi
                     fi
                  fi
                  i=`expr $i + 1`
               done

               if [ $yy -eq -99 ]; then

#  No luck, next look for a qualifier with the date in the form mmddyyyy (can
#   come only after a "_" delimiter, but may have a "." in column 9)
#  --------------------------------------------------------------------------

                  num_qual=`echo $dsname | awk -F"_" '{print NF}'`
                  i=1
                  while [ $i -le $num_qual ]; do
                     qual=$(echo $dsname | cut -d"_" -f${i})
                     inum=${#qual}
                     if [ $inum -eq 8 -o \
                     \( "$inum" -gt '8' -a "`echo $qual | cut -c9`" = '.' \) ]
                     then
                        qual1_8=`echo $qual | cut -c1-8`
                        if [[ ! -z $( echo $qual1_8 | grep "^[0-9]*$" ) ]]; then
                           if [ `echo $qual1_8 | cut -c5-8` -ge 1800 -a \
                                `echo $qual 1_8 | cut -c5-8` -le 3000 ]; then
                              if [ `echo $qual1_8 | cut -c1-2` -ge 01 -a \
                                   `echo $qual1_8 | cut -c1-2` -le 12 ]; then
                                 if [ `echo $qual1_8 | cut -c3-4` -ge 01 -a \
                                      `echo $qual1_8 | cut -c3-4` -le 31 ]; then
                                    yyyy=$(echo $qual1_8 | cut -c5-8)
                                    mm=$(echo $qual1_8 | cut -c1-2)
                                    dd=$(echo $qual1_8 | cut -c3-4)
                                 fi
                              fi
                           fi
                        fi
                     fi
                     i=`expr $i + 1`
                  done
               fi

###################### THE GBBEPX MODIFICATIONS ARE HERE ########################

               if [ $yy -eq -99 -a $yyyy -eq 9999 ]; then

#  No luck, next look for a qualifier which begins with the date in the form
#   yyyymmdd (can come only after a "_" or "." delimiter) (the qualifier may be
#   just yyyymmdd, or it may have any number of alphanumeric characters after
#   it, e.g., "<yyyymmddhh>" or "<yyyymmdd>.")
#  ----------------------------------------------------------------------------

echo " dsname is  $dsname  "
echo " dsname is  $dsname  "
echo " dsname is  $dsname  "
echo " dsname is  $dsname  "
echo " dsname is  $dsname  "
# DON"T TAKE ABOVE
		  echo 'IN HERE 1'    
		  echo 'IN HERE 1'    
		  echo 'IN HERE 1'    
		  echo 'IN HERE 1'    

#  No luck, next look for filename ending on _cYYYYMMDDHHMMSSS.nc *.bin , etc.
#  Delimited "_" should be the last "_c" in the file name
#  ----------------------------------------------------------------------------

        	       if [ `echo $dsname | cut -c 1-5` = GBBEP  \
			     -o `echo $dsname | cut -c 1-5` = GMASI  ] ; then 
			  echo " TESTING THIS PART OF THE IF LOOP l  "
			echo "  THIS IS FOR GBBEPX OR AUTOSNOS ; TANKFILE is $TANKFILE "
			echo "  THIS IS FOR GBBEPX OR AUTOSNOS ; TANKFILE is $TANKFILE "
			echo "  THIS IS FOR GBBEPX OR AUTOSNOS ; TANKFILE is $TANKFILE "
			echo "  THIS IS FOR GBBEPX OR AUTOSNOS ; TANKFILE is $TANKFILE "
			echo "  THIS IS FOR GBBEPX OR AUTOSNOS ; TANKFILE is $TANKFILE "
			echo "  THIS IS FOR GBBEPX OR AUTOSNOS ; TANKFILE is $TANKFILE "
                  for delim in "_" ; do
                          echo " delim is $delim "

                          echo " TESTING THIS PART OF THE IF LOOP 2 "
                          echo " TESTING THIS PART OF THE IF LOOP 2 "
                          echo " TESTING THIS PART OF THE IF LOOP 2 "
                          echo " TESTING THIS PART OF THE IF LOOP 2 "
                     num_qual=`echo $dsname | awk -F"$delim" '{print NF}'`
                     echo num_qual=  $num_qual
		     
		     echo " delim is $delim and  num_qual is $num_qual "
                     echo " delim is $delim and  num_qual is $num_qual "
                     echo " delim is $delim and  num_qual is $num_qual "
                     echo " delim is $delim and  num_qual is $num_qual "
		     i=$num_qual
		     echo $i 'i HERE!!!!'
                     #while [ $i -ge 1 ]; do
                        qual=$(echo $dsname | cut -d"$delim" -f${i})
			echo "qual is :" $qual

                        inum=${#qual}
			echo "inum is: " $inum 

                        qual1_8=`echo $qual | cut -c2-9`
			echo "qual1_8 is: "$qual1_8

                        echo " qual is $qual and inum is $inum and qual1_8 is " $qual1_8
                        echo " qual is $qual and inum is $inum and qual1_8 is $qual1_8 "
                        echo " qual is $qual and inum is $inum and qual1_8 is $qual1_8 "
                        echo " qual is $qual and inum is $inum and qual1_8 is $qual1_8 "
                        if [[ ! -z $( echo $qual1_8 | grep "^[0-9]*$" ) ]]
                        then
                           if [ `echo $qual1_8 | cut -c1-4` -ge 1800 -a \
                                `echo $qual1_8 | cut -c1-4` -le 3000 ]; then
                              if [ `echo $qual1_8 | cut -c5-6` -ge 01 -a \
                                   `echo $qual1_8 | cut -c5-6` -le 12 ]; then
                                 if [ `echo $qual1_8 | cut -c7-8` -ge 01 -a \
                                      `echo $qual1_8 | cut -c7-8` -le 31 ]
                                 then
                                    yyyy=$(echo $qual1_8 | cut -c1-4)
                                    mm=$(echo $qual1_8 | cut -c5-6)
                                    dd=$(echo $qual1_8 | cut -c7-8)
                                 fi
                              fi
                           fi
                        fi
                        #i=`expr $i - 1`
                     #done
                     [ $yyyy -ne 9999 ]  &&  break
                  done
	      
# DON"T TAKE THIS
echo '===================='
echo $yyyy
echo $mm
echo $dd

		else # if it is not GBBEPX or AUTOSNOW
		echo " IT IS NOT GBBEPX OR AUTOSNOW "
		echo " IT IS NOT GBBEPX OR AUTOSNOW "
		echo " IT IS NOT GBBEPX OR AUTOSNOW "
		echo " IT IS NOT GBBEPX OR AUTOSNOW "

               fi
	fi
###################### THE GBBEPX MODIFICATIONS ARE HERE ########################
###################### THE GBBEPX MODIFICATIONS ARE HERE ########################
###################### THE GBBEPX MODIFICATIONS ARE HERE ########################
###################### THE GBBEPX MODIFICATIONS ARE HERE ########################
###################### THE GBBEPX MODIFICATIONS ARE HERE ########################
###################### THE GBBEPX MODIFICATIONS ARE HERE ########################
               if [ $yy -eq -99 -a $yyyy -eq 9999 ]; then

#  Still no luck, give up
#  ----------------------

                  bufrerror=99
               else
                  if [ $yyyy -eq 9999 ]; then

#  The date is of the form Dyyjjj - use windowing technique to convert 2-digit
#   yr to 4-digit yr, then convert year and day-of-year to year, month and day
#   [yy=41-99; then yyyy=1941-1999; yy=00-40; then yyyy=2000-2040
#  ---------------------------------------------------------------------------

                     if [ $yy -gt 40 ]; then
                        yyyyddd=19${yy}${ddd}
                     else
                        yyyyddd=20${yy}${ddd}
                     fi

                     yyyymmdd=`$UTILROOT/ush/date2jday.sh $yyyyddd`

                  else
                     yyyymmdd=${yyyy}${mm}${dd}
                  fi
               fi # end of check if year is 2 or 4 digs

               if [ $bufrerror -ne 99 ]; then
                  if [ ! -d $TANKDIR/$yyyymmdd/$TANKFILE ] ; then
                     mkdir -m 775 -p $TANKDIR/$yyyymmdd/$TANKFILE
                  fi
                  if [ $TARGETFILE = same_name2 ]; then
                     cp $DATA/$dsname $TANKDIR/$yyyymmdd/$TANKFILE/$dsname
#                     cp $dsname $TANKDIR/$yyyymmdd/$TANKFILE/$dsname
                     bufrerror=$?
                     if [ $bufrerror -eq 0 ] ; then
                        echo "$dsname_full RECEIVED FROM REMOTE MACHINE, \
$dsname (for $yyyymmdd) COPIED AT `date -u +%Y/%m/%d' '%H:%M:%S' UTC'`" >> \
                         ${ORBITLIST}_copy.history
                        msg="$dsname_full received from remote unix machine, \
$dsname copied for $yyyymmdd"
                        $UTILROOT/ush/postmsg "$jlogfile" "$msg"
                     fi
                  else # GMASI data will route through here
                     cp $DATA/$dsname $TANKDIR/$yyyymmdd/$TANKFILE/$TARGETFILE
#                     cp $dsname $TANKDIR/$yyyymmdd/$TANKFILE/$TARGETFILE
                     bufrerror=$?
                     if [ $bufrerror -eq 0 ] ; then
                        echo "$dsname_full RECEIVED FROM REMOTE MACHINE, \
$TARGETFILE (for $yyyymmdd) COPIED AT `date -u +%Y/%m/%d' '%H:%M:%S' UTC'`" >> \
                         $USERDIR/$TARGETFILE.history
                        msg="$dsname_full received from remote unix machine, \
$TARGETFILE copied for $yyyymmdd"
                        $UTILROOT/ush/postmsg "$jlogfile" "$msg"
                     fi
                  fi #IG GMASI is copied ok
               fi 
            fi 
         else
            bufrerror=99
         fi

      fi 
      ## not utilized by GMASI data
      #elif [ $EXECUTE = bufr_tranjb.sh ] ; then
      #   give_rc="YES"
      #   if [ $FTYPE = ncepbufr ] ; then
      #      cword="yes"
      #      DX_SKIP=YES # tell $CWORDush to not copy any dictionary msgs
      #   else
      #      cword="no"
      #   fi
      #   ksh $TRANush $TANKDIR $DATA/$dsname 
      #   bufrerror=$?
      #else
      #   ksh $USHobsproc_satingest/ingest_translate_orbits.sh
      #   bufrerror=$?
      #fi

      set +x
      echo
      echo "Time is now $(date -u)."
      echo
      [ $DEBUGSCRIPTS = ON -o $DEBUGSCRIPTS = YES ]  &&  set -x

      if [ $PROC_MULT_FILES = YES ]; then #IG if concatenation, more than one filename, rethink this if-then-fi block !!!
         cat $DATA/orbitlist_path_cat | {
         read line
         rc=$?
         while [ $rc -eq 0 ] ; do
            set -A FILEINFO $line
            if [ $bufrerror -eq 0 ] ; then
               iword=0
               while [ $iword -lt ${#FILEINFO[*]} ] ; do
                  neworbit=${FILEINFO[$iword]}
                  dsname=$(basename $neworbit)
                  print -u9 $neworbit
                  procorbcount=$(($procorbcount+1))
                  echo "$neworbit PROCESSED AT \
$(date -u +%Y/%m/%d' '%H:%M:%S' UTC')" >> $ORBITLIST.history
                  if [ $DELAFTPROC = YES ] ; then
                     rm $DATA/$dsname
                  fi
                  iword=$(($iword+1))
               done
            else
               iword=0
               while [ $iword -lt ${#FILEINFO[*]} ] ; do
                  neworbit=${FILEINFO[$iword]}
                  dsname=$(basename $neworbit)
                  noproccount=$(($noproccount+1))
                  echo "COULD NOT PROCESS $neworbit AT \
$(date -u +%Y/%m/%d' '%H:%M:%S' UTC')" >> $ORBITLIST.history
                  if [ $DELAFTPROC = YES ] ; then
                     rm $DATA/$dsname
                  fi
                  iword=$(($iword+1))
               done
            fi
            read line
            rc=$?
         done }
      else
         if [ $bufrerror -eq 0 ] ; then
            iword=0
            while [ $iword -lt ${#FILEINFO[*]} ] ; do
               neworbit=${FILEINFO[$iword]}
               dsname=$(basename $neworbit)
               print -u9 $neworbit
               procorbcount=$(($procorbcount+1))
               echo "$neworbit PROCESSED AT \
$(date -u +%Y/%m/%d' '%H:%M:%S' UTC')" >> $ORBITLIST.history
               if [ $DELAFTPROC = YES ] ; then
                  rm $DATA/$dsname
               fi
               iword=$(($iword+1))
            done
         else
            iword=0
            while [ $iword -lt ${#FILEINFO[*]} ] ; do
               neworbit=${FILEINFO[$iword]}
               dsname=$(basename $neworbit)
               noproccount=$(($noproccount+1))
               echo "COULD NOT PROCESS $neworbit AT \
$(date -u +%Y/%m/%d' '%H:%M:%S' UTC')" >> $ORBITLIST.history
               if [ $DELAFTPROC = YES ] ; then
                  rm $DATA/$dsname
               fi
               iword=$(($iword+1))
            done
         fi
      fi # IG end of concatenated

   fi # closes $toterr