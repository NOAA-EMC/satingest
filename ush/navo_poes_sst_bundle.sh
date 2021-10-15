#!/bin/ksh

# Generate look-alike intermediate bufr files of navo sst obs for downstream job
#   poes_phyret_sst

set -xu

host=$(hostname -s)
pid=$$
max_failedsplit=${max_failedsplit:-3}

## Allow time for intermediate bufr files with obs valid during the previous 
## cycle to accumulate before we process them here, bundling obs from the same 
## satellite and cycle.  The bundling step produces fewer files input to 
## downstream job poes_phyret_sst, improving its efficiency.

read yyyymmdd hhmm <<< $(date --utc "+%Y%m%d %H%M")  # get current date and time

# Leaving some leeway between when we start bundling and expected kick-off 
#   of next poes_phyret_sst run (currently 0105 0705 1305 1905).
#   An extra bundle is added after 1830 so the last poes_phyret_sst run prior
#   rtgssthr analysis can pick up a few extra swaths for that job.  This is
#   being done to come close to the number of sstnvp obs currently assimilated
#   by rtgssthr. 

# Using 2-digit hours for clarity.  Not worrying about octal interpretation 
# since hhmm var is only used for comparison. (reconsider if doing arithmatic). 
# Windows below are based on JIPOES_SST trigger times of 25 and 55 minutes after the hour.

if [ $hhmm -gt 2230 ]; then
  start_cutoff=${yyyymmdd}21
elif [ $hhmm -gt 1830 -a $hhmm -lt 1910 ]; then
  start_cutoff=${yyyymmdd}19    # early bundle for 1905 phyret run prior to rtgssthr
elif [ $hhmm -gt 1630 ]; then
  start_cutoff=${yyyymmdd}15
elif [ $hhmm -gt 1030 ]; then
  start_cutoff=${yyyymmdd}09
elif [ $hhmm -gt 0430 ]; then
  start_cutoff=${yyyymmdd}03
else
  start_cutoff=$(date --date="${yyyymmdd} - 1 day" +%Y%m%d21)
fi

# look for unbundled files and process if yyyymmddhh of first ob is before $start_cutoff

for SAT in n19 mta n18; do
  case $SAT in
    n18) SATNAME=noaa18;;
    n19) SATNAME=noaa19;;
    mta) SATNAME=metopa;;
    *) echo "do not process sat $SAT"; continue;;
  esac
  echo $SAT
  for IDATE in $PDYm2 $PDYm1 $PDY; do
    echo $IDATE
    rm -f NC012012.*.bufr NC012012.*.bufr.bak
    for ifile in $COMINGEST.$IDATE/b012.xx012.mcsst_${SAT}_d????????_s??????_e??????_b*_cnavo.tmp.tmpout.*
    do
      [ ! -s $ifile ] && continue
      bfile=$(basename $ifile)
      if [ -s ${COMINGEST}.$IDATE/bundled.$SAT.list ]; then
        if [ $(grep -l "$bfile DONE" ${COMINGEST}.$IDATE/bundled.$SAT.list) ]; then
            echo "File $bfile already bundled."
            continue
        fi
        num_fail=$(grep $bfile ${COMINGEST}.$IDATE/bundled.$SAT.list|grep -c "COULD NOT PROCESS")
        if [ $num_fail -gt 0 ]; then
          if [ $num_fail -gt $max_failedsplit ]; then
            msg="SKIP $bfile.  DESIGNATED UNPROCESSABLE AFTER $num_fail ATTEMPT(s)."
            $DATA/postmsg "$jlogfile" "$msg"
            continue
          else
            echo "Give $bfile another go despite $num_fail failed attempt(s)."
          fi
        fi
      else 
        [[ "$SENDCOM" == YES ]] && touch ${COMINGEST}.$IDATE/bundled.$SAT.list
      fi
      orig_dsname=${bfile/b012.xx012.}
      read bfilestart bfileend < $COMINGEST.$IDATE/timwin.$orig_dsname
      echo $bfilestart $bfileend
      start_yyyymmddhh_bfile=$(echo $bfilestart | cut -c 1-10)
      if [ $start_yyyymmddhh_bfile -ge $start_cutoff ]; then
         echo too soon to process $bfile | tee -a SKIPPED.txt
         continue
      fi
# have file ready to be processed.  
      cp $ifile .

# create backups of existing files that may receive additional data
      for tbfile in NC012012.??????????.bufr; do
        [ ! -s $tbfile ] && continue
        cp -p $tbfile $tbfile.bak
      done
      export pgmout=stdout
      pgm=bufr_cycsplit
      . prep_step
      export FORT20=$bfile
set +x
echo ""
echo " Output filename(s) generated within code based on ob date/cycles found within same input file"
echo "   FORT51=> output bufr file for first cycle found" 
echo "   FORT52=> output bufr file for second cycle found (if needed)"
echo "     etc... "
echo " Files generated by previous calls of this code may get updated by this instance."
echo ""
set -x
      $EXECobsproc_satingest/bufr_cycsplit
      retcode=$?
      if [ $retcode -eq 0 ]; then
        echo "$bfile DONE $(date) " >> bundled.$SAT.I$IDATE.tmplist
      else
        msg="ERROR PROGRAM $pgm RETURN CODE $retcode. Continue..."
        $DATA/postmsg "$jlogfile" "$msg"
        echo "$bfile $(date) COULD NOT PROCESS" >> bundled.$SAT.I$IDATE.tmplist
        echo "Recover any temp bufr files from backups and continue."
        for tbfile in NC012012.??????????.bufr; do
          rm -f $tbfile
          [ -s $tbfile.bak ] && cp $tbfile.bak $tbfile
        done
      fi
    done  # each potential input file for this satellite and ingest day

    for tbfile in NC012012.??????????.bufr; do
      if [ -s $tbfile ]; then
        CDATE=$(echo $tbfile | cut -f2 -d.)
        ddate=$(echo $CDATE | cut -c 1-8)
        bcycle=t$(echo $CDATE | cut -c 9-10)z
        year=$(echo $ddate | cut -c1-4)
        doy=$(date --date=$ddate +%j)
        lookalike_base=navysst.tmpobs.lookalike.$year.$doy
# A 4-digit seqnum is used to simulate the time field in the tmpobs filename in the old feed.
# A shared $year.$doy.$seqnum string for noaa19 and metopa bundle names allows data for the two 
# satellites to be processed in same gsi instance in poes_phyret_sst, saving run time.
        seqnum=$(printf "%04d\n" $(( $(ls $COMINGEST.$IDATE/ymd.$lookalike_base.*.${bcycle}.* 2>/dev/null | grep -v ${host}.${pid}$ |wc -l) + 1 )) )
        lookalike=b012.xx012.navysst.tmpobs.lookalike.$year.$doy.$seqnum.$bcycle.$SATNAME.tmpout.$host.$pid
        if [[ "$SENDCOM" == YES ]]; then
          cp $tbfile $COMINGEST.$IDATE/$lookalike
          ymdfile=ymd.navysst.tmpobs.lookalike.$year.$doy.$seqnum.$bcycle.tmpout.$host.$pid
          echo $ddate > $COMINGEST.$IDATE/$ymdfile
          for ccycle in t00z t06z t12z t18z; do
            [[ $ccycle == $bcycle ]] && continue
            ymdfile=ymd.navysst.tmpobs.lookalike.$year.$doy.$seqnum.$ccycle.tmpout.$host.$pid
            [[ ! -s $COMINGEST.$IDATE/$ymdfile ]] && echo "       0" > $COMINGEST.$IDATE/$ymdfile
          done
        fi
        mv $tbfile $tbfile.$SAT  # just for reference if $DATA is not scrubbed.
      fi
    done
    if [[ "$SENDCOM" == YES && -s bundled.$SAT.I$IDATE.tmplist ]]; then
       cat bundled.$SAT.I$IDATE.tmplist >> $COMINGEST.$IDATE/bundled.$SAT.list
    fi
  done  # end processing ingest date dir
done  # end satellite loop

