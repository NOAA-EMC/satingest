#!/bin/ksh
# Run under ksh

#  SCRIPT NAME :  ingest_qmgr.sh
#               AUTHOR :  Luke Lin
#         DATE WRITTEN :  05/29/1998
#
#  Abstract:  This script manages the non-decoder production (or checkout if so
#             specified) ingest job queue on the NCEP-WCOSS based upon the
#             following criteria:
#             1. any existing non-decoder production (or checkout if so
#                specified) ingest job running in the ingest queue will
#                continue to the end.
#             2. any new incoming non-decoder ingest job (production, test, or
#                checkout) will not run on the NCEP-WCOSS if there is an
#                existing production (or checkout if so specified) ingest job
#                of the same type running in the queue; the new job will set
#                itself to complete under eCFlow and kill itself (or return
#                with status code 99 if so specified)
#
#  History:
#  1999-11-30  Gilbert     - Converted to the IBM CCS.
#  2001-07-31  Keyser      - See option 3 in Abstract.
#  2004-08-13  Keyser      - Added diag. message to joblog; Added option to NOT
#                            kill the job but just exit w/ RC=99 (variable
#                            KILL-default is still YES); added option to test
#                            for jobs owned by a user other than nwprod
#                            (variable OWNER-default is still nwprod)
#  2007-05-15  Keyser      - No longer tests to see if a ruc2a dump job is in
#                            the system when this job is of the type "radsnd"
#                            (in order to then kill or stop this job) because
#                            ruc2a dump jobs no longer contain a step to
#                            ingest "radsnd" data prior to dumping data
#  2012-10-18  Y. Ling     - Modified to run on WCOSS. This script is now set
#                            to run under ksh shell as the default.
#  2014-01-03  Keyser      - Moved from util/ush (when it was under /nwprod
#                            root) to ush here.  Modified to execute ecflow
#                            commands only if imported variable $SENDECF is
#                           'YES' (the default).  $OWNER now defaults to
#                           "$USER" rather than "nwprod" so checkout jobs do
#                            not have to export it any longer ($OWNER will only
#                            default to "nwprod" if $USER is not set).  Added
#                            check to make sure this job is now in bjobs
#                            listing before testing against listing of all jobs
#                            in bjobs for $OWNER (since there is sometimes a
#                            lag).
#  2021-10-15  Nadiga     -  changed bjobs to qstat
#  2022-01-18  S. Stegall -  Replaced $DATA/ before calling utility scripts and
#                            instead used $UTILROOT/ush/ to properly leverage the
#                            prod_util module.
#

set -x
KILL=${KILL:-YES}
#####OWNER=${OWNER:-nwprod}
OWNER=${OWNER:-${USER:-nwprod}}
SENDECF=${SENDECF:-YES}
env

reqid=`echo ${PBS_JOBID}`
echo "reqid= $reqid "
jobname=${PBS_JOBNAME}
echo "jobname= $jobname"

#  make sure THIS job is now in qstat (sometimes a lag)
#  ----------------------------------------------------
joblist=$DATA/jobs_list
iflag=0
icount=0
while [ $iflag -eq 0 ]; do
   icount=`expr $icount + 1`
   if [ $icount -ge 10 ];then
      echo "this job $reqid $jobname still NOT in qstat after 10 tries- give up"
      break
   fi
   qstat -u $OWNER -r -w | awk '{print $1" "$7}' | tail -n +2 > $joblist
   cat $joblist
   cat $joblist | while read jidx jname;do
      if [ $jidx = $reqid ]; then
         echo "this job $reqid $jobname is in qstat - continue on"
         iflag=1
         break
      fi
   done < $joblist
   if [ $iflag -eq 0 ]; then
      echo "this job $reqid $jobname is NOT in qstat - check again in 3 seconds"
      sleep 3
   fi
done

#   Get a list of jobs on the system owned by $OWNER (defaults to $USER which
#   defaults to nwprod)

joblist=${DATA}/jobs.list.$reqid
qstat -u $OWNER -r -w | awk '{print $1" "$7}' | tail -n +2 > $joblist
cat $joblist

cat $joblist |while read jid jname
do
       if test "$jid" = "$reqid"
       then
           echo " yes, this is the job itself -  $jid"
           echo " This job should continue to the end "
#          .. this is the only ingest job in the queue with the same type
#          .. this job will stay and run to the end
       else

#          ... Make sure the job has the same type of ingest job 
#          ... This is for safe guard purpose, some other FTP jobs
#          ... may run at the same time.

           if test "$jname" = "$jobname"
           then
#             ... Another job with this name is in the system, let that job
#                 run and ...
              if test "$KILL" = 'YES'
              then
#                ... kill the current job and set it to complete on ecFlow
                 msg="Another job with this name is in the system, this \
ingest job will kill itself"
                 echo $msg
                 $UTILROOT/ush/postmsg "$jlogfile" "$msg"
                 [ $SENDECF = YES ] && ecflow_client --complete
                 [ -d $DATAROOT ]  && cd $DATAROOT
                 rm -rf $DATA
                 qdel $reqid
                 sleep 120
              else
#                ... exit out of this script with rc=99 but do not kill the
#                    current job 
                 exit 99
              fi
           fi 
       fi
done < $joblist


