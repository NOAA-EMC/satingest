!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: BUFR_TRANIMGR
!   PRGMMR: KEYSER              ORG: NP22        DATE: 2014-01-20
!
! ABSTRACT: READS IN NESDIS GOES IMAGE RADIANCE DATA IN WMO BUFR
! FORMAT, REFORMATS AND PACKS INTO A BUFR FILE WHICH CAN BE DATABASED
! BY TRANJB.  THERE ARE CURRENTLY SIX CHANNELS.
!
! PROGRAM HISTORY LOG:
! 2001-09-19  X. Su - Original author
! 2002-06-12  X. Su - New format of GOES radiance
! 2002-10-25  X. Su, J. Woollen - New format of GOES radiance
! 2003-01-30  X. Su, J. Woollen - New BUFR format of GOES radiance
! 2004-09-03  X. Su - New BUFR format with Gaussian source
! 2006-02-02  D. Keyser - Replaced call to BUFRLIB routine IREADIBM
!     with call to BUFRLIB routine IREADMG (IREADIBM obsolete with
!     1/31/2006 version of BUFRLIB)
! 2008-02-19  G. Krasowski - Replaced TMBRST, which stores brightness
!     temperature to the nearest 0.1 Kelvin, with TMBR and SDTB, which
!     stores brightness temperature and standard deviation of brightness 
!     temperature, respectively, to the nearest 0.01 Kelvin, in calls to 
!     UFBREP.
! 2012-11-08  S. Melchior - Changes to run on WCOSS (e.g., replaced W3LIB
!     with more specific W3NCO).
! 2014-01-20  D. KEYSER -- Minor changes.
!
! USAGE:
!   INPUT FILES:
!     UNIT 05  - STANDARD INPUT. W3TRNARG PARSES ARGUMENTS FROM
!              - STANDARD INPUT.
!     UNIT 11  - WMO BUFR FILE.
!     UNIT 19  - FOREIGN BUFR TABLE FILE CONTAINING BUFR TABLES A,
!              - B, AND D (FOR UNIT 11).
!     UNIT 20  - NCEP BUFR TABLE FILE CONTAINING BUFR TABLES A, B, AND
!                D (FOR UNIT 51).
!
!   OUTPUT FILES:
!     UNIT 06  - PRINTOUT
!     UNIT 51  - POINTS TO THE OUTPUT BUFR FILE. TRANJB WILL PLACE
!                THE BUFR MESSAGES INTO THE PROPER TANKS.
!
!   SUBPROGRAMS CALLED:
!     LIBRARY:
!       W3NCO  - W3TRNARG W3TAGB   W3TAGE   err_exit
!     BUFRLIB  - OPENBF   CLOSBF   OPENMB   UFBINT   UFBREP   WRITSB
!              - IREADMG  IREADSB  DATELEN
!
!   EXIT STATES:
!     COND =   0 - SUCCESSFUL RUN
!          =   1 - UNABLE TO PARSE INPUT ARGUMENTS IN W3TRNARG
!          = 253 - NO REPORTS WRITTEN OUT
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 95
!   MACHINE:  NCEP WCOSS
!
!$$$

program BUFR_TRANIMGR

real(8),dimension(8) :: timein 
real(8),dimension(10) :: satinfo,a
real(8),dimension(6,12) :: radin
real(8),dimension(6) :: pccfin
real(8),dimension(13,6) :: radout
real(8),dimension(5,12) :: cldinf
real(8),dimension(18)   :: qcinf
real(8),dimension(14)   :: sidpinf
integer :: iyr

character(8) :: subset,tlflag,subfgn
character(80) :: appchr,subdir,tankid

data lunin /11/
data lindx /19/
data lundx /20/
data lunot /51/

data idate_PREV/-99/,ldate_PREV/-99/

!------------------------------------------------------------------------------

call w3tagb('BUFR_TRANIMGR',2014,0020,0082,'NP22')

PRINT *, ' '
PRINT *, ' ==> Welcome to BUFR_TRANIMGR -- Version 01/20/2014'
PRINT *, ' '

call w3trnarg(subdir,lsubdr,tankid,ltnkid,appchr,lapchr,tlflag,jdate,kdate,ierr)
if(ierr /= 0) then
   write(6,&
   '(''UNABLE TO PARSE ARGS TO TRANSLATION ROUTINE - RETURN CODE = '',I5)') IERR
   call  w3tage('BUFR_TRANIMGR')
   call  err_exit(ierr)
end if

subset = 'NC'//subdir(lsubdr-2:lsubdr)//tankid(ltnkid-2:ltnkid)         
!------------------------------------------------------------------------------

ird=0
iwt=0
ktskpt=0
ktskpt_msg=0
ikeep=0
radout=10e10

call datelen(10)

!  Open the input and output BUFR files
!  ------------------------------------

!!!print *,'start to read data'

call openbf(lunin,'IN',lindx)
!!!!!call openbf(lunot,'OUT',lundx)
call openbf(lunot,'NODX',lundx)

!  Read through the message/subsets in the file
!  --------------------------------------------

do while(ireadmg(lunin,subfgn,idate) == 0)

!!!print *, 'idate =', idate
!!!print *,' subfgn is ',subfgn

   if(idate.ne.idate_PREV)  THEN
      print *, ' '
      print *, 'OPENING INPUT  MESSAGE WITH NEW DATE ',IDATE,' (SUBSET ',&
       SUBFGN,')'
      print *, ' '
   endif
   idate_PREV = idate
   
   do while(ireadsb(lunin) == 0)

!  Read the internal date and check for the realism
!  ------------------------------------------------

      call ufbint(lunin,timein,8,1,iret,&
       'YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH')

      iyr = nint(timein(1))
      mon = nint(timein(2))
      idy = nint(timein(3))
      ihr = nint(timein(4))
      min = nint(timein(5))
      isc = nint(timein(6)) 
      ird=ird+1
   
      if( iyr <= 0 .or. mon <1 .or. mon >12 .or. idy <1 .or. idy >31 .or. &
       ihr <0   .or. ihr >24 .or. min <0 .or. min >60 .or. isc <0 .or. &
       isc >60) then
 
         print 200, iyr,mon,idy,ihr,min,isc,subfgn
200      format(' BAD DATE:',i4,5i2.2,' SUBSET:',a8)
         ktskpt=ktskpt+1
      else 
 
         radin=-e10
         call ufbint(lunin,satinfo,10,1,iret,&
          'SAID GCLONG SCLF SSNX SSNY NPPR NPPC SAZA SOZA LSQL')
         call ufbrep(lunin,cldinf,5,12,iret,'SCCF SCBW CLDMNT NCLDMNT CLTP')
!!!         call ufbrep(lunin,qcinf,1,18,iret,'TMBRST')
!  Split up array qcinf in ufbrep to include TMBR and SDTB for greater precision. -GSK
         call ufbrep(lunin,qcinf(1),1,6,iret,'TMBR')
         call ufbrep(lunin,qcinf(13),1,6,iret,'SDTB')
         call ufbrep(lunin,sidpinf,1,14,iret,'SIDP')
         call ufbrep(lunin,radin,6,12,iret,'RDTP RDCM SCCF SCBW SPRD RDNE')
         call ufbrep(lunin,pccfin,1,6,iret,'PCCF')
        
         if(qcinf(1) >1.0e8 .and. qcinf(2) >1.0e8 .and. qcinf(3) >1.0e8 .and. &
            qcinf(4) >1.0e8 .and. qcinf(5) >1.0e8 .and. qcinf(6) >1.0e8) then
            ktskpt=ktskpt+1
            ktskpt_msg=ktskpt_msg+1
            cycle
         else

!  Put into output array
!  ---------------------

            radout(1,1:6)=sidpinf(3:8)
            radout(2:7,1:6)=radin(1:6,1:6)
            radout(8,1:6)=qcinf(1:6)
            radout(9:11,1:6)=cldinf(3:5,1:6)
            radout(12,1:6)=qcinf(13:18)
            radout(13,1:6)=pccfin(1:6)

!  Check the data
!  --------------
            ikeep=ikeep+1

            if(ikeep ==20) then
!!!!!       if(pccfin(3) <10e10) then
               write(6,300) iyr,mon,idy,ihr,min,isc,timein(7),timein(8)
300            format('year, month, day, hour, min, sec, lat, lon'/6i6,2f10.2)
               write(6,400)
400            format('satid, center, sat class, xsize, ysize, nppr, nppc, ',&
                'sat. zenith, sol. zenith,land/ocean ')
               write(6,401) satinfo
401            format(10e12.3)
               write(6,500)
500       format('rad. type, rad. calc., freq., band width, spec. rad., rad., ')
               do j=1,12
                  write(6,501) j,(radin(i,j),i=1,6)
501               format('  chn',i3,2x,10e12.3)
               enddo

               write(6,402)
402            format ('Cloud info follows')
               do j=1,12
                  write(6,501) j, (cldinf(i,j),i=1,5)
               enddo
               write(6,*) ' quality information follows'
               write(6,403) (qcinf(i),i=1,18)
403            format(6e12.3)

               write(6,*) 'percentage confidence info follows'
               write(6,403) (pccfin(i),i=1,6)

               write(6,*) 'output radiance'
               do j=1,6
                  write(6,404) (radout(i,j),i=1,13)
404               format(7e12.3)
               enddo
            endif
               
!  Check report date to see if a new output message should be opened (tranjb
!   takes care of this for uncompressed files, but it doesn't hurt to have
!   redundancy built in here)
!  -------------------------------------------------------------------------
  
            ldate=iyr*1000000+mon*10000+idy*100+ihr
!!!!!       print *,' ldate ',ldate
!!!!!       print *,' subset is ',subset

            if(ldate.ne.ldate_PREV)  then
               print *, ' '
               print *, 'OPENING OUTPUT MESSAGE WITH NEW DATE ',LDATE,&
                ' (SUBSET ',SUBSET,')'
               print *, ' '
            endif
            ldate_PREV = ldate
            call openmb(lunot,subset,ldate)

!  Write out subset
!  ----------------

            call ufbint(lunot,timein,8,1,iret,&
             'YEAR MNTH DAYS HOUR MINU SECO CLAT CLON')
            call ufbint(lunot,satinfo,10,1,iret,&
             'SAID GCLONG SCLF SSNX SSNY NPPR NPPC SAZA SOZA LSQL')
            call ufbrep(lunot,radout,13,6,iret,&
      'SIDP RDTP RDCM SCCF SCBW SPRD RDNE TMBRST CLDMNT NCLDMNT CLTP SDTB PCCF')
            call WRITSB(lunot)

            iwt=iwt+1
         end if 
      end if
   enddo
enddo
   
call closbf(lunin)
call closbf(lunot)

if(ktskpt_msg.gt.0)  then
   print *, ' '
   print *, ktskpt_msg,&
    ' REPORTS SKIPPED BECAUSE ALL 5 BRIGHTNESS TEMPERATURES WERE MISSING'
   print *, ' '
   print *, ' '
end if

print *, '*** PROCESSING ENDED NORMALLY ***'
print*,'*** READ :',IRD
print*,'*** WROT :',IWT
print*,'*** SKIP :',KTSKPT
print*,'*** PROCESSING ENDED NORMALLY ***'
      
if(iwt == 0) then
   print *, 'NO REPORTS PROCESSED -- DISABLING ALL SUBSEQUENT PROCESSING.'
   CALL W3TAGE('BUFR_TRANIMGR')
   CALL err_exit(253)
end if

CALL W3TAGE('BUFR_TRANIMGR')

stop
end
