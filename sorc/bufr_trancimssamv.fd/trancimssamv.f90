!$$$  main program documentation block
!
! Main program: BUFR_TRANCIMSSAMV
!   Prgmmr: Stegall           Org: EMC         Date: 2022-09-15
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Rewrite/update abstract:
! ABSTRACT:  Reads in University of Wisconsin supplied ASCII data files 
!            containing CIMSS AMV wind speeds and wind directions.  The
!            ASCII files are subject to some gross QC.  Then the ASCII 
!            data is translated into an output BUFR file. 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!rewrite/update program history
! Program history log:
! 2022-09-15  S. T. Stegall   -- Adapted from program PROCESS_GOESIMGR_SKYCOVER
!             written by KEYSER/Carley/Whiting. 
!
! Usage:
!  Input files:
!    Unit 05  - Standard input. W3TRNARG parses arguments from standard input
!    Unit 11  - ASCII file containing CIMSS AVM data
!    Unit 20  - BUFR mnemonic table file containing BUFR tables A, B, and D
!
!  Output files:
!    Unit 06  - Standard output print
!    unit 51  - output BUFR format data file later to be appended to BUFR tank
!               containing CIMSS AMV data
!
! Subprogams called:
!    Unique: -  REMTDY (inline source)
!   Library:
!       W3NCO - W3TAGB  W3TAGE W3TRNARG W3DOXDAT W3FS26 ERREXIT
!     BUFRLIB - DATELEN OPENBF GETBMISS OPENMB   UFBINT WRITCP  CLOSMG CLOSBF
!
! Exit states:
!   Cond = 0  - successful run
!      =   1 - Unable to parse input arguments in W3TRNARG
!      =   2 - Invalid subdirectory argument input to W3TRNARG
!      =  66 - Invalid incoming effective cloud amouunt (either single, center
!              fov or averaged fov)
!      =  77 - Invalid incoming data record (only line 1 checked)
!      =  88 - Invalid incoming satellite number
!
! Attributes:
!   Language: Fortran 90 (free format)
!   Machine:  NCEP WCOSS2
!
!$$$

      program BUFR_TRANCIMSSAMV

      implicit none

      integer lunin  /11/
      integer luntab /20/
      integer lunout /51/
      integer nl,nused
      integer jdate,itime,isat,iyr,idyr,ierr,iday,mnth,ihr,imin,isec
      integer lsubdr,ltnkid,lapchr,jjdate,kkdate,idate,iret
      integer dir,pre,qi,itype,said

      integer iGNAPS,iGCLONG,iOGCE

      real*8 arr(8),bmiss,getbmiss
      real rlat,rlon,spd

      character*20  dsname
      character*5   dsname_trim
      character*132 line,temp
      character*80  fmt,appchr
      character*12  subdir,tankid
      character*10  datechar
      character*8   subset,tlflag
      character*4   ctype

      logical       db   /.false./

!      call w3tagb('BUFR_TRANSKYCOVR',2017,0313,0050,'NP22')
      call w3tagb('BUFR_TRANCIMSSAMV',2022,0313,0050,'NP22')

      CALL GET_ENVIRONMENT_VARIABLE('dsname',dsname)

      print*
      print*, 'WELCOME TO BUFR_TRANCIMSSAMV - VERSION 09-15-2022'
      print*

!      db=.true.    ! debug

      call w3trnarg(subdir,lsubdr,tankid,ltnkid,appchr,lapchr,tlflag,jjdate, &
                    kkdate,ierr)
!.......................................................................
      if(ierr.ne.0) then
         write(6,&
 '('' UNABLE TO PARSE ARGS TO TRANSLATION ROUTINE - RETURN CODE = '',i5)') ierr
         call w3tage('BUFR_TRANCIMSSAMV')
         call errexit(ierr)
      endif
!.......................................................................
      subset = 'NC'//subdir(lsubdr-2:lsubdr)//tankid(ltnkid-2:ltnkid)
 print*, 'SUBSET = ',SUBSET

! Obtain BUFRLIB value for missing

      bmiss = getbmiss()
      print*, 'bmiss returned as ',bmiss
      print*

      call datelen(10)

     dsname_trim = dsname(16:20)
     print*, 'dsname ', dsname
     print*, 'dsname_trim ', dsname_trim

     if(dsname_trim.eq.'GOESE') then
	said=270
     else if(dsname_trim.eq.'GOESW') then
	said=272
     endif

!-qi stuff------------------------------------------------------------------------------------------------
iGNAPS=5
!!!!iGCLONG=160
!!!iGCLONG=bmiss
iOGCE=160
itype=15      !SWCM missing value
!said=270	!GOESE is G16, said=270.  GOESW is G17, said=271 (currently no data from GOESW yet from lftp site)


!!!!! call openbf (LUNOUT,'OUT',LUNTAB) ! Open new output BUFR file
      call openbf (LUNOUT,'NODX',LUNTAB)! Open new output BUFR file

!expected ascii format for new CIMSS AMV data:
!type     day        hms     lat       lon         spd        dir       pre        qi
!IR     2022083    164500   44.8000   79.3434       4.3       261       754        66
!IR     2022083    164500   44.8000   74.2626      13.4       233       759        94
!IR     2022083    164500   44.8000   74.2121      14.5       238       754        87
!IR     2022083    164500   44.8000   74.1010      14.4       242       742        85

!read in text of firstline into dummy variable (skip first line)
         read(lunin,'(a)',end=200) temp
 
! Loop thru lines of input data
      nl=0
      nused=0

      read1: do
            

         read(lunin,'(a)',end=200) line
         nl=nl+1

         if (mod(nl,100)==0) print*,'working on report', nl

         if (db) write(*,'(1x,i3,1x,a)') nl,">>>" // trim(line) // "<<<"

         read(line,*) ctype,jdate,itime,rlat,rlon,spd,dir,pre,qi

! --parse date & time

         iyr=jdate/1000
         idyr=mod(jdate,1000)

! Go from day-of-year to month and day        
         call remtdy(iyr,idyr,mnth,iday)
 
         if (db) write(*,*) 'dates:',idyr,iyr,mnth,iday

         ihr=itime/10000
         imin=mod(itime,10000)/100
         isec=mod(itime,100)
         if (db)  write(*,*) 'times:',itime,ihr,imin,isec

! -- perform a sanity check for the first line (report) read in to ensure it is
!    in the expected input format- if this one is ok assume all lines following
!    are ok

         if(nl.eq.1) then
            if(iyr.lt.1990 .or. iyr.gt.2050) then
               write(6,&
                '('' #####> INVALID YEAR '',i4, &
                '' -- STOP WITH RETURN CODE 77'')') iyr
               go to 66
            end if
            if(mnth.lt.1 .or. mnth.gt.12) then
               write(6,&
                '('' #####> INVALID MONTH '',i4, &
                '' -- STOP WITH RETURN CODE 77'')') mnth
               go to 66
            end if
            if(iday.lt.1 .or. iday.gt.31) then
               write(6,&
                '('' #####> INVALID DAY '',i4, &
                '' -- STOP WITH RETURN CODE 77'')') iday
               go to 66
            end if
            if(ihr.lt.0 .or. ihr.gt.23) then
               write(6,&
                '('' #####> INVALID HOUR '',i4, &
                '' -- STOP WITH RETURN CODE 77'')') ihr
               go to 66
            end if
            if(imin.lt.0 .or. imin.gt.59) then
               write(6,&
                '('' #####> INVALID MINUTE '',i4, &
                '' -- STOP WITH RETURN CODE 77'')') imin
               go to 66
            end if
            if(isec.lt.0 .or. isec.gt.59) then
               write(6,&
                '('' #####> INVALID SECOND '',i4, &
                '' -- STOP WITH RETURN CODE 77'')') isec
               go to 66
            end if
            if(rlat.lt.-90. .or. rlat.gt.90.) then
               write(6,&
                '('' #####> INVALID LATITUDE '',F9.4, &
                '' -- STOP WITH RETURN CODE 77'')') rlat
               go to 66
            end if
            if(rlon.lt.-180. .or. rlat.gt.360.) then
               write(6,&
                '('' #####> INVALID LONGITUDE '',F9.4, &
                '' -- STOP WITH RETURN CODE 77'')') rlon
               go to 66
            end if
            go to 67
66          continue
            call w3tage('BUFR_TRANCIMSSAMV')
            call errexit(77)
         endif
67       continue

! -- perform a sanity check of wind speed and wind dir for every report

         if(spd.lt.0.)  then
            write(6,&
            '('' #####> INVALID WIND SPEED '',f7.2, &
            '' -- STOP WITH RETURN CODE 66'')') spd
            call w3tage('BUFR_TRANCIMSSAMV')
            call errexit(66)
         else if(dir.lt.0. .or. dir.gt.360.)  then
            write(6,&
            '('' #####> INVALID WIND DIRECTION '', &
            f7.2,'' -- STOP WITH RETURN CODE 66'')') dir
            call w3tage('BUFR_TRANCIMSSAMV')
            call errexit(66)
         endif

!         read(csat,'(i2)') isat

! -- convert CIMSS char type to SWCM int type (see SWCM in https://www.nco.ncep.noaa.gov/sib/jeff/CodeFlag_0_STDv31_LOC7.html#002023)
! -- ex.  ctype=IR -> itype=1 -> SWCM=1
! -- May need to update if more ctypes are found in files.  The 5 below are the ones found in the files so far

	if(ctype.eq.'IR') then
	  itype=1
	else if(ctype.eq.'VIS') then
	  itype=2
        else if(ctype.eq.'WVCT') then
          itype=3
        else if(ctype.eq.'WVCA') then
          itype=5
        else if(ctype.eq.'OIR') then
          itype=6
	endif

!-NEED TO FIGURE OUT LONGITUDES-------------------------------------------------------------------------------------------------------------------------
! Lon read in is positive W oriented (either 0 to +360 W, or 0 to +180 W and
! 0 to -180 E - not sure which) - convert to positive East oriented (0 to 360 E)

         rlon=(rlon*-1.0)+360.0
         rlon=mod(rlon,360.)

! At this point E lon is BUFR standard (O to +180 E) but W lon is +180 to +360 E
!  which is not BUFR standard of 0 to -180 W - so convert

         if(rlon.gt.180.)  rlon = rlon-360.

         fmt='(1x,i7,1x,i6,1x,f9.4,1x,f10.4,2(1x,f6.2),1x,i2)'
!         if (db) write(*,fmt) jdate,itime,rlat,rlon,eca_avg,eca,isat
!         if (db) write(*,fmt) ctype,jdate,itime,rlat,rlon,spd,dir,pre,qi
         if (db) write(*,*) ctype,jdate,itime,rlat,rlon,spd,dir,pre,qi
  
         nused=nused+1     

!---------------------------------------------------------------------------------------------------------------------------------------------------------
! build BUFR file
! -- open message (as needed)

         ! 10-digit date (yyyymmddhh)
         write(datechar,fmt='(I4.4,I2.2,I2.2,I2.2)') iyr,mnth,iday,ihr
         read(datechar,fmt='(I10.10)') idate
         if (db) write(*,*) 'idate',idate

         call OPENMB(lunout,subset,idate)


   ! -- populate output array w/ content
   ! -- subset mnemonics:
   !   SWCM YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH WSPD WDIR PRLC PCCF SAID 

         arr=BMISS                                  ! initialize to BUFR missing
         arr(1) = dble(itype)                       ! SWCM
         arr(2) = dble(iyr)                         ! YEAR
         arr(3) = dble(mnth)                        ! MNTH
         arr(4) = dble(iday)                        ! DAYS
         arr(5) = dble(ihr)                         ! HOUR
         arr(6) = dble(imin)                        ! MINU
         arr(7) = dble(isec)                        ! SECO
         arr(8) = dble(rlat)                        ! CLATH
         arr(9) = dble(rlon)                        ! CLONH
         call UFBINT(LUNOUT,arr, 9, 1,iret, &
                    'SWCM YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH')
!----------- SAT ID-------------------------------

         if(said.ge.273 .or. said.le.269) then
            write(6,&
            '('' #####> INVALID INCOMING SATELLITE NUMBER '',i3, &
            '' -- STOP WITH RETURN CODE 88'')') said
            call w3tage('BUFR_TRANCIMSSAMV')
            call errexit(88)
         endif
!----------------------------------------------------------------------------------------------------------------------------------------------------
	 pre = pre*100		!convert pre to pascals

         arr(1) = dble(spd)                        ! WSPD
         arr(2) = dble(dir)                        ! WDIR
         arr(3) = dble(pre)                        ! PRLC
         arr(4) = dble(iGNAPS)                     ! GNAPS
!!!         arr(5) = dble(iGCLONG)                    ! GCLONG
         arr(5) = dble(iOGCE)                      ! OGCE
         arr(6) = dble(qi)                         ! PCCF
         arr(7) = dble(said)                       ! SAID
         call UFBINT(LUNOUT,arr, 8, 1,iret,'WSPD WDIR PRLC GNAPS OGCE PCCF SAID')
         call WRITCP(LUNOUT)  ! write out compressed BUFR messages to save space

         if (db .and. nl.ge.183) exit       ! debug interrupt

       end do read1

200    continue

! -- all reports have been read in

      close(lunin)

      write(*,'(A//A,I10/A,I10)') 'ALL REPORTS PROCESSED',&
       'Number of reports read:    ',nl, 'Number of reports written: ',nused
    
      call CLOSMG(LUNOUT)
      call CLOSBF(LUNOUT)

      call w3tage('BUFR_TRANCIMSSAMV')

      end program BUFR_TRANCIMSSAMV

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    REMTDY
!   PRGMMR: SAGER            ORG: NP12       DATE: 2001-03-20
!
! ABSTRACT: DETERMINES MONTH-OF-YEAR AND DAY-OF-MONTH GIVEN FOUR-DIGIT
!   YEAR AND DAY-OF-YEAR.
!
! PROGRAM HISTORY LOG:
! 2001-03-20  L. SAGER -- ORIGINAL AUTHOR
!
! USAGE:    CALL REMTDY(IYEAR,IDOY,MON,IDAY)
!   INPUT ARGUMENT LIST:
!     IYEAR    - YEAR (YYYY)
!     IDOY     - DAY-OF-YEAR
!
!   OUTPUT ARGUMENT LIST:
!     MON      - MONTH-OF-YEAR
!     IDAY     - DAY-OF-MONTH
!
!   OUTPUT FILES:
!     UNIT 06  - PRINTOUT
!
! REMARKS: THIS SUBROUTINE WILL WORK FROM 1583 A.D. TO 3300 A.D.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 (FREE FORMAT)
!   MACHINE:  NCEP WCOSS
!$$$
      SUBROUTINE REMTDY(IYEAR,IDOY,MON,IDAY)

      INTEGER    IDAT(8)

      DATA IDAT  /0,1,1,5*0/

!     First, calculate the Julian day on Jan. 1 of year.

!!!!! print *,' remtdy   iyear dayyr = ',iyear,idoy
      IDAT(1) = IYEAR
      CALL W3DOXDAT(IDAT,JDOW,JDOY,JDAY)

!!!!! print *,' dox-dow doy day ',jdow,jdoy,jday

!     Add the day-of-year to Julian day.

      jday = jday + idoy - 1
!!!!! print *,' updated jday idoy are ',jday,idoy

!     Call W3FS26 to get month/day from the Julian day.

      CALL W3FS26(JDAY,IYEAR,MON,IDAY,IDAYWK,IDAYYR)
!!!!! print *,' year, month, day = ',iyear,mon,iday

      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
