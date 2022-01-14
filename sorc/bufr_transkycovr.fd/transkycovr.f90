!$$$  main program documentation block
!
! Main program: BUFR_TRANSKYCOVR
!   Prgmmr: Keyser           Org: EMC         Date: 2017-11-09
!
! ABSTRACT:  Reads in University of Wisconsin supplied ASCII data files
!   containing GOES Imager Sky Cover data.  This includes effective cloud
!   amount (emissivity) and cloud-top pressure, at point locations.  Both
!   point (center fov pixel) and averaged (fov pixels around center) values
!   are present.  The averaged fov is defined as follows:
!     Take a given GOES Imager pixel somewhere, and call it location (Y,X),
!     where Y = a line (N-S) coordinate, and X = an element (E-W) coordinate.
!     For a given image, there can be literally millions of these pixels, as
!     you can imagine.   Then for that pixel, we determine the # of pixels
!     (using great circle distance) one must travel N, E, S and W to arrive at
!     a location 25km distant from pixel (Y,X).  This forms essentially a 4-
!     sided polygon surrounding pixel (Y,X).  In other words, we will have a
!     2-d matrix of pixels that surround pixel (Y,X).  Then for each pixel that
!     lies within this polygon/matrix, the great circle distance gets computed,
!     and ONLY those pixels whose great circle distance is  <= 25.0km  get used
!     to compute the averaged fov values.  The point here is that we are
!     attempting to only include those pixels in the computations of these two
!     quantities that an earth-based observer would see if they were looking up
!     and around in all directions  (the  "celestial dome").
!   The single, center fov and averaged fov effective cloud amount data are
!   translated into an output BUFR file.
!
! Program history log:
! 2015-01-15  D. A. Keyser   -- Adapted from program PROCESS_GOESIMGR_SKYCOVER
!             written by Carley/Whiting. Updated Docblock.  Steamlined code -
!             removed unused variables and unneeded logic. Update I/O unit
!             numbers to conform with standards used in scripts which execute
!             satellite ingest translation codes. Only processes a single input
!             file rather than multiple input files (no longer needs to read in
!             namelist). Calls W3NCO routine W3TRNARG to obtain sub-directory
!             and BUFR tank filename which are then used to construct BUFR
!             message type mnemonic (conforms to standard for satellite ingest
!             translation codes). No longer does any time window or
!             geographical checking - all reports are processed regardless of
!             their time and location. Obtains BUFR missing value via call to
!             GETBMISS rather than hardwiring it to 10E10. Now uses inline
!             subroutine REMTDY to obtain month and day from day-of-year (had
!             used subroutine DOY2YMD - REMTDY is used by other obs processing
!             codes and will eventually be placed in a library). Performs a
!             sanity check on all values read in first record, to ensure ASCII
!             format is as expected.  Performs a sanity check on effective
!             cloud amount (single, center fov and averaged fov) and checks
!             satellite id for validity for all records read in. Corrected
!             longitude encoded into BUFR to be 0 to +180 for E and 0 to -180
!             for W (was encoded as 0 to 360 E).  Removed encoding of "RPID"
!             which was always missing. Renamed mnemonics "TOCC" and "TOCC_AVG"
!             to "ECAS" and "ECAM", resp. (new local descriptors that define
!             these explicitly as "effective cloud Amount").
! 2017-11-09  D. A. Keyser   -- Updated to handle GOES-16 through GOES-19.
!
! Usage:
!  Input files:
!    Unit 05  - Standard input. W3TRNARG parses arguments from standard input
!    Unit 11  - ASCII file containing GOES Imager effective cloud amount and
!               cloud-top pressure (sky cover) data
!    Unit 20  - BUFR mnemonic table file containing BUFR tables A, B, and D
!
!  Output files:
!    Unit 06  - Standard output print
!    unit 51  - output BUFR format data file later to be appended to BUFR tank
!               containing GOES Imager effective cloud amount data
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
!   Machine:  NCEP WCOSS
!
!$$$

      program BUFR_TRANSKYCOVR

      implicit none

      integer lunin  /11/
      integer luntab /20/
      integer lunout /51/
      integer nl,nused
      integer jdate,itime,isat,iyr,idyr,ierr,iday,mnth,ihr,imin,isec
      integer lsubdr,ltnkid,lapchr,jjdate,kkdate,idate,iret

      real*8 arr(8),bmiss,getbmiss
      real rlat,rlon,eca_avg,eca,dum1,dum2,said

      character*132 line
      character*80  fmt,appchr
      character*12  subdir,tankid
      character*10  datechar
      character*8   subset,tlflag
      character*3   csat

      logical       db   /.false./

      call w3tagb('BUFR_TRANSKYCOVR',2017,0313,0050,'NP22')

      print*
      print*, 'WELCOME TO BUFR_TRANSKYCOVR - VERSION 11-09-2017'
      print*

      !db=.true.    ! debug

      call w3trnarg(subdir,lsubdr,tankid,ltnkid,appchr,lapchr,tlflag,jjdate, &
                    kkdate,ierr)
!.......................................................................
      if(ierr.ne.0) then
         write(6,&
 '('' UNABLE TO PARSE ARGS TO TRANSLATION ROUTINE - RETURN CODE = '',i5)') ierr
         call w3tage('BUFR_TRANSKYCOVR')
         call errexit(ierr)
      endif
!.......................................................................
      subset = 'NC'//subdir(lsubdr-2:lsubdr)//tankid(ltnkid-2:ltnkid)
!!!!! print*, 'SUBSET = ',SUBSET

! Obtain BUFRLIB value for missing

      bmiss = getbmiss()
      print*, 'bmiss returned as ',bmiss
      print*

      call datelen(10)

!!!!! call openbf (LUNOUT,'OUT',LUNTAB) ! Open new output BUFR file
      call openbf (LUNOUT,'NODX',LUNTAB)! Open new output BUFR file

! Loop thru lines of input data
      nl=0
      nused=0

      read1: do
            
! --expected ASCII format:
!   yyyyjjj hhmmss   lat       lon    pix-avg eca  eca  cl-top p satid pix-avg cl-top p
!                    N+,S-      W+         %        %       mb            mb
!   ------- ------  ------   -------- ----------- ----- -------- ----- ----------------
! ' 2014303 190000  67.4361  104.8787    98.01     0.00     0.00 13i     0.00'

         read(lunin,'(a)',end=200) line
         nl=nl+1

!         if (mod(nl,100)==0) print*,'working on report', nl

         if (db) write(*,'(1x,i3,1x,a)') nl,">>>" // trim(line) // "<<<"

         read(line,*) jdate,itime,rlat,rlon,eca_avg,eca,dum1,csat,dum2

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
            call w3tage('BUFR_TRANSKYCOVR')
            call errexit(77)
         endif
67       continue

! -- perform a sanity check of effective cloud amount (both single, center fov
!    and averaged fov) for every report

         if(eca_avg.lt.0. .or. eca_avg.gt.100.)  then
            write(6,&
            '('' #####> INVALID AVERAGED FOV EFFECTIVE CLOUD AMOUNT '',f7.2, &
            '' -- STOP WITH RETURN CODE 66'')') eca_avg
            call w3tage('BUFR_TRANSKYCOVR')
            call errexit(66)
         else if(eca.lt.0. .or. eca.gt.100.)  then
            write(6,&
            '('' #####> INVALID SINGLE, CENTER FOV EFFECTIVE CLOUD AMOUNT '', &
            f7.2,'' -- STOP WITH RETURN CODE 66'')') eca
            call w3tage('BUFR_TRANSKYCOVR')
            call errexit(66)
         endif

         read(csat,'(i2)') isat

! Lon read in is positive W oriented (either 0 to +360 W, or 0 to +180 W and
! 0 to -180 E - not sure which) - convert to positive East oriented (0 to 360 E)

         rlon=(rlon*-1.0)+360.0
         rlon=mod(rlon,360.)

! At this point E lon is BUFR standard (O to +180 E) but W lon is +180 to +360 E
!  which is not BUFR standard of 0 to -180 W - so convert

         if(rlon.gt.180.)  rlon = rlon-360.

         fmt='(1x,i7,1x,i6,1x,f9.4,1x,f10.4,2(1x,f6.2),1x,i2)'
         if (db) write(*,fmt) jdate,itime,rlat,rlon,eca_avg,eca,isat
  
         nused=nused+1     

! build BUFR file
! -- open message (as needed)

         ! 10-digit date (yyyymmddhh)
         write(datechar,fmt='(I4.4,I2.2,I2.2,I2.2)') iyr,mnth,iday,ihr
         read(datechar,fmt='(I10.10)') idate
         if (db) write(*,*) 'idate',idate
         call OPENMB(lunout,subset,idate)


   ! -- populate output array w/ content
   ! -- subset mnemonics:
   !   YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAID ECAS ECAM

         arr=BMISS                                  ! initialize to BUFR missing
         arr(1) = dble(iyr)                         ! YEAR
         arr(2) = dble(mnth)                        ! MNTH
         arr(3) = dble(iday)                        ! DAYS
         arr(4) = dble(ihr)                         ! HOUR
         arr(5) = dble(imin)                        ! MINU
         arr(6) = dble(isec)                        ! SECO
         arr(7) = dble(rlat)                        ! CLATH
         arr(8) = dble(rlon)                        ! CLONH
         call UFBINT(LUNOUT,arr, 8, 1,iret, &
                    'YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH')

! -- convert GOES satellite number to BUFR satellite id
!!   (currently handles GOES-13 to 19)

         if(isat.ge.13 .and. isat.le.15) then
            said = isat + 244
         else if(isat.ge.16 .and. isat.le.19) then
            said = isat + 254
         else
            write(6,&
            '('' #####> INVALID INCOMING SATELLITE NUMBER '',i3, &
            '' -- STOP WITH RETURN CODE 88'')') isat
            call w3tage('BUFR_TRANSKYCOVR')
            call errexit(88)
         endif
         arr(1) = dble(said)                       ! SAID
         arr(2) = dble(eca)                        ! ECAS
         arr(3) = dble(eca_avg)                    ! ECAM
         call UFBINT(LUNOUT,arr, 3, 1,iret,'SAID ECAS ECAM')   
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

      call w3tage('BUFR_TRANSKYCOVR')

      end program BUFR_TRANSKYCOVR

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
