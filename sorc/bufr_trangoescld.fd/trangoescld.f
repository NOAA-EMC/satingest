C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM:  BUFR_TRANGOESCLD
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2014-01-20
C
C ABSTRACT: READS IN GOES CLOUD TOP DATA FROM NESDIS FLAT FILES IB WMO
C   BUFR FORMAT, REFORMATS AND PACKS INTO AN NCEP BUFR FILE WHICH CAN
C   BE DATABASED BY TRANJB.
C
C PROGRAM HISTORY LOG:
C 2001-04-12  L. SAGER  - ORIGINAL AUTHOR
C 2001-07-31  D. KEYSER - ADDED GOES CLOUD TOP TEMP ("GDCTT") TO
C     LIST OF VARIABLES READ IN AND WRITTEN OUT
C 2006-02-02  D. KEYSER - REPLACED CALL TO BUFRLIB ROUTINE IREADIBM
C     WITH CALL TO BUFRLIB ROUTINE IREADMG (IREADIBM OBSOLETE WITH
C     1/31/2006 VERSION OF BUFRLIB)
C 2006-12-12  D. KEYSER - RENAMED FROM BUFR_TRAN1X1S TO
C     BUFR_TRANGOESCLD SINCE THIS WILL NEVER PROCESS ANYTHING MORE THAN
C     GOES CLOUD DATA (SFOV)
C 2012-11-07  S. Melchior - Changes to run on WCOSS (e.g., replaced
C     dimension declarations with real(8), explicitly declared variable
C     rid; replaced W3LIB with more specific W3NCO).
C 2014-01-20  D. Keyser - MINOR CHANGES
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT. W3TRNARG PARSES ARGUMENTS FROM
C              - STANDARD INPUT.
C     UNIT 11  - WMO BUFR GOES CLOUD TOP DATA
C     UNIT 19  - FOREIGN BUFR TABLE FILE CONTAINING BUFR TABLES A,
C              - B, AND D (FOR UNIT 11).
C     UNIT 20  - BUFR TABLE FILE CONTAINING BUFR TABLES A, B, AND
C                D (FOR UNIT 51).
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 51  - POINTS TO THE OUTPUT BUFR FILE. TRANJB WILL PLACE
C                THE BUFR MESSAGES INTO THE PROPER TANKS.
C
C   SUBPROGRAMS CALLED:
C     UNIQUE   - REMTDY
C     LIBRARY:
C       W3NCO  - W3TRNARG W3TAGB   W3TAGE   W3FS26   W3DOXDAT ERREXIT
C     BUFRLIB  - OPENBF   CLOSBF   OPENMB   UFBSEQ   WRITSB   IREADMG
C              - IREADSB  DATELEN
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =   1 - UNABLE TO PARSE INPUT ARGUMENTS IN W3TRNARG
C          =  99 - UNABLE TO DETERMINE SATELLITE ID FROM INPUT FILE
C          = 253 - NO REPORTS WRITTEN OUT
C
C   REMARKS:
C
C ----------------------------------------------------------------------
C             INPUT GOES CLOUD TOP SEQUENCE OF MNEMONICS (11)
C ----------------------------------------------------------------------
C NC003000 | YEAR DOYR HOUR MINU SAID SIDU CLATH CLONH TOCC CDTP GCDTT
C ----------------------------------------------------------------------
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      PROGRAM BUFR_TRANGOESCLD

      PARAMETER (NDAT=11)
      PARAMETER (NOUT=14)

      real(8),dimension (NDAT) :: BUFRI
      real(8)      DATOUT(NOUT)
      real(8)      rid
      INTEGER*8    IUFRI(NDAT)
      EQUIVALENCE  (IUFRI,BUFRI)

      CHARACTER*8   SID
      CHARACTER*8   SUBSET,TLFLAG,SUBFGN
      CHARACTER*36  STROUT1,STROUT2
      CHARACTER*80  APPCHR,SUBDIR,TANKID

      
      DATA LUNIN /11/
      DATA LINDX /19/
      DATA LUNDX /20/
      DATA LUNOT /51/
      DATA BMISS /10E10/
      DATA SID   /'????????'/
      DATA IDATE_prev/-99/,LDATE_prev/-99/

      DATA STROUT1 /'RPID YEAR MNTH DAYS HOUR MINU SECO  '/
      DATA STROUT2 /'CLAT CLON SAID SIDU CDTP TOCC GCDTT '/

      EQUIVALENCE(SID,RID) 

C-----------------------------------------------------------------------
      CALL W3TAGB('BUFR_TRANGOESCLD',2014,0020,0081,'NP22')
      PRINT *, ' '
      PRINT *, ' ==> Welcome to BUFR_TRANGOESCLD -- Version 01/20/2014'
      PRINT *, ' '
      CALL W3TRNARG(SUBDIR,LSUBDR,TANKID,LTNKID,APPCHR,LAPCHR,
     1              TLFLAG,JDATE,KDATE,IERR)
      IF(IERR.NE.0) THEN
        WRITE(6,'('' UNABLE TO PARSE ARGS TO TRANSLATION ROUTINE - '',
     1            '' RETURN CODE = '',I5)') IERR
        CALL W3TAGE('BUFR_TRANGOESCLD')
        CALL ERREXIT(IERR)
      ENDIF
      SUBSET = 'NC'//SUBDIR(LSUBDR-2:LSUBDR)//TANKID(LTNKID-2:LTNKID)
C-----------------------------------------------------------------------

      ipr = 0
      IRD = 0
      IWT = 0
      KTSKPT=0

      CALL DATELEN(10)

C  OPEN AND READ THRU THE INPUT BUFR FILE
C  --------------------------------------

      CALL OPENBF(LUNIN,'IN',LINDX)
ccccc CALL OPENBF(LUNOT,'OUT',LUNDX)
      CALL OPENBF(LUNOT,'NODX',LUNDX)

C  READ THROUGH THE MESSAGES/SUBSETS IN THE FILE
C  ---------------------------------------------
      icout = 0
      DO WHILE(IREADMG(LUNIN,SUBFGN,IDATE).EQ.0)
         IF(IDATE.NE.IDATE_prev)  then
            print *, ' '
            print *, 'OPENING INPUT  MESSAGE WITH NEW DATE ',IDATE,
     $       ' (SUBSET ',SUBFGN,')'
            print *, ' '
         ENDIF
         IDATE_prev = IDATE
      DO WHILE(IREADSB(LUNIN).EQ.0)

C  READ IN THE ENTIRE REPORT SEQUENCE
C  ----------------------------------

       CALL UFBSEQ(LUNIN,BUFRI,NDAT,1,IRET,SUBFGN)
      IRD=IRD+1
C     IF(ipr .eq. 300) CALL UFBDMP(LUNIN)

      ipr = ipr + 1
C     IF(ipr .lt. 101) THEN
C     print *,' ipr = ',ipr
C     print *,' date ',(BUFRI(kk),kk=1,5)
C     print 112,(IUFRI(kk),kk=1,5)
C112  FORMAT(5z18)
C     print *,' data ',(BUFRI(kk),kk=6,NDAT)
C     print 112,(IUFRI(kk),kk=6,NDAT)
C     print *,'  '
C     ENDIF

C
C     CHECK FOR MISSING LAT LON
C
      ALAT = BUFRI(7)
      ALON = BUFRI(8)
      IF(MAX(ALAT,ALON) .GE. BMISS) THEN
        PRINT *,' ENCOUNTERED MISSING LAT/LON IN SUBSET ',SUBSET
        KTSKPT = KTSKPT + 1
C
C  CHECK THE INTERNAL DATE FOR REALISM
C  -----------------------------------

      ELSE 
      IYR  = NINT(BUFRI(1))
      IDOY = NINT(BUFRI(2))
      IHR  = NINT(BUFRI(3))
      MIN  = NINT(BUFRI(4))
      ISEC = 0 ! Since ISEC aren't currently available set to 0

C  CHECK FOR REPORT WITH VALID TIME STAMP
C  --------------------------------------

      IF(IYR .LT.0 .OR.
     .   IDOY.LT.1 .OR. IDOY.GT.366  .OR.
     .   IHR .LT.0 .OR. IHR .GT. 24  .OR.
     .   MIN .LT.0 .OR. MIN .GT. 60  .OR.
     .   ISEC.LT.0 .OR. ISEC.GT. 60) THEN
        PRINT '("BAD DATE:",2I4,3I3.2," SUBSET:",A8)',
     .         IYR,IDOY,IHR,MIN,ISEC,SUBSET
        KTSKPT=KTSKPT+1
      ELSE

C  CONVERT INPUT DAY OF YEAR TO MONTH AND DAY
C  ------------------------------------------
         CALL REMTDY(IYR,IDOY,MON,IDAY)
C        write(6,'('' time stamp : '',6i4)') iyr,mon,iday,ihr,min,isec

C  CHECK REPORT DATE (YYYYMMDDHH) TO SEE IF A NEW OUTPUT MESSAGE
C  SHOULD BE OPENED (TRANJB TAKES CARE OF THIS FOR UNCOMPRESSED
C  FILES, BUT IT DOESN'T HURT TO HAVE REDUNDANCY BUILT IN HERE)
C  -------------------------------------------------------------

         LDATE = IYR*1000000+MON*10000+IDAY*100+IHR
C        print *,' LDATE ',ldate
C        print *,' SUBSET IS ',SUBSET
         IF(LDATE.NE.LDATE_prev)  then
            print *, ' '
            print *, 'OPENING OUTPUT MESSAGE WITH NEW DATE ',LDATE,
     $       ' (SUBSET ',SUBSET,')'
            print *, ' '
         ENDIF
         LDATE_prev = LDATE
         CALL OPENMB(LUNOT,SUBSET,LDATE)

         DATOUT(1)  = RID
         DATOUT(2)  = IYR
         DATOUT(3)  = MON
         DATOUT(4)  = IDAY
         DATOUT(5)  = IHR
         DATOUT(6)  = MIN
         DATOUT(7)  = ISEC
         DATOUT(8)  = ALAT
         DATOUT(9)  = ALON
         DATOUT(10) = BUFRI(5)
         DATOUT(11) = BUFRI(6)
         DATOUT(12) = BUFRI(10)
         DATOUT(13) = BUFRI(9)
         DATOUT(14) = BUFRI(11)
         
C  WRITE A SUBSET
C  --------------

         CALL UFBINT(LUNOT,DATOUT,NOUT,1,IRET,STROUT1//STROUT2)

         CALL WRITSB(LUNOT)
         IWT=IWT+1

      ENDIF
      ENDIF
      ENDDO
      ENDDO

C  WHEN FINISHED MAKE SURE ALL BUFFERS ARE FLUSHED THEN EXIT
C  ---------------------------------------------------------

      CALL CLOSBF(LUNIN)
      CALL CLOSBF(LUNOT)
      PRINT*,'*** PROCESSING ENDED NORMALLY ***'
      PRINT*,'*** READ :',IRD
      PRINT*,'*** WROT :',IWT
      PRINT*,'*** SKIP :',KTSKPT
      PRINT*,'*** PROCESSING ENDED NORMALLY ***'
      IF(IWT.EQ.0) THEN
        WRITE(6,2003)
 2003   FORMAT(' NO REPORTS PROCESSED -- DISABLING ALL SUBSEQUENT ',
     1         'PROCESSING.')
        CALL W3TAGE('BUFR_TRANGOESCLD')
        CALL ERREXIT(253)
      ENDIF
      CALL W3TAGE('BUFR_TRANGOESCLD')

      STOP
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    REMTDY
C   PRGMMR: SAGER            ORG: NP12       DATE: 2001-03-20
C
C ABSTRACT: DETERMINES MONTH OF YEAR AND DAY OF MONTH GIVEN
C   FOURT-DIGIT YEAR AND DAY OF YEAR.
C
C PROGRAM HISTORY LOG:
C 2001-03-20  L. SAGER -- ORIGINAL AUTHOR
C
C USAGE:    CALL REMTDY(IYEAR,IDOY,MON,IDAY)
C   INPUT ARGUMENT LIST:
C     IYEAR    - YEAR (YYYY)
C     IDOY     - DAY OF YEAR
C
C   OUTPUT ARGUMENT LIST:
C     MON      - MONTH OF YEAR
C     IDAY     - DAY OF MONTH
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C
C REMARKS: THIS SUBROUTINE WILL WORK FROM 1583 A.D. TO 3300 A.D.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE REMTDY(IYEAR,IDOY,MON,IDAY)

      INTEGER    IDAT(8)

      DATA IDAT  /0,1,1,5*0/

C     First, calculate the Julian day on Jan. 1 of year.

ccccc print *,' remtdy   iyear dayyr = ',iyear,idoy 
      IDAT(1) = IYEAR
      CALL W3DOXDAT(IDAT,JDOW,JDOY,JDAY) 

ccccc print *,' dox-dow doy day ',jdow,jdoy,jday

C     Add the day-of-year from the goes report to Julian day.

      jday = jday + idoy - 1
ccccc print *,' updated jday idoy are ',jday,idoy

C     Call W3FS26 to get month/day from the Julian day for goes report.

      CALL W3FS26(JDAY,IYEAR,MON,IDAY,IDAYWK,IDAYYR)
ccccc print *,' year, month, day = ',iyear,mon,iday

      RETURN
      END

