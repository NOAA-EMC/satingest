C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_TRANSSND
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2014-01-20
C
C ABSTRACT: READS IN GOES SOUNDINGS AND RADIANCES FROM NESDIS 
C   ORBIT-BY-ORBIT FILES IN WMO BUFR FORMAT, REFORMATS AND
C   PACKS INTO A BUFR FILE WHICH CAN BE DATABASED BY TRANJB.
C
C PROGRAM HISTORY LOG:
C 1996-07-22  B. KATZ   - ORIGINAL AUTHOR
C 1998-03-23  B. KATZ   - REMOVED W3LOG CALLS
C 1998-12-01  B. KATZ   - CHANGES FOR Y2K AND FORTRAN 90 COMPLIANCE,
C     INCLUDING CHANGES TO FEED OUTPUT TO TRANJB SCRIPT, RATHER THAN
C     WRITE DIRECTLY TO TANKS
C 1999-02-10  B. KATZ   - REPLACED REFERENCE TO CLBUFR BY CALLS TO
C     CLOSBF
C 1999-08-25  D. KEYSER - ADDED ERROR HANDLING WHEN NO OUTPUT IS
C     CREATED SO THAT SUBSEQUENT TRANJB'S ARE SKIPPED.
C 2000-04-27  D. KEYSER - FIXED BAD DATE DIAGNOSTIC PRINT TO LIST
C     VALUES FOR HOUR AND MINUTE AND PROPER VALUE FOR BUFR SUBSET
C 2000-11-09  D. KEYSER - ADDED ENCODING OF REPORT ID ("RPID"), SET TO
C     "????????". SKIPS OVER "FILLER" REPORTS WHICH CONTAIN NO
C     INFORMATION (INDICATED BY MISSING LAT/LON).
C 2001-03-30  D. KEYSER - NOW CHECKS YYYYMMDDHH OF EACH REPORT TO SEE
C     IF A NEW OUTPUT MESSAGE SHOULD BE OPENED WITH THIS DATE, ENSURES
C     THAT OUTPUT FILE BUFR MESSAGES CONTAIN ONLY REPORTS WITH SAME
C     YYYYMMDDHH AS MESSAGE (THIS IS A REDUNDANT CHECK FOR UNCOMPRESSED
C     FILES SINCE SUBSEQUENT BUFR_TRANJB PROGRAM ALSO DOES THIS, BUT
C     THIS CHECK DOESN'T COST ANY TIME AND IT WILL BE IMPORTANT IF
C     THESE FILES ARE EVER COMPRESSED); STREAMLINED CODE
C 2001-05-03  D. KEYSER - CORRECTED AN ERROR INTRODUCED IN PREVIOUS
C     IMPLEMENTATION WHICH LED TO ALL REPORTS WITH AN ACTUAL HOUR PRIOR
C     TO 00Z BEING ASSIGNED A YEAR, MONTH AND DAY 24-HOURS LATER THAN
C     ACTUAL WHEN THE REPORTS WERE IN A FILE WITH AN HOUR QUALIFIER OF
C     00 (THE FIRST FILE OF THE NEXT, NEW DAY) (ALL SUCH REPORTS WERE
C     LATER REJECTED IN THE BUFR_TRANJB DATE CHECKING ROUTINE)
C 2001-05-18  D. KEYSER - REFINED CONVERSION FROM GROUP YEAR, MONTH,
C     DAY TO ACTUAL YEAR, MONTH, DAY: IF GROUP HOUR IS 00 OR 01 AND
C     ACTUAL HOUR IS 22 OR 23, ACTUAL YEAR, MONTH AND DAY IS 1-DAY
C     EARLIER THAN GROUP YEAR MONTH DAY, OTHERWISE THEY ARE THE SAME
C     (PRIOR TO THIS THE DAY WAS REDUCED BY ONE ONLY WHEN THE GROUP
C     HOUR WAS 00 AND THE ACTUAL HOUR WAS NOT 00, REPORTS WITH GROUP
C     HOUR 01 AND ACTUAL HOUR 23 WERE ASSIGNED THE WRONG YEAR, MONTH
C     AND DAY)
C 2006-02-02  D. KEYSER - REPLACED LOCAL MNEMONIC "PH2O" WITH WMO
C     MNEMONIC "TPWT" (CORRESPONDING TO SIMULTANEOUS CHANGE IN BUFR
C     TABLE IN UNIT 20); ADDED "DINU" (DETECTOR INSTRUMENT NUMBER) TO
C     LIST OF PARAMETERS TRANSFERRED TO NCEP BUFR FILE (ONLY PRESENT IN
C     GOES 1x1 REPORTS); REPLACED CALL TO BUFRLIB ROUTINE IREADIBM WITH
C     CALL TO BUFRLIB ROUTINE IREADMG (IREADIBM OBSOLETE WITH 1/31/2006
C     VERSION OF BUFRLIB)
C 2007-08-07  D. KEYSER - LIMITS NUMBER OF LINES THAT ARE PRINTED WHEN
C     REPORT LAT/LON IS MISSING TO 100 (USED TO PRINT ALL REPORTS)
C 2012-11-07  S. Melchior - Changes to run on WCOSS (e.g., replaced all
C     dimension declarations with real or real(8), explicitly declared
C     variable rid; replaced W3LIB with more specific W3NCO).
C 2014-01-20  D. KEYSER - MINOR CHANGES
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT. W3TRNARG PARSES ARGUMENTS FROM
C              - STANDARD INPUT.
C     UNIT 11  - NESDIS WMO BUFR FILE.
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
C              - W3MOVDAT
C     BUFRLIB  - OPENBF   CLOSBF   OPENMB   UFBINT   UFBREP   WRITSB
C              - IREADMG  IREADSB  DATELEN
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =   1 - UNABLE TO PARSE INPUT ARGUMENTS IN W3TRNARG
C          = 253 - NO REPORTS WRITTEN OUT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      PROGRAM BUFR_TRANSSND

      real(8)    DATES(5,2)
      real(8)    DATOUT(7)
      real(8)    XLOCAT(2),SATINF(7)
      real(8)    TEMPDB(2,41),TEMPDP(2,41)
      real(8)    BRIGHT(72)
      real(8)    BRTOUT(2,18)
      real(8)    SUNDRY(7)
      real(8)    PRECWT(5)
      real(8)    PRCOUT(4)
      real(8)    PRSHGT(2,41),RETOUT(4,41)
      real(8)    rid
      real       KDAT(8)

      CHARACTER*8  SUBSET,SUBFGN,SID

      CHARACTER*12 SUBDIR,TANKID
      CHARACTER*80 APPCHR
      CHARACTER*8 TLFLAG

      EQUIVALENCE (RID,SID)

      DATA LUNIN /11/
      DATA LINDX /19/
      DATA LUNDX /20/
      DATA LUNOT /51/
      DATA BMISS /10E10/

      DATA IDATE_prev/-99/,LDATE_prev/-99/

C-----------------------------------------------------------------------
      CALL W3TAGB('BUFR_TRANSSND',2014,0020,0082,'NP22') 
      PRINT *, ' '
      PRINT *, ' ==> Welcome to BUFR_TRANSSND -- Version 01/20/2014'
      PRINT *, ' '
      CALL W3TRNARG(SUBDIR,LSUBDR,TANKID,LTNKID,APPCHR,LAPCHR,
     1              TLFLAG,JDATE,KDATE,IERR)
      IF(IERR.NE.0) THEN
        WRITE(6,'('' UNABLE TO PARSE ARGS TO TRANSLATION ROUTINE - '',
     1            '' RETURN CODE = '',I5)') IERR
        CALL W3TAGE('BUFR_TRANSSND')
        CALL ERREXIT(IERR)
      ENDIF
      SUBSET = 'NC'//SUBDIR(LSUBDR-2:LSUBDR)//TANKID(LTNKID-2:LTNKID)
C-----------------------------------------------------------------------

      IRD = 0
      IWT = 0
      KTSKPT=0
      IMSG_LALO = 0

      CALL DATELEN(10)

ccccc CALL OPENBF(0,'QUIET',2) ! Uncomment for extra print from bufrlib

C  OPEN AND READ THRU THE INPUT BUFR FILE                               
C  --------------------------------------                               

      CALL OPENBF(LUNIN,'IN',LINDX)
ccccc CALL OPENBF(LUNOT,'OUT',LUNDX)
      CALL OPENBF(LUNOT,'NODX',LUNDX)

C  READ THROUGH THE MESSAGES/SUBSETS IN THE FILE                        
C  ---------------------------------------------                        

      DO WHILE(IREADMG(LUNIN,SUBFGN,IDATE).EQ.0)
ccccc    print *,' IDATE ',idate
ccccc    print *,' SUBFGN IS ',SUBFGN
         IF(IDATE.NE.IDATE_prev)  then
            print *, ' '
            print *, 'OPENING INPUT  MESSAGE WITH NEW DATE ',IDATE,
     $       ' (SUBSET ',SUBFGN,')'
            print *, ' '
         ENDIF
         IDATE_prev = IDATE
      DO WHILE(IREADSB(LUNIN).EQ.0)

C  READ THE INTERNAL LAT/LON AND DATE AND CHECK FOR REALISM
C  --------------------------------------------------------

      CALL UFBINT(LUNIN,XLOCAT,2,1,IRET,'CLATH CLONH')
ccccc write(6,'('' latitude, longitude :'',1p2e12.4)') xlocat
      ALAT   = XLOCAT(1)
      ALON   = XLOCAT(2)
      CALL UFBREP(LUNIN,DATES,5,2,IRET,'YEAR DOYR HOUR MINU SECO')
cppppp
cccc  write(6,'('' INPUT GROUP DATE (YYYY DDD HH MM SS)    :'',
cccc . 1x,2i4,2x,3i3.2)') nint(dates(1:5,1))
cccc  write(6,'('' INPUT PROCESSING DATE (YYYY DDD)        :'',
cccc . 1x,2i4)') nint(dates(1:2,2))
cccc  write(6,'('' INPUT ACTUAL DATE (HH MM SS)            :'',
cccc . 11x,3i3.2)') nint(dates(3:5,2))
cppppp
      IYRFIL  = NINT(DATES(1,1))
      IDOYFIL = NINT(DATES(2,1))
      IHRFIL  = NINT(DATES(3,1))
      IHR     = NINT(DATES(3,2))
      MIN     = NINT(DATES(4,2))
      ISEC    = NINT(DATES(5,2))
      IRD = IRD+1

      IF(MAX(ALAT,ALON) .GE. BMISS) THEN
        IMSG_LALO=IMSG_LALO+1
        IF(IMSG_LALO.LE.100)
     $   PRINT '(" MISSING LAT/LON: SUBSET:",A8)', SUBSET
        KTSKPT=KTSKPT+1
      ELSE IF(IYRFIL .LT.0 .OR.
     .        IDOYFIL.LT.1 .OR. IDOYFIL.GT.366 .OR.
     .        IHRFIL .LT.0 .OR. IHRFIL .GT. 24 .OR.
     .        IHR    .LT.0 .OR. IHR    .GT. 24 .OR.
     .        MIN    .LT.0 .OR. MIN    .GT. 60 .OR.
     .        ISEC   .LT.0 .OR. ISEC   .GT. 60) THEN
        PRINT '("BAD DATE:: GROUP(YYYY DDD HH):",2I4,I3.2,
     .          " ACTUAL(HH MM SS):",3I3.2," SUBSET:",A8)',
     .         IYRFIL,IDOYFIL,IHRFIL,IHR,MIN,ISEC,SUBSET
        KTSKPT=KTSKPT+1
      ELSE

C  CONVERT INPUT GROUP DAY OF YEAR TO GROUP MONTH AND DAY
C  ------------------------------------------------------

         CALL REMTDY(IYRFIL,IDOYFIL,MONFIL,IDAYFIL)
cppppp
cccc     write(6,'('' CONVERTED GROUP DATE (MM DD)            :'',
cccc .    5x,2i3.2)') monfil,idayfil
cppppp

C  CONVERT GROUP YEAR, MONTH AND DAY TO ACTUAL VALUES
C   - if group hour is 00 or 01 and actual hour is
C     22 or 23, actual year month and day is 1-day
C     earlier than group year month day; otherwise
C     they are the same
C  --------------------------------------------------

         IF(IHRFIL.LE.01 .AND. IHR.GE.22) THEN
            CALL W3MOVDAT((/-1.0,0.,0.,0.,0./),(/IYRFIL,MONFIL,IDAYFIL,
     .                    0,0,0,0,0/),KDAT)
            IYR  = KDAT(1)
            MON  = KDAT(2)
            IDAY = KDAT(3)
         ELSE
            IYR  = IYRFIL
            MON  = MONFIL
            IDAY = IDAYFIL
         END IF

         DATOUT(1) = IYR
         DATOUT(2) = MON
         DATOUT(3) = IDAY
         DATOUT(4:6) = DATES(3:5,2)
cppppp
cccc     write(6,'('' OUTPUT ACTUAL DATE (YYYY MM DD HH MM SS):'',
cccc .    1x,i4,5i3.2)') nint(datout(1:6))
cppppp
         SID = '????????'
         DATOUT(7) = RID

         CALL UFBINT(LUNIN,SATINF,7,1,IRET,
     1               'SAID SIDU GSDP QMRK ACAV DINU TCSF')
ccccc write(6,'('' satid  instr. data  q.mk  flds-of-view dectector #''/
ccccc1          f6.1,8x,f6.1,6x,f6.1,6x,f6.1,6x,f6.1,6x,f6.1)')
ccccc2  (satinf(ii),ii=1,6)

         CALL UFBREP(LUNIN,BRIGHT,1,72,IRET,'TMBR')
ccccc write(6,'('' brightness temperatures : ''/(1x,1p4e12.4))')
ccccc1     (bright(i),bright(i+18),bright(i+36),bright(i+54),i=1,18)
         DO  I = 1,18
            BRTOUT(1,I) = I
         ENDDO
         BRTOUT(2,1:18) = BRIGHT(1:18)

         CALL UFBINT(LUNIN,SUNDRY,7,1,IRET,
     1               'SOEL ELEV GLFTI CLAM CDTP GCDTT TMSK')
ccccc write(6,'('' solar zenith sat. zenith  cloud amt. cld top prs''
ccccc1          '' cld top tmp   skin temp''/1x,1p6e12.4)') sundry

         CALL UFBREP(LUNIN,PRECWT,1,5,IRET,'TPWT')
ccccc write(6,'(''   tot prch2o guess prch2o 3layr prch2o''/
ccccc1          1x,1p5e12.4))') precwt
         PRCOUT(1) = PRECWT(1)
         PRCOUT(2:4) = PRECWT(3:5)

         CALL UFBREP(LUNIN,PRSHGT,2,41,IRET,'PRLC HITE')
ccccc write(6,'('' pressure and geop height : ''/(1x,1p2e12.4))') prshgt
         CALL UFBREP(LUNIN,TEMPDB,1,82,IRET,'TMDB')
         CALL UFBREP(LUNIN,TEMPDP,1,82,IRET,'TMDP')
ccccc write(6,'('' temperature and dew point : ''/(1x,1p4e12.4))')
ccccc1     (tempdb(1,i),tempdp(1,i),tempdb(2,i),tempdp(2,i),i=1,41)
         RETOUT(1,1:41) = PRSHGT(1,1:41)
         RETOUT(2,1:41) = TEMPDB(1,1:41)
         RETOUT(3,1:41) = TEMPDP(1,1:41)
         RETOUT(4,1:41) = PRSHGT(2,1:41)

C  CHECK REPORT DATE (YYYYMMDDHH) TO SEE IF A NEW OUTPUT MESSAGE
C  SHOULD BE OPENED (TRANJB TAKES CARE OF THIS FOR UNCOMPRESSED
C  FILES, BUT IT DOESN'T HURT TO HAVE REDUNDANCY BUILT IN HERE)
C  -------------------------------------------------------------

         LDATE = IYRFIL*1000000+MON*10000+IDAY*100+IHR
ccccc    print *,' LDATE ',ldate
ccccc    print *,' SUBSET IS ',SUBSET
         IF(LDATE.NE.LDATE_prev)  then
            print *, ' '
            print *, 'OPENING OUTPUT MESSAGE WITH NEW DATE ',LDATE,
     $       ' (SUBSET ',SUBSET,')'
            print *, ' '
         ENDIF
         LDATE_prev = LDATE
         CALL OPENMB(LUNOT,SUBSET,LDATE)

C  WRITE A SUBSET                 
C  --------------                 

        CALL UFBINT(LUNOT,DATOUT,7,1,IRET,
     1              'YEAR MNTH DAYS HOUR MINU SECO RPID')
        CALL UFBINT(LUNOT,XLOCAT,2,1,IRET,'CLAT CLON')
        CALL UFBINT(LUNOT,SATINF,7,1,IRET,
     1              'SAID SIDU GSDP QMRK ACAV DINU TCSF')
        CALL UFBINT(LUNOT,SUNDRY,7,1,IRET,
     1              'SOEL ELEV GLFTI CLAM CDTP GCDTT TMSK')
        CALL UFBINT(LUNOT,BRTOUT,2,18,IRET,'CHNM TMBR')
        CALL UFBINT(LUNOT,PRCOUT,4,1,IRET,'TPWT PH2O19 PH2O97 PH2O73')
        CALL UFBINT(LUNOT,RETOUT,4,41,IRET,'PRLC TMDB TMDP HGHT')
        CALL WRITSB(LUNOT)

        IWT = IWT+1

      ENDIF
      ENDDO
      ENDDO

C  WHEN FINISHED MAKE SURE ALL BUFFERS ARE FLUSHED THEN EXIT            
C  ---------------------------------------------------------            

      CALL CLOSBF(LUNIN)
      CALL CLOSBF(LUNOT)
      IF(IMSG_LALO.GT.100)  THEN
         PRINT'(/I6," OCCURRENCES OF MISSING LAT/LON (ONLY FIRST 100 ",
     $    "PRINTED) - SUBSET ",A8/)', IMSG_LALO,SUBSET
      ENDIF
      PRINT*,'*** PROCESSING ENDED NORMALLY ***'
      PRINT*,'*** READ :',IRD
      PRINT*,'*** WROT :',IWT
      PRINT*,'*** SKIP :',KTSKPT
      PRINT*,'*** PROCESSING ENDED NORMALLY ***'
      IF(IWT.EQ.0) THEN
        WRITE(6,2003)
 2003   FORMAT(' NO REPORTS PROCESSED -- DISABLING ALL SUBSEQUENT ',
     1         'PROCESSING.')
        CALL W3TAGE('BUFR_TRANSSND')
        CALL ERREXIT(253)
      ENDIF
      CALL W3TAGE('BUFR_TRANSSND')

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
C$$$
      SUBROUTINE REMTDY(IYEAR,IDOY,MON,IDAY)

      INTEGER    IDAT(8)

      DATA IDAT  /0,1,1,5*0/

C     First, calculate the Julian day on Jan. 1 of year.

ccccc print *,' remtdy   iyear dayyr = ',iyear,idoy
      IDAT(1) = IYEAR
      CALL W3DOXDAT(IDAT,JDOW,JDOY,JDAY)

ccccc print *,' dox-dow doy day ',jdow,jdoy,jday

C     Add the day-of-year from the sbuv report to Julian day.

      jday = jday + idoy - 1
ccccc print *,' updated jday idoy are ',jday,idoy

C     Call W3FS26 to get month/day from the Julian day for sbuv report.

      CALL W3FS26(JDAY,IYEAR,MON,IDAY,IDAYWK,IDAYYR)
ccccc print *,' year, month, day = ',iyear,mon,iday

      RETURN
      END

