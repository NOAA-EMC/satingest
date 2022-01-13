      PROGRAM BUFR_TRANPOESSST_NAVO

C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_TRANPOESSST_NAVO
C   PRGMMR: NADIGA           ORG: NP22        DATE: 2019-10-09
C
C ABSTRACT: READS IN NAVO FILES CONTAINING SEA SURFACE TEMPERATURE AND
C   AVHRR BRIGHTNESS TEMPERATURE/ALBEDO DATA FROM NOAA AND METOP POES.  
C   IT THEN WRITES OUT BUFR MESSAGES TO FILES WHICH ARE LATER APPENDED
C   TO THE BUFR DATABASE BY PROGRAM BUFR_TRANJB.
C
C PROGRAM HISTORY LOG:
C 2014-04-14  D. STOKES - ADAPTED FROM BUFR_TRANPOESSST
C 2019-10-09  S. Nadiga - Modified to shift the Y2K windowing technique
C      that converts 2-digit years to 4-digit.
C
C USAGE:
C
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT. W3TRNARG PARSES ARGUMENTS FROM
C              - STANDARD INPUT.
C     UNIT 11  - BINARY SATELLITE SST/BTEMP/ALBEDO FILE (FROM NAVOCEANO)
C     UNIT 15  - NAMELIST WITH SATELLITE INFORMATION.
C     UNIT 20  - BUFR TABLE FILE CONTAINING BUFR TABLES A, B, AND D.
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 51  - BUFR FILE CONTAINING SST/BTEMP/ALBEDO
C     UNIT 71  - TEXT FILE CONTAINING START/END TIME
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:  - SSTRPT   
C     LIBRARY:
C       W3NCO  - W3TRNARG W3MOVDAT W3TAGB   W3TAGE
C     BUFRLIB  - OPENBF   CLOSBF   OPENMG   UFBINT   WRITSB
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =   1 - UNABLE TO PARSE INPUT ARGUMENTS IN W3TRNARG
C          =   4 - UNRECOGNIZED SATELLITE ID READ (NON-FATAL)
C          =  99 - DATA SOURCE IS NOT RECOGNIZED (NOT NAVOCEANO)
C          = 255 - ERROR READING INPUT FILE        
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM WCOSS
C
C$$$

      CHARACTER*1  CBUFTN(104)
      CHARACTER*8  TLFLAG,SUBSET
      CHARACTER*9  TYPE
      CHARACTER*12 SUBDIR,TANKID
      CHARACTER*80 APPCHR

      INTEGER      ITEMP(38),KOUNT(20)
      INTEGER      IBFUNIT,IREC,IYMD,ITXTUNIT
      integer(8)   itime,itime_min,itime_max

      integer(4), parameter :: lunamelst=15
      integer(4), parameter :: max_sat=20
      integer(4),dimension(max_sat+1)::isaid,isstsrc
      character(8),dimension(max_sat+1)::csat_name
      namelist /setup/ nsat
      namelist /sat_list/ isstsrc,isaid,csat_name

      DATA INUNIT  /11/
      DATA INDX    /20/
      DATA IBFUNIT /51/
      DATA ITXTUNIT /71/

      DATA BMISS  /10E10/

C-----------------------------------------------------------------------
      CALL W3TAGB('BUFR_TRANPOESSST_NAVO',2019,0282,0050,'NP22') 
      PRINT *
      PRINT *, ' ==> Welcome to BUFR_TRANPOESSST_NAVO -- ',
     1                                              'Version 10/09/2019'
      PRINT *
      CALL W3TRNARG(SUBDIR,LSUBDR,TANKID,LTNKID,APPCHR,LAPCHR,
     1              TLFLAG,JDATE,KDATE,IERR)
      IF(IERR.NE.0) THEN
        WRITE(6,'('' UNABLE TO PARSE ARGS TO TRANSLATION ROUTINE - '',
     1            '' RETURN CODE = '',I5)') IERR
        CALL W3TAGE('BUFR_TRANPOESSST_NAVO') 
        CALL err_exit(IERR)
      ENDIF
      SUBSET = 'NC'//SUBDIR(LSUBDR-2:LSUBDR)//TANKID(LTNKID-2:LTNKID)
C-----------------------------------------------------------------------

      IF(SUBSET.EQ.'NC012012') THEN
         TYPE = 'NAVOCEANO'
      ELSE
         WRITE(6,9001) SUBSET
 9001 FORMAT(' ***** ERROR: UNEXPECTED DATA SOURCE. SUBSET IS ',A,
     1       '.  THIS CODE IS SPECIFIC TO NAVOCEANO ',A)
         CALL W3TAGE('BUFR_TRANPOESSST_NAVO')
         CALL err_exit(99)
      ENDIF

      PRINT *, '     Source of data is ',type
      PRINT *

C  Get info for any expected satellite from namelist
C   First get number of platforms in list
      read(lunamelst,setup,iostat=ios)
      if(ios.ne.0)then
        print'(a,i0,a,i0)', '*** WARNING *** IOSTAT=',ios,
     2         ' reading setup namelist from unit ', lunamelst
        stop 88
      endif
      if(nsat.gt.max_sat)then
        print'(a,i0,a,i0,a)', '*** WARNING: Input nsat=',nsat,
     1   ' greater than max_sat=',max_sat,'.  Reduce number of ',
     2   ' satellites or rebuild with increased max_sat.'
         stop 89
      endif
C   Now get the information for each platform
      read(lunamelst,sat_list,iostat=ios)
      if(ios.ne.0)then
        print'(a,i0,a,i0)', '*** WARNING *** IOSTAT=',ios,
     2         ' reading sat_list namelist from unit ', lunamelst
        stop 88
      endif
      csat_name(nsat+1)='UNKNOWN'

      IREC = 0
      IRECALL = 0
      IYMD = 0
      ISTOP = 0
      itime_min=9e11
      itime_max=-itime_min 

      open(inunit,access='stream',form='unformatted')

   10 CONTINUE

C  Read in next record containing one SST/BTEMP/ALBEDO "report"
C  ------------------------------------------------------------

      READ(INUNIT,ERR=8888,END=9999) CBUFTN

      IRECALL = IRECALL + 1

      DO I = 9 , 28
        ITEMP(I-8) = MOVA2I(CBUFTN(I))
      ENDDO
      DO I = 31 , 34
        ITEMP(I-10) = MOVA2I(CBUFTN(I))
      ENDDO
      DO I = 39 , 48
        ITEMP(I-14) = MOVA2I(CBUFTN(I))
      ENDDO
      DO I = 59 , 62
        ITEMP(I-24) = MOVA2I(CBUFTN(I))
      ENDDO

      ISAT = -99
      do i=1,nsat
        if(itemp(2).eq.isstsrc(i))then
          satid=isaid(i)
          isat=i
          exit
        endif
      enddo

      IF(ISAT.EQ.-99) THEN
        IF(ITEMP(2).GT.0.AND.ITEMP(2).LT.21) THEN
           KOUNT(ITEMP(2)) = KOUNT(ITEMP(2)) + 1
           IF(KOUNT(ITEMP(2)).LT.51) THEN
              PRINT*, 'WARNING: SATELLITE NUMBER ASSOCIATED WITH ',
     1                'ITEMP(2) = ',ITEMP(2), ' NOT RECOGNIZED, STORE ',
     1                'WITH MISSING SATELLITE ID'
           ELSE IF(KOUNT(ITEMP(2)).EQ.51) THEN
              PRINT*, '         ONLY FIRST 50 REPORTS WITH ITEMP(2) = ',
     1                ITEMP(2), ' PRINTED HERE'
           ENDIF
        ELSE
           PRINT*, 'WARNING: SATELLITE NUMBER ASSOCIATED WITH ITEMP(2)',
     1             ' = ',ITEMP(2), ' NOT RECOGNIZED,  STORE WITH ',
     1             ' MISSING SATELLITE ID'
        ENDIF
        SATID = BMISS ! UNKNOWN SATELLITE (STORE SAT. ID AS MISSING)
        ISAT = nsat+1
        ISTOP = 4
      ENDIF
      
      IYR1      = ITEMP(35) ! CBUFTN(59)
      IYR2      = ITEMP(36) ! CBUFTN(60)
      IYR     = 256 * IYR1 + IYR2

      IF(IYR.LE.1200 .OR. IYR.GE.2050) THEN

C  If 2-digit year - must use "windowing" technique to get 4-digit year
c  iyr=00-40; then iyr=2000-2040; if IYR=41-99; then IYR=1941-1999
C  --------------------------------------------------------------------

         IYR       = ITEMP(3) ! CBUFTN(11)
         IF(IYR.LT.41) THEN
            IYR     = 2000 + IYR
         ELSEIF(IYR.LT.100) THEN
            IYR     = 1900 + IYR
         ENDIF
      ENDIF

                             ! CBUFTN(12)     CBUFRTN(17)
      IDATE = IYR*1000000 + ITEMP(4)*10000 + ITEMP(9)*100 +
     1        ITEMP(10)
            ! CBUFTN(18)

      IF(IREC.EQ.0) THEN

C  Open appropriate BUFR output file
C  ---------------------------------

! NOTE: Output files must have dx table at top because they are now
!       read in a separate job downstream rather than in the same job
!       as this one (as before) - comment out the 'NODX' OPENBF call
        CALL OPENBF(IBFUNIT,'OUT',INDX)

        CALL OPENMG(IBFUNIT,SUBSET,IDATE)
      ENDIF

C  Call SSTRPT to encode report into BUFR message
C  ----------------------------------------------

      CALL SSTRPT(IBFUNIT,ITEMP,SUBSET,IYR,SATID,TYPE,IERR)

      IF(IERR.EQ.0) THEN
        IREC = IREC + 1
        imo=itemp(4)
        idy=itemp(9)
        ihr=itemp(10)
        imn=itemp(11)
        itime=iyr*1e8_8+imo*1e6_8+idy*1e4_8+ihr*1e2_8+imn
        if(itime.lt.itime_min)itime_min=itime
        if(itime.gt.itime_max)itime_max=itime
      ELSE
        IF(IREC.EQ.0) CALL CLOSBF(IBFUNIT)
      ENDIF

C  Go back and read next reocrd
C  ----------------------------

      GO TO 10

 8888 CONTINUE

C  I/O ERROR EXIT
C  --------------

      WRITE(6,1001) IRECALL
 1001 FORMAT(' ERROR ENCOUNTERED IN SST INPUT FILE AFTER RECORD NO. ',
     1       I8)
      CALL W3TAGE('BUFR_TRANPOESSST_NAVO') 
      CALL err_exit(255)

 9999 CONTINUE

C  NORMAL EXIT - ALL INPUT RECORDS READ
C  ------------------------------------

      WRITE(6,1003) IREC,itime_min,itime_max
 1003 FORMAT(i0,' REPORTS PROCESSED. MIN/MAX TIME:',2I13)

      DO I = 1,20
         IF(KOUNT(I).GT.0)  PRINT*, 'A TOTAL OF ',KOUNT(I),' REPORTS ',
     1    'WERE STORED W/ MISSING SAT ID DUE TO THEIR HAVING AN ',
     1    'UNRECOGNIZED SAT NUMBER ASSOC W/ ITEMP(2) = ',I
      ENDDO

      IF(IREC.GT.0) CALL CLOSBF(IBFUNIT)
      WRITE(ITXTUNIT,'(i0,1x,i0)') itime_min, itime_max
      CLOSE(ITXTUNIT)

      CALL W3TAGE('BUFR_TRANPOESSST_NAVO') 

      IF(ISTOP.EQ.4) CALL err_exit(04)
      STOP

      END


      SUBROUTINE SSTRPT(IUNT,ITEMP,SUBSET,IYR,SATID,TYPE,IERR)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C
C SUBPROGRAM:    SSTRPT
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2014-01-02           
C                                                                       
C ABSTRACT: EXTRACTS SEA SURFACE TEMPERATURE, BRIGHTNESS TEMPERATURE
C   AND ALBEDO FROM A SINGLE SATELLITE REPORT, PLACES INTO ARRAY AND
C   ENCODES INTO A BUFR MESSAGE.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C 1996-09-03  B. KATZ   - ORIGINAL AUTHOR
C 1998-11-27  B. KATZ   - CHANGES FOR Y2K AND FORTRAN 90 COMPLIANCE,
C    INCLUDING CHANGES TO FEED OUTPUT TO TRANJB SCRIPT, RATHER THAN
C    WRITE DIRECTLY TO TANKS
C 1999-02-10  B. KATZ   - ADDED ARGUMENT FOR OUTPUT BUFR FILE; MOVED
C    FILE OPENING TO MAIN ROUTINE.
C 2002-02-11  D. KEYSER - REMOVED UNUSED ARGUMENTS
C 2005-09-27  B. KATZ   - ADDED EXTRACTION OF AVHRR BRIGHTNESS TEMP AND
C    ALBEDO AND THEIR SUBSEQUENT ENCODING INTO OUTPUT BUFR FILES
C    (NEEDED AS PART OF UPDATE OF EXECUTING JOB TO PERFORM PHYSICAL
C    RETRIEVALS OF SST USING THESE DATA AS INPUT TO THE GSI)
C 2006-03-23  D. KEYSER - IMPROVE, CORRECT, AND AUGMENT BOTH COMMENTS
C    AND THE SUBPROGRAM DOCBLOCK; STREAMLINE ITS LOGIC; AND FIX MINOR
C    "BUGS" IN THE CODE
C 2011-02-14  D. KEYSER - CORRECTED TO STORE ALBEDO INSTEAD OF
C    BRIGHTNESS TEMPERATURE IN CHANNEL THREE IF VALUE BEING STORED IS
C    < 101 {A.M. SATELLITES STORE ALBEDO IN CHANNEL 3 50% OF THE
C    TIME (DAYTIME) AND STORE BRIGHTNESS TEMPERATURE IN CHANNEL 3 50%
C    OF THE TIME (NIGHTTIME)} (BEFORE ALWAYS ASSUMED BRIGHTNESS
C    TEMPERAURE WAS STORED IN CHANNEL 3, LED TO SOME VERY SMALL VALUES
C    BEING ENCODED INTO OUTPUT BUFR FILE)
C 2014-01-02  D. KEYSER -  MNEMONIC "OPTH" (AEROSOL OPTICAL THICKNESS)
C    IS NOW ALWAYS STORED AS MISSING FOR INPUT NESDIS DATA BECAUSE
C    NESDIS STORES A MEANINGLESS PLACEHOLDER FOR THIS (IT IS VALID FOR
C    INPUT NAVOCEANO DATA AND STORED AS SUCH IN THIS CASE); ADDED NEW
C    INPUT ARGUMENT "TYPE" TO DIFFERENTIATE BETWEEN NESDIS AND
C    NAVOCEANO AS SOURCE OF DATA
C                                                                       
C USAGE:    CALL SSTRPT(IUNT,ITEMP,SUBSET,IYR,SATID,TYPE,ERR)
C   INPUT ARGUMENT LIST:                                                
C     IUNT     - UNIT NUMBER FOR OUTPUT BUFR FILE
C     ITEMP    - SINGLE REPORT OF SST/BTEMP/ALBEDO DATA, CONTAINED IN
C                38-MEMBER INTEGER ARRAY
C     SUBSET   - 8 CHARACTER BUFR MESSAGE TYPE
C     IYR      - 4-DIGIT YEAR OF OBSERVATION
C     SATID    - SATELLITE ID AS DEFINED IN BUFR
C     TYPE     - 9 CHARACTER SOURCE OF DATA TYPE (EITHER ' NESDIS  ' OR
C               'NAVOCEANO')
C
C   OUTPUT ARGUMENT LIST:
C     IERR     - ERROR RETURN
C                 = 0 - NORMAL COMPLETION
C                 = 1 - DATA FAILS TIME-ACCEPTANCE CHECK
C                                                                       
C   OUTPUT FILES:
C     UNIT 06   - STANDARD OUTPUT PRINT
C     UNIT IUNT - BUFR FILE CONTAINING SST/BTEMP/ALBEDO
C
C   SUBPROGRAMS CALLED :
C     LIBRARY:
C     BUFRLIB  - UFBINT   WRITSB   
C
C REMARKS:                                                              
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  IBM-SP
C                                                                       
C$$$                                                                    

      CHARACTER*80  IDST1,IDST2,IDCHN
      CHARACTER*9   TYPE
      CHARACTER*8   SUBSET

      INTEGER       ITEMP(38)

      DATA IDST1  /'YEAR MNTH DAYS HOUR MINU SECO SSTYPE SSTSRC SAID '/
      DATA IDST2  /'CLAT CLON SST1 IREL SOZA SAZA IRMS   SOLAZI OPTH '/
cvvvvv
c after replacing "IREL" with "SSTD" and "IRMS" with "SSTB", replace
c  the above line with:
cc    DATA IDST2  /'CLAT CLON SST1 SSTD SOZA SAZA SSTB   SOLAZI OPTH '/
c^^^^^
      DATA IDCHN  /'INCN ALBD TMBR '/

      DATA BMISS  /10E10/

      DIMENSION XDATA(18),XCHAN(3,5)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C           EXPLANATION OF DATA IDENTIFYING COMMENTS BELOW
C
C STORAGE               MNEM  F XX YYY /INT OF  DESCRIPTIVE TEXT
C LOCATION              ONIC           /FXXYYY 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      XDATA     = 10.E10
      K         = 0

C XDATA(1)              YEAR  0 04 001 /  1025  YEAR                    

      K        = K + 1
      XDATA(K) = IYR

C XDATA(2)              MNTH  0 04 002 /  1026  MONTH                   

      K         = K + 1
      MON       = ITEMP(4) ! CBUFTN(12)
      XDATA(K)  = MON

C XDATA(3)              DAYS  0 04 003 /  1027  DAY                     

      K         = K + 1
      IDAY      = ITEMP(9) ! CBUFTN(17)
      XDATA(K)  = IDAY

C XDATA(4)              HOUR  0 04 004 /  1028  HOUR                    

      K         = K + 1
      IHR       = ITEMP(10) ! CBUFTN(18)
      XDATA(K)  = IHR

C XDATA(5)              MINU  0 04 005 /  1029  MINUTE                  

      K         = K + 1
      MIN       = ITEMP(11) ! CBUFTN(19)
      XDATA(K)  = MIN

C XDATA(6)              SECO  0 04 006 /  1030  SECOND                  

      K         = K + 1
      ISEC      = ITEMP(12) ! CBUFTN(20)
      XDATA(K)  = ISEC

C  CHECK FOR REPORT WITHIN THE TRANSLATION WINDOW
C  ----------------------------------------------

      IF(IYR.LT.0 .OR. 
     .    MON.LT.1 .OR.  MON.GT.12   .OR.
     .   IDAY.LT.1 .OR. IDAY.GT.31   .OR.
     .    IHR.LT.0 .OR.  IHR.GT.24   .OR.
     .    MIN.LT.0 .OR.  MIN.GT.60   .OR.
     .   ISEC.LT.0 .OR. ISEC.GT.60)  THEN
        PRINT '("BAD DATE:",I4,3I2.2," SUBSET:",A8)',
     .         IYR,MON,IDAY,IHR,SUBSET
        IERR = 1
        RETURN
      ENDIF

C XDATA(7)            SSTYPE  0 06 193 /  1729  SST OBS TYPE            

      K         = K + 1
      XDATA(K)  = ITEMP(1) ! CBUFTN(9)

C XDATA(8)            SSTSRC  0 06 194 /  1730  SST OBS SOURCE          

      K         = K + 1
      XDATA(K)  = ITEMP(2) ! CBUFTN(10)

C XDATA(9)              SAID  0 01 007 /   263  SATELLITE ID        

      K         = K + 1
      XDATA(K)  = SATID

C XDATA(10)              CLAT  0 05 002 /  1282  LATITUDE               

      K         = K + 1
      LAT1      = ITEMP(5) ! CBUFTN(13)
      LAT2      = ITEMP(6) ! CBUFTN(14)
      LAT       = 256 * LAT1 + LAT2
      IF(LAT.GT.32767) THEN
        LAT = LAT - 65536
      ENDIF
      XDATA(K)  = FLOAT(LAT) * 0.01

C XDATA(11)             CLON  0 06 002 /  1538  LONGITUDE               

      K         = K + 1
      LON1      = ITEMP(7) ! CBUFTN(15)
      LON2      = ITEMP(8) ! CBUFTN(16)
      LON       = 256 * LON1 + LON2
      IF(LON.GT.32767) THEN
        LON = LON - 65536
      ENDIF
      XDATA(K)  = FLOAT(LON) * 0.01

C XDATA(12)             SST1  0 22 042 /  5674  SEA SURFACE TEMP        

      K         = K + 1
      ISST1     = ITEMP(13) ! CBUFTN(21)
      ISST2     = ITEMP(14) ! CBUFTN(22)
      ISST      = 256 * ISST1 + ISST2
      IF(ISST.GT.32767) THEN
        ISST = ISST - 65536
      ENDIF
      IF(ISST.NE.-3000) XDATA(K)  = FLOAT(ISST) * 0.1 + 273.2
ccc   write(6,'('' ISST1 = '',i4,'' ISST2 = '',i4,'' ISST = '',i6,
ccc  1          '' K = '',i3,'' XDATA = '',1pe10.3)')
ccc  2      isst1,isst2,isst,k,xdata(K)

C XDATA(13)             IREL  0 22 227 /  5859  RELIABILITY OF SST
C                                               RETRIEVAL
cvvvvv
c after replacing "IREL" with "SSTD" and "IRMS" with "SSTB", replace
c  the above 2 lines with:
C XDATA(13)             SSTD  0 22 050 /  5682  STANDARD DEVIATION OF
C                                               SST RETRIEVAL
C                        (ONLY FOR NAVOCEANO DATA, MISSING FOR NESDIS)
c^^^^^

      K         = K + 1
      IREL1     = ITEMP(15) ! CBUFTN(23)
      IREL2     = ITEMP(16) ! CBUFTN(24)
      IREL      = 256 * IREL1 + IREL2
      XDATA(K)  = FLOAT(IREL)
cvvvvv
c after replacing "IREL" with "SSTD" and "IRMS" with "SSTB", replace
c  the above 4 lines with:
cc    IF(TYPE.EQ.'NAVOCEANO') THEN
cc      ISTD1     = ITEMP(15) ! CBUFTN(23)
cc      ISTD2     = ITEMP(16) ! CBUFTN(24)
cc      ISTD      = 256 * ISTD1 + ISTD2
cc      XDATA(K)  = FLOAT(ISTD) * 0.01
cc    ELSE
cc      XDATA(K) = BMISS
cc    ENDIF
c^^^^^

C XDATA(14)             SOZA  0 07 025 /  1817  SOLAR ZENITH ANGLE 
 
      K         = K + 1
      ISZN1     = ITEMP(17) ! CBUFTN(25)
      ISZN2     = ITEMP(18) ! CBUFTN(26)
      ISZN      = 256 * ISZN1 + ISZN2
      XDATA(K)  = FLOAT(ISZN) * 0.1

C XDATA(15)             SAZA  0 07 024 /  1816  SATELLITE ZENITH ANGLE 

      K         = K + 1
      ISZN1     = ITEMP(19) ! CBUFTN(27)
      ISZN2     = ITEMP(20) ! CBUFTN(28)
      ISZN      = 256 * ISZN1 + ISZN2
      IF(ISZN.GT.32767) THEN
        ISZN = ISZN - 65536
      ENDIF
      IF(ISZN.NE.-3000) THEN
        XDATA(K) = FLOAT(ISZN) * 0.01
      ELSE
        XDATA(K) = BMISS
      ENDIF

C XDATA(16)             IRMS  0 22 228 /  5860  INTERNAL RMS ERROR OF
C                                               SST RETRIEVAL
cvvvvv
c after replacing "IREL" with "SSTD" and "IRMS" with "SSTB", replace
c  the above 2 lines with:
C XDATA(16)             SSTB  0 25 037 /  6437  SST RETRIEVAL BIAS
C                        (ONLY FOR NAVOCEANO DATA, MISSING FOR NESDIS)
c^^^^^

      K         = K + 1
      IRMS1     = ITEMP(21) ! CBUFTN(31)
      IRMS2     = ITEMP(22) ! CBUFTN(32)
      IRMS      = 256 * IRMS1 + IRMS2
      XDATA(K)  = FLOAT(IRMS) * 0.01
cvvvvv
c after replacing "IREL" with "SSTD" and "IRMS" with "SSTB", replace
c  the above 4 lines with:
cc    IF(TYPE.EQ.'NAVOCEANO') THEN
cc      IBIA1     = ITEMP(21) ! CBUFTN(31)
cc      IBIA2     = ITEMP(22) ! CBUFTN(32)
cc      IBIA      = 256 * IBIA1 + IBIA2
cc      IF(IBIA.GT.32767) THEN
cc        IBIA = IBIA - 65536
cc      ENDIF
cc      XDATA(K)  = FLOAT(IBIA) * 0.01
cc    ELSE
cc      XDATA(K) = BMISS
cc    ENDIF
c^^^^^

C XDATA(17)           SOLAZI  0 05 022 /  1302  SOLAR AZIMUTH ANGLE    

      K         = K + 1
      ISAZ1     = ITEMP(23) ! CBUFTN(33)
      ISAZ2     = ITEMP(24) ! CBUFTN(34)
      ISAZ      = 256 * ISAZ1 + ISAZ2
      IF(ISAZ.GT.32767) THEN
        ISAZ = ISAZ - 65536
      ENDIF
      IF(ISAZ.NE.-3000) THEN
        XDATA(K) = FLOAT(ISAZ) * 0.1
      ELSE
        XDATA(K) = BMISS
      ENDIF

C XDATA(18)             OPTH  0 15 193 /  4033  AEROSOL OPTICAL
C                                               THICKNESS
C                        (ONLY FOR NAVOCEANO DATA, MISSING FOR NESDIS)
      K         = K + 1
      IF(TYPE.EQ.'NAVOCEANO') THEN
        IAOT1     = ITEMP(37) ! CBUFTN(61)
        IAOT2     = ITEMP(38) ! CBUFTN(62)
        IAOT      = 256 * IAOT1 + IAOT2
        XDATA(K)  = FLOAT(IAOT) 
      ELSE
        XDATA(K) = BMISS
      ENDIF


C  LOOP THROUGH THE FIVE AVHRR CHANNELS
C   - CHANNELS 1-2 ALWAYS CONTAIN ALBEDO
C   - CHANNELS 3 CONTAINS BRIGHTNESS TEMPERATURE FOR P.M. SATELLITES
C   - CHANNELS 3 CONTAINS 50% ALBEDO (DAYTIME), 50% BRIGHTNESS
C                TEMPERATURE (NIGHTTIME) FOR A.M. SATELLITES
C   - CHANNELS 4-5 ALWAYS CONTAIN BRIGHTNESS TEMPERATURE
C  -----------------------------------------------------------------

      DO ICH = 1 , 5
        K         = 0

C XCHAN(1,ICH)          INCN  0 02 150 /   662  AVHRR/ATOVS CHANNEL NO.

        K         = K + 1
        XCHAN(K,ICH) = REAL(47+ICH)
        IBRT1     = ITEMP(23+2*ICH) ! CBUFTN(39,41,43,45,47)
        IBRT2     = ITEMP(24+2*ICH) ! CBUFTN(40,42,44,46,48)
        IBRT      = 256 * IBRT1 + IBRT2
        ALBBRT    = FLOAT(IBRT) * 0.01
        IF(ICH.LT.3) THEN

C XCHAN(2,ICH)          ALBD  0 14 027 /  2611  ALBEDO                 

          K       = K + 1
          XCHAN(K,ICH) = ALBBRT

C XCHAN(3,ICH)          TMBR  0 12 163 /  3235  BRIGHTNESS TEMPRERATURE

          K       = K + 1
          XCHAN(K,ICH) = BMISS 

        ELSE IF(ICH.EQ.3) THEN

          IF(ALBBRT.LT.101.) THEN

C XCHAN(2,ICH)          ALBD  0 14 027 /  2611  ALBEDO

            K       = K + 1
            XCHAN(K,ICH) = ALBBRT

C XCHAN(3,ICH)          TMBR  0 12 163 /  3235  BRIGHTNESS TEMPRERATURE

            K       = K + 1
            XCHAN(K,ICH) = BMISS
          ELSE

C XCHAN(2,ICH)          ALBD  0 14 027 /  2611  ALBEDO

            K       = K + 1
            XCHAN(K,ICH) = BMISS  
  
C XCHAN(3,ICH)          TMBR  0 12 163 /  3235  BRIGHTNESS TEMPRERATURE

            K       = K + 1
            XCHAN(K,ICH) = ALBBRT
          ENDIF

        ELSE

C XCHAN(2,ICH)          ALBD  0 14 027 /  2611  ALBEDO                 

          K       = K + 1
          XCHAN(K,ICH) = BMISS  
  
C XCHAN(3,ICH)          TMBR  0 12 163 /  3235  BRIGHTNESS TEMPRERATURE

          K       = K + 1
          XCHAN(K,ICH) = ALBBRT
        ENDIF
      ENDDO
C 
C-----------------------------------------------------------------------
C  
C  BUFR LAYOUT OF THE ARRAY CONTAINING THE SST/BTEMP/ALBEDO REPORT:
C
C XDATA(1)   =  YEAR    004001   YEAR                    
C XDATA(2)   =  MNTH    004002   MONTH                   
C XDATA(3)   =  DAYS    004003   DAY                     
C XDATA(4)   =  HOUR    004004   HOUR                    
C XDATA(5)   =  MINU    004005   MINUTE                  
C XDATA(6)   =  SECO    004006   SECOND                  
C XDATA(7)   =  SSTYPE  006193   SST OBS TYPE            
C XDATA(8)   =  SSTSRC  006194   SST OBS SOURCE          
C XDATA(9)   =  SAID    001007   SATELLITE ID
C XDATA(10)  =  CLAT    005002   LATITUDE                
C XDATA(11)  =  CLON    006002   LONGITUDE               
C XDATA(12)  =  SST1    022042   SEA SURFACE TEMP        
C XDATA(13)  =  IREL    022227   RELIABILITY OF SST RETRIEVAL
cvvvvv
c after replacing "IREL" with "SSTD" and "IRMS" with "SSTB", replace
c  above line with:
C XDATA(13)  =  SSTD    022050   STANDARD DEVIATION OF SST RETRIEVAL
c^^^^^
C XDATA(14)  =  SOZA    007025   SOLAR ZENITH ANGLE 
C XDATA(15)  =  SAZA    007024   SATELLITE ZENITH ANGLE 
C XDATA(16)  =  IRMS    022228   INTERNAL RMS ERROR OF SST RETRIEVAL
cvvvvv
c after replacing "IREL" with "SSTD" and "IRMS" with "SSTB", replace
c  above line with:
C XDATA(16)  =  SSTB    025037   SST RETRIEVAL BIAS
c^^^^^
C XDATA(17)  =  SOLAZI  005022   SOLAR AZIMUTH ANGLE    
C XDATA(18)  =  OPTH    015193   AEROSOL OPTICAL THICKNESS
C
C  where J = 1,5 (5 channels)
C XCHAN(1,J) =  INCN    002150   AVHRR/ATOVS CHANNEL NO.
C XCHAN(2,J) =  ALBD    014027   ALBEDO                 
C XCHAN(3,J) =  TMBR    012163   BRIGHTNESS TEMPRERATURE


C
C-----------------------------------------------------------------------

C  ENCODE DATA INTO BUFR
C  ---------------------

      CALL UFBINT(IUNT,XDATA(1),9,1,IRET,IDST1)
      CALL UFBINT(IUNT,XDATA(10),9,1,IRET,IDST2)
      CALL UFBREP(IUNT,XCHAN,3,5,IRET,IDCHN)

C  WRITE SUBSET TO BUFR MESSAGE
C  ----------------------------

      CALL WRITSB(IUNT)

      IERR = 0

      RETURN

      END

