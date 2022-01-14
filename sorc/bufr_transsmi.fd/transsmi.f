C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_TRANSSMI
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2014-01-20
C
C ABSTRACT: READS IN SSM/I SCANS FROM A NESDIS ORBIT-BY-ORBIT 
C   RADIANCE OR PRODUCTS FILE, REFORMATS AND PACKS INTO A BUFR FILE.
C
C PROGRAM HISTORY LOG:
C 1996-09-17  BERT B. KATZ 
C 1997-03-14  BERT B. KATZ  CHANGE LONGITUDE RANGE FROM 0-360 TO 
C                           -180-180.
C 1998-03-23  BERT B. KATZ  ADDED PRODUCTION OF A THIRD TANK HOLDING
C                           NEURAL NET PRODUCTS.
C 1998-03-25  BERT B. KATZ  DISABLED SUMMARY OF NEURAL NET PROCESSING
C                           WHEN PROCESSING STANDARD SSM/I PRODUCTS.
C 1998-09-02  BERT B. KATZ  ADDED HIGH-DENSITY 85 GHz BRIGHTNESS
C                           TEMPERATURE OUTPUT.
C 1998-12-01  BERT B. KATZ  CHANGES FOR Y2K AND FORTRAN 90 COMPLIANCE, 
C                           INCLUDING CHANGES TO FEED OUTPUT TO TRANJB 
C                           SCRIPT, RATHER THAN WRITE DIRECTLY TO TANKS.
C 1999-01-12  BERT B. KATZ  MADE VARIABLES IDAT1 AND IDAT2 INTO ARRAYS,
C                           AS PER USE IN SUBR. SSMIUNPK.
C 1999-02-10  BERT B. KATZ  REPLACED REFERENCE TO CLBUFR BY CALLS TO 
C                           CLOSBF. MOVED FILE OPENING TO MAIN ROUTINE.
C 1999-06-07  BERT B. KATZ  ADDED ERROR HANDLING WHEN NO OUTPUT IS 
C                           CREATED SO THAT SUBSEQUENT TRANJB'S ARE    
C                           SKIPPED.
C 2000-02-24  KEYSER  NOW ACCOUNTS FOR DMSP-15 SATELLITE (248
C                     IN BUFR CODE TABLE 0-01-007)
C 2000-06-15  KEYSER  CORRECTED SCALING FACTOR FOR RAINFALL RATE IN
C                     SUBR. SSMIUNPK - MUST MULTIPLY PACKED VALUE BY
C                     0.2 TO GET IT INTO MM/HR (BEFORE NO SCALING WAS
C                     ASSUMED)
C 2000-07-07  BERT B. KATZ  ADDED HIGH-RESOLUTION 85 GHz BRIGHTNESS
C                           TEMPERATURES TO HOLDING TANK.  ALSO ADDED
C                           CLOUD LIQUID WATER AND SEA SURFACE 
C                           TEMPERATURE TO NEURAL NET PRODUCTS HOLDING
C                           TANK.
C 2000-01-16  KEYSER  REMOVED UNNEEDED LOGIC PERTAINING TO IN-LINE
C                     NN3 CALCULATION AND TIME CHECKING; RENAMED
C                     SUBROUTINES; STREAMLINED
C 2002-02-11  KEYSER  DATE CHECK FOR SST FILE READ IN IS NOW +/- 7-DAYS
C                     FROM DATE OF FIRST SCAN READ IN (BEFORE IT RANGED
C                     FROM 9-DAYS PRIOR TO THE CURRENT WALLCLOCK TIME
C                     TO 7.5-DAYS AFTER AND WAS BASED ON THE OUTPUT
C                     TIME RANGE FROM W3TRNARG)
C 2004-09-12  KEYSER  NOW ENCODES TOTAL PRECIPITABLE WATER USING THE
C                     WMO MNEMONIC "TPWT" INSTEAD OF THE LOCAL MNEMONIC
C                     "PH2O", ENCODES SNOW DEPTH USING THE WMO MNEMONIC
C                     "TOSD" INSTEAD OF THE LOCAL MNEMONIC "SNDP",
C                     ENCODES OCEAN SURFACE WIND SPEED USING THE WMO
C                     MNEMONIC "WSPD" (WIND SPEED) INSTEAD OF THE LOCAL
C                     MNEMONIC "WSOS", AND ENCODES CLOUD WATER USING
C                     THE WMO MNEMONIC SEQUENCE "METFET VILWC METFET"
C                     {WHERE THE FIRST METFET IS SET TO 12 (CLOUD),
C                     VILWC IS VERT. INTEGR. LIQUID WATER CONTENT, AND
C                     THE SECOND METFET IS SET TO MISSING (CANCEL)}
C                     INSTEAD OF THE LOCAL MNEMONIC "CH2O"
C 2012-12-31  STOKES  MINOR CHANGES FOR WCOSS TRANSITION, INPUT SSM/I
C                     DATA AS STREAM RATHER THAN AS BLOCKED RECORDS
C                     SINCE UPSTEAM PROCESSING ON WCOSS NO LONGER
C                     BLOCKS INPUT FILE
C 2013-01-05  KEYSER  MINOR FINISHING TOUCHES ON TRANSITION TO WCOSS.
C                     ALSO NOW WRITES OUTPUT BUFR FILES WITHOUT THE 
C                     DICTIONARY TABLE AT THE TOP.
C 2014-01-20  KEYSER  MINOR CHANGES.
C                           
C USAGE:
C
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT. W3TRNARG PARSES ARGUMENTS FROM
C              - STANDARD INPUT.
C     UNIT 11  - NESDIS BINARY SSM/I FILE CONTAINING EITHER RADIANCE
C              - OR PRODUCT DATA. (STREAM)
C     UNIT 20  - BUFR TABLE FILE CONTAINING BUFR TABLES A, B, AND D.
C     UNIT 31  - GRIB FILE OF GLOBAL SEA SURFACE TEMPERATURE.
C     UNIT 32  - GRIB INDEX FILE OF GLOBAL SEA SURFACE TEMPERATURE.
C     UNIT 33  - FILE OF NESDIS LAND-SEA TAGS.
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 51  - OUTPUT BUFR FILE FOR RADIANCES OR NESDIS PRODUCTS. 
C              - TRANJB WILL PLACE THE BUFR MESSAGES INTO THE PROPER 
C              - TANKS.
C     UNIT 52  - OUTPUT BUFR FILE FOR NEURAL NET PRODUCTS. TRANJB WILL 
C              - PLACE THE BUFR MESSAGES INTO THE PROPER TANKS.
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:  - BRTRPT   PRDRPT   SSMIUNPK IN_LINE  NN3_DRVR LANDSEA
C              - READ_LS  READ_SST NN3_ALG
C     LIBRARY:
C       SYSTEM - GET_ENVIRONMENT_VARIABLE
C       W3NCO  - W3TRNARG W3MOVDAT W3DOXDAT W3UTCDAT W3DIFDAT W3AI39
C              - MOVA2I
C              - GBYTEC   GBYTESC  GETGB    W3TAGB   W3TAGE   ERREXIT
C     BUFRLIB  - OPENBF   CLOSBF   OPENMG   UFBINT   UFBREP   WRITSB
C              - GETBMISS
C       BACIO  - BAOPENR  BAREAD   BACLOSE
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =   1 - UNABLE TO PARSE INPUT ARGUMENTS IN W3TRNARG
C          =   6 - ENVIRONMENT VARIABLE RETURNED FROM
C                  GET_ENVIRONMENT_VARIABLE DOES NOT EXIST
C          =   7 - ENVIRONMENT VARIABLE RETURNED FROM
C                  GET_ENVIRONMENT_VARIABLE IS OF WRONG LENGTH
C          =   8 - NON-SPECIFIC ERROR(S) FROM GET_ENVIRONMENT_VARIABLE
C          =   9 - UNEXPECTED STATUS FROM GET_ENVIRONMENT_VARIABLE
C          = 253 - NO SCANS PROCESSED
C          = 254 - ERROR READING INPUT SSM/I DATA
C          = 255 - WRONG SSMI BUFR SUBCATEGORY          
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      PROGRAM BUFR_TRANSSMI

C-----------------------------------------------------------------------

      CHARACTER*12 SUBDIR,TANKID,SUBDIRP,TANKIDP
      CHARACTER*80 APPCHR,APPCHRP
      CHARACTER*8 TLFLAG,SUBSET,TLFLAGP,SUBSETP

      COMMON/MISCEE/DMAX,DMIN,LFLAG,LICEC,KNTSCN,LAERR,LOERR,NLR,NIR,
     1              LBTER(7)

      INTEGER IBUFTN(1036)
      INTEGER IBUF85(1548),IREC_BAD(15)
      DATA INUNIT /11/
      DATA INDX   /20/
      DATA IUNT   /51/
      DATA IUNP   /52/
C-----------------------------------------------------------------------
      CALL W3TAGB('BUFR_TRANSSMI',2014,0020,0050,'NP22') 
      PRINT *, ' '
      PRINT *, '  WELCOME TO BUFR_TRANSSMI - VERSION 01-20-2014'
      PRINT *, ' '
      CALL W3TRNARG(SUBDIR,LSUBDR,TANKID,LTNKID,APPCHR,LAPCHR,
     1              TLFLAG,JDATE,KDATE,IERR)
c.......................................................................
      IF(IERR.NE.0) THEN
        WRITE(6,'('' UNABLE TO PARSE ARGS TO TRANSLATION ROUTINE - '',
     1            '' RETURN CODE = '',I5)') IERR
        CALL W3TAGE('BUFR_TRANSSMI') 
        CALL ERREXIT(IERR)
      ENDIF
c.......................................................................
      SUBSET = 'NC'//SUBDIR(LSUBDR-2:LSUBDR)//TANKID(LTNKID-2:LTNKID)
      IF(SUBSET(6:8).EQ.'001') THEN
        ITSSMI = 2
ccccc   CALL OPENBF(IUNT,'OUT',INDX)
        CALL OPENBF(IUNT,'NODX',INDX)
        CALL W3TRNARG(SUBDIRP,LSUBDRP,TANKIDP,LTNKIDP,APPCHRP,LAPCHRP,
     1                TLFLAGP,JDATE,KDATE,IERR)
        IF(IERR.EQ.0) THEN
          SUBSETP = 'NC'//SUBDIRP(LSUBDRP-2:LSUBDRP)//
     1                    TANKIDP(LTNKIDP-2:LTNKIDP)
ccccc     CALL OPENBF(IUNP,'OUT',INDX)
          CALL OPENBF(IUNP,'NODX',INDX)
        ELSE
          SUBSETP = 'NONEURAL'
        ENDIF
      ELSE IF(SUBSET(6:8).EQ.'002') THEN
        ITSSMI = 1
ccccc   CALL OPENBF(IUNT,'OUT',INDX)
        CALL OPENBF(IUNT,'NODX',INDX)
        SUBSETP = 'NONEURAL'
      ELSE
c.......................................................................
        WRITE(6,1002) SUBSET(6:8)
 1002   FORMAT(1X,'''',A3,''' IS NOT AN ALLOWABLE SSMI SUBCATEGORY.')
        CALL W3TAGE('BUFR_TRANSSMI') 
        CALL ERREXIT(255)
c.......................................................................
      ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      KOUNT = 0
      IREC = 0
      IREC_BAD = 0
      INDEX = 0  ! ALWAYS
      DO  WHILE (INDEX.EQ.0)
         CALL SSMIUNPK(INUNIT,ITSSMI,IBUFTN,IBUF85,KOUNT,NRECN,IER)
         IF(IER.EQ.0 .AND. KOUNT.GT.0) THEN
            IF(IREC.EQ.0) THEN
               IDATE = IBUFTN(2)*1000000 + IBUFTN(3)*10000 +
     1                 IBUFTN(4)*100 + IBUFTN(5) 
               CALL OPENMG(IUNT,SUBSET,IDATE)
               IF(SUBSETP.NE.'NONEURAL') CALL OPENMG(IUNP,SUBSETP,IDATE)
            ENDIF

            IF(ITSSMI.EQ.2) THEN
               CALL BRTRPT(IUNT,IUNP,IBUFTN,IBUF85,SUBSET,IDATE,SUBSETP,
     1          IERR)
            ELSE IF(ITSSMI.EQ.1) THEN
               CALL PRDRPT(IUNT,IBUFTN,SUBSET,IERR)
            ENDIF
            IF(IERR.EQ.0) THEN
               IREC = IREC + 1
            ELSE
               IF(IERR.GT.0.AND.IERR.LE.15) THEN
                  IREC_BAD(IERR) = IREC_BAD(IERR) + 1
               ELSE
                  IREC_BAD(15) = IREC_BAD(15) + 1
               ENDIF
               IF(IERR.NE.1) IREC = IREC + 1
            END IF
         ELSE IF(KOUNT.LE.0) THEN
            EXIT ! ALL DONE
         ELSE IF(IER.NE.0) THEN
            IREC = IABS(KOUNT)
            GO TO 8888 ! PROBLEM
         ENDIF
      END DO
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C ALL SCAN HAVE BEEN READ AND PROCESSED - FINISHED
      IF(ITSSMI.EQ.2 .AND. SUBSETP.NE.'NONEURAL') THEN
         PRINT 124, KNTSCN
  124    FORMAT(/' BUFR_TRANSSMI: +++++ ALL VALID SCANS UNPACKED AND ',
     $    'RETURNED FROM THIS NESDIS BINARY SSM/I FILE'//34X,
     $    '** BUFR_TRANSSMI: SUMMARY **'//35X,'TOTAL NUMBER OF SCANS ',
     $    'COMPLETELY PROCESSED AND RETURNED',11X,I7)
         IF(IREC_BAD(1).GT.0) PRINT 801, IREC_BAD(1)
  801 FORMAT(46X,'# OF SCANS COMPLETELY SKIPPED DUE TO BAD DATE:    ',
     $ 5X,I7)
         IF(IREC_BAD(2).GT.0) PRINT 802, IREC_BAD(2)
  802 FORMAT(46X,'# OF SCANS W/ ALL NN3 MISSING, ERROR OPENING L/S ',
     $ 'FILE: ',I7)
         IF(IREC_BAD(3).GT.0) PRINT 803, IREC_BAD(3)
  803 FORMAT(46X,'# OF SCANS WITH ALL NN3 MISSING, SST NOT FOUND:   ',
     $ 5X,I7)
         IF(IREC_BAD(4).GT.0) PRINT 804, IREC_BAD(4)
  804 FORMAT(46X,'# OF SCANS WITH ALL NN3 MISSING, SST HAS BAD DATE:',
     $ 5X,I7)
         IF(IREC_BAD(5).GT.0) PRINT 805, IREC_BAD(5)
  805 FORMAT(46X,'# OF SCANS WITH ALL NN3 MISSING, SST FILE READ ',
     $ 'ERROR:  ',I7)
         IF(IREC_BAD(6).GT.0) PRINT 806, IREC_BAD(6)
  806 FORMAT(46X,'# OF SCANS WITH ALL NN3 MISSING, SST FILE READ ',
     $ 'ERROR:  ',I7)
         DO I = 7,15
            IF(IREC_BAD(I).GT.0) PRINT 815, I,IREC_BAD(I)
         ENDDO
  815 FORMAT(46X,'# OF SCANS WITH IERR ',I2,':                       ',
     $ '        ',I7)
         PRINT 324, LAERR,LOERR
  324    FORMAT(
     $   /35X,'NO. OF RETRIEVALS WITH LATITUDE OUT OF RANGE:       ',I7/
     $    35X,'NO. OF RETRIEVALS WITH LONGITUDE OUT OF RANGE:      ',I7)
         PRINT 780, LBTER,NLR,NIR
  780    FORMAT(
     $    35X,'NO. OF RETRIEVALS W/ ERROR IN 19 GHZ V BRIGHT. TEMP:',I7/
     $    35X,'NO. OF RETRIEVALS W/ ERROR IN 19 GHZ H BRIGHT. TEMP:',I7/
     $    35X,'NO. OF RETRIEVALS W/ ERROR IN 22 GHZ V BRIGHT. TEMP:',I7/
     $    35X,'NO. OF RETRIEVALS W/ ERROR IN 37 GHZ V BRIGHT. TEMP:',I7/
     $    35X,'NO. OF RETRIEVALS W/ ERROR IN 37 GHZ H BRIGHT. TEMP:',I7/
     $    35X,'NO. OF RETRIEVALS W/ ERROR IN 85 GHZ V BRIGHT. TEMP:',I7/
     $    35X,'NO. OF RETRIEVALS W/ ERROR IN 85 GHZ H BRIGHT. TEMP:',I7/
     $    35X,'NO. OF RETRIEVALS REJECTED DUE TO BEING OVER LAND:  ',I7/
     $    35X,'NO. OF RETRIEVALS REJECTED DUE TO BEING OVER ICE:   ',I7)
         PRINT 781, LFLAG,LICEC
  781    FORMAT(
     $    35X,'NO. OF NN3 RETR. REJECTED DUE TO FAILING RAIN FLAG: ',I7/
     $    35X,'NO. OF NN3 RETR. REJECTED DUE TO ICE CONTAMINATION: ',I7)
         PRINT 782, DMAX,DMIN
  782    FORMAT(/'  ** FOR SEA-SFC TEMP AT ALL RETRIEVAL LOCATIONS:',
     $    ' FIELD MAX =',F8.3,' DEG K, FIELD MIN =',F8.3,' DEG K'/)
      ENDIF
C.......................................................................
      CALL CLOSBF(IUNT)
      IF(ITSSMI.EQ.2 .AND. SUBSETP.NE.'NONEURAL') THEN
        CALL CLOSBF(IUNP)
      ENDIF
c.......................................................................
      IF(IREC.EQ.0) THEN
        WRITE(6,1003)
 1003   FORMAT(' NO SCANS PROCESSED -- DISABLING ALL SUBSEQUENT ',
     1         'PROCESSING.')
        CALL W3TAGE('BUFR_TRANSSMI')
        CALL ERREXIT(253)
      ENDIF
c.......................................................................
      PRINT *
      PRINT*,'PROCESSED (FULLY OR PATIALLY) ',IREC,' SCANS *** ',
     $ 'COMPLETED NORMALLY '
      CALL W3TAGE('BUFR_TRANSSMI') 
      STOP

c.......................................................................
 8888 CONTINUE
      WRITE(6,1001) IREC
 1001 FORMAT(' ERROR ENCOUNTERED READING INPUT NESDIS SSM/I DATA AFTER',
     1 ' RECORD NO. ',I8)
      CALL W3TAGE('BUFR_TRANSSMI') 
      CALL ERREXIT(254)
c.......................................................................

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BRTRPT      EXTRACT DATA FROM SINGLE SSM/I SCAN    
C   PRGMMR: D. A. KEYSER     ORG: NP22       DATE: 2004-09-12
C                                                                       
C ABSTRACT: EXTRACTS SSM/I BRIGHTNESS DATA ELEMENTS FROM ORBITAL SCANS,
C   AND ENCODES THEM INTO A BUFR MESSAGE WHICH IS WRITTEN TO A FILE.
C   ALSO PERFORMS NEURAL NET CALCULATIONS FOR WIND SPEED, TOTAL 
C   PRECIPITABLE WATER, CLOUD LIQUID WATER AND SEA-SURFACE TEMPERATURE.
C   ENCODES THEM INTO A BUFR MESSAGE, AND WRITES THEM OUT TO A SEPARATE
C   HOLDING FILE.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C 1996-09-03  KATZ                                                 
C 1998-03-23  KATZ    ADDED ABILITY TO PRODUCE HOLDING TANK      
C                     CONTAINING NEURAL NET PRODUCTS.
C 1998-12-01  KATZ    CHANGES FOR Y2K AND FORTRAN 90 COMPLIANCE, 
C                     INCLUDING CHANGES TO FEED OUTPUT TO TRANJB 
C                     SCRIPT, RATHER THAN WRITE DIRECTLY TO TANKS.
C 1999-02-10  KATZ    ADDED ARGUMENTS FOR OUTPUT BUFR FILES.
C                     MOVED FILE OPENING TO MAIN ROUTINE.
C 2000-02-23  KATZ    INCLUDED CALLS TO BAOPENR AND BACLOSE FOR 
C                     PORT TO IBM/SP.
C 2000-02-24  KEYSER  NOW ACCOUNTS FOR DMSP-15 SATELLITE (248
C                     IN BUFR CODE TABLE 0-01-007)
C 2000-07-07  KATZ    MODIFIED TO ADD HIGH RESOLUTION 85 GHz BRIGHTNESS
C                     TEMPERATURES TO HOLDING TANK.  ALSO ADDED CLOUD
C                     LIQUID WATER AND SEA SURFACE TEMPERATURE TO
C                     NEURAL NET PRODUCTS HOLDING TANK.
C 2004-09-12  KEYSER  NOW ENCODES TOTAL PRECIPITABLE WATER USING THE
C                     WMO MNEMONIC "TPWT" INSTEAD OF THE LOCAL MNEMONIC
C                     "PH2O", ENCODES SNOW DEPTH USING THE WMO MNEMONIC
C                     "TOSD" INSTEAD OF THE LOCAL MNEMONIC "SNDP",
C                     ENCODES OCEAN SURFACE WIND SPEED USING THE WMO
C                     MNEMONIC "WSPD" (WIND SPEED) INSTEAD OF THE LOCAL
C                     MNEMONIC "WSOS", AND ENCODES CLOUD WATER USING
C                     THE WMO MNEMONIC SEQUENCE "METFET VILWC METFET"
C                     {WHERE THE FIRST METFET IS SET TO 12 (CLOUD),
C                     VILWC IS VERT. INTEGR. LIQUID WATER CONTENT, AND
C                     THE SECOND METFET IS SET TO MISSING (CANCEL)}
C                     INSTEAD OF THE LOCAL MNEMONIC "CH2O"
C                                                                       
C USAGE:    CALL BRTRPT(IUNT,IUNP,ITEMP,ITM85,SUBSET,IDATE,SUBSETP,IERR)
C   INPUT ARGUMENT LIST:                                                
C     IUNT     - UNIT NUMBER FOR BUFR OUTPUT FILE OF BRIGHTNESS
C                TEMPERATURES.
C     IUNP     - UNIT NUMBER FOR BUFR OUTPUT FILE OF NEURAL NET 
C                PRODUCTS.
C     ITEMP    - SINGLE SSM/I SCAN OF BRIGHTNESS TEMPERATURE DATA,
C                CONTAINED IN 1036-MEMBER ARRAY.
C     ITM85    - SINGLE SSM/I SCAN OF HIGH-DENSITY 85GHz BRIGHTNESS
C                TEMPERATURE DATA, CONTAINED IN 1548-MEMBER ARRAY.
C     SUBSET   - 8 CHARACTER BUFR DATA TYPE.
C     IDATE    - DATE OF FIRST INPUT SCAN IN FORM YYYYMMDDHH.
C     SUBSETP  - 8 CHARACTER BUFR DATA TYPE FOR PRODUCTS.
C
C   OUTPUT ARGUMENT LIST:
C     IERR     - ERROR RETURN
C                 0 - SUCCESSFUL RETURN
C                 1 - BAD DATE ON A SCAN - DO NOT PROCESS SCAN
C                 2 - ERROR OPENING R. ACCESS FILE HOLDING LAND/SEA
C                     TAGS - DO NOT PROCESS NN3 PRODUCTS FOR ALL
C                     REPORTS IN THIS SCAN (SET TO MISSING)
C                 3 - SEA-SURFACE TEMPERATURE NOT FOUND IN GRIB INDEX
C                     FILE - DO NOT PROCESS NN3 PRODUCTS FOR ALL
C                     REPORTS IN THIS SCAN (SET TO MISSING)
C                 4 - SEA-SURFACE TEMPERATURE GRIB MESSAGE HAS A DATE
C                     THAT IS EITHER MORE THAN 7-DAYS PRIOR TO DATE OF
C                     FIRST SCAN READ OR MORE THAN 7-DAYS AFTER THE
C                     DATE OF THE FIRST SCAN READ - DO NOT PROCESS NN3
C                     PRODUCTS FOR ALL REPORTS IN THIS SCAN (SET TO
C                     MISSING)
C                 5 - BYTE-ADDRESSABLE READ ERROR FOR GRIB FILE
C                     CONTAINING SEA-SURFACE TEMPERATURE FIELD - DO NOT
C                     PROCESS NN3 PRODUCTS FOR ALL REPORTS IN THIS SCAN
C                     (SET TO MISSING)
C                 6 - ERROR RETURNED FROM GRIB DECODER GETGB FOR SEA-
C                     SURFACE TEMPERATURE FIELD - DO NOT PROCESS NN3
C                     PRODUCTS FOR ALL REPORTS IN THIS SCAN (SET TO
C                     MISSING)
C                                                                       
C   INPUT FILES:
C     UNIT 31  - GRIB FILE OF GLOBAL SEA SURFACE TEMPERATURE.
C     UNIT 32  - GRIB INDEX FILE OF GLOBAL SEA SURFACE TEMPERATURE.
C     UNIT 33  - FILE OF NESDIS LAND-SEA TAGS.
C
C   OUTPUT FILES:
C     UNIT 51  - POINTS TO THE BUFR RADIANCE OUTPUT FILE. TRANJB WILL 
C              - PLACE THE BUFR MESSAGES INTO THE PROPER TANKS.
C     UNIT 52  - POINTS TO THE BUFR NEURAL NET PRODUCTS OUTPUT FILE. 
C              - TRANJB WILL PLACE THE BUFR MESSAGES INTO THE PROPER 
C              - TANKS.
C
C REMARKS:                                                              
C                RADIANCE VALUES OF ZERO (0) ARE SET TO MISSING         
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  NCEP WCOSS
C                                                                       
C$$$                                                                    
      SUBROUTINE BRTRPT(IUNT,IUNP,ITEMP,ITM85,SUBSET,IDATE,SUBSETP,IERR)
      SAVE

      INTEGER       ITEMP(*),ITM85(*)

      CHARACTER*80  XIDST,XLCST,XL85ST,BRTST,BR85ST,PRDST
      CHARACTER*8   SUBSET,SUBSETP

      DIMENSION     XIDENT(9),XLOC(4,64),XLOC85(4,192),
     1              BRT(2,7,64),BRT85(2,2,192),PRD(4,64)
      REAL          XDATA(64,7),SWNN(64),TPWNN(64),LQWNN(64),SSTNN(64),
     1              METFET(128)
      INTEGER       IFLAG(64)

      COMMON/MISCCC/SSTDAT(360,180)
      COMMON/MISCEE/DMAX,DMIN,LFLAG,LICEC,KNTSCN,LAERR,LOERR,NLR,NIR,
     1              LBTER(7)

      DATA  INGBD/31/, INGBI/32/, INLSF/33/, ITIMES/0/
      DATA  XMSG/99999.0/,NPRALG/1/

      CHARACTER*6 envvar
      CHARACTER*120 fileb,filei

      DATA XIDST,XLCST,XL85ST,BRTST,BR85ST,PRDST
     ./
     .'SAID YEAR MNTH DAYS HOUR MINU SECO ORBN SCNN               ',
     .'CLAT CLON SFTG POSN                                        ',
     .'CLAT85 CLON85 SFTG85 POSN85                                ',
     .'CHNM TMBR                                                  ',
     .'CHNM85 TMBR85                                              ',
     .'WSPD TPWT VILWC SST1                                       '/

      BMISS = GETBMISS()

      IERR = 0
C
C                                ADD REPORT TO DATA ARRAY               
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C           EXPLANATION OF DATA IDENTIFYING COMMENTS BELOW
C
C STORAGE               MNEM  F XX YYY /INT OF  DESCRIPTIVE TEXT
C LOCATION              ONIC           /FXXYYY 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF(ITIMES.EQ.0)  THEN
         IF(NPRALG.GT.0 .AND. SUBSETP.NE.'NONEURAL') THEN

C-----------------------------------------------------------------------
C     IN-LINE CALCULATION OF WIND SPEED, TPW, CLOUD LIQUID WATER AND
C             SEA_SURFACE TEMPERATURE FROM NEURAL NET 3 ALG.
C  FIRST CALL TO THIS SUBROUTINE WILL READ IN SEA-SURFACE TEMPERATURE
C                    FIELD AS A CHECK FOR ICE LIMITS
C-----------------------------------------------------------------------

            DMAX = -99999.0
            DMIN =  99999.0
            envvar='FORT  '
            write(envvar(5:6),fmt='(I2)') INGBD
            call get_environment_variable(envvar,fileb,length,iestatus)
            select case(iestatus)
              case(0)
                continue
              case(1)
                print*,'environment variable ',trim(envvar),
     1                                             ' does not exist'
                CALL W3TAGE('BUFR_TRANSSMI') 
                call errexit(6)
              case(-1)
                print*,'env variable ',trim(envvar),
     1                        ' is set to string of',length,
     1                        ' characters which does not fit in fileb.'
                CALL W3TAGE('BUFR_TRANSSMI') 
                call errexit(7)
              case(3)
                print*,'non-specific error(s) from ',
     1                                  'GET_ENVIRONMENT_VARIABLE'
                CALL W3TAGE('BUFR_TRANSSMI') 
                call errexit(8)
              case default
                print*,'unexpected status from GET_ENVIRONMENT_VARIABLE'
                CALL W3TAGE('BUFR_TRANSSMI') 
                call errexit(9)
            end select

            envvar='FORT  '
            write(envvar(5:6),fmt='(I2)') INGBI
            call get_environment_variable(envvar,filei,length,iestatus)
            select case(iestatus)
              case(0)
                continue
              case(1)
                print*,'environment variable ',trim(envvar),
     1                                             ' does not exist'
                CALL W3TAGE('BUFR_TRANSSMI') 
                call errexit(6)
              case(-1)
                print*,'env variable ',trim(envvar),
     1                        ' is set to string of',length,
     1                        ' characters which does not fit in filei.'
                CALL W3TAGE('BUFR_TRANSSMI') 
                call errexit(7)
              case(3)
                print*,'non-specific error(s) from ',
     1                                  'GET_ENVIRONMENT_VARIABLE'
                CALL W3TAGE('BUFR_TRANSSMI') 
                call errexit(8)
              case default
                print*,'unexpected status from GET_ENVIRONMENT_VARIABLE'
                CALL W3TAGE('BUFR_TRANSSMI') 
                call errexit(9)
            end select

            CALL BAOPENR(INGBD,fileb,IRET1)
c            print *,'sagt: ',INGBD,fileb,IRET1
            CALL BAOPENR(INGBI,filei,IRET2)
c            print *,'sagt: ',INGBI,filei,IRET2
            CALL READ_SST(INGBI,INGBD,IDATE,*9993,*9994,*9995,*9996)
            CALL BACLOSE(INGBI,IRET)
            CALL BACLOSE(INGBD,IRET)
         END IF
         KNTSCN = 0
         LAERR = 0
         LOERR = 0
         NLR = 0
         NIR = 0
         LBTER = 0
         LFLAG = 0
         LICEC = 0
         ITIMES = 1
      END IF

C
C  CHECK FOR REPORT WITHIN THE TRANSLATION WINDOW
C  ----------------------------------------------

      IF(ITEMP(2).LT.0 .OR. 
     .   ITEMP(3).LT.1 .OR. ITEMP(3).GT.12 .OR.
     .   ITEMP(4).LT.1 .OR. ITEMP(4).GT.31 .OR.
     .   ITEMP(5).LT.0 .OR. ITEMP(5).GT.24 .OR.
     .   ITEMP(6).LT.0 .OR. ITEMP(6).GT.60 .OR.
     .   ITEMP(7).LT.0 .OR. ITEMP(7).GT.60) THEN
        PRINT '("BAD DATE:",I4,3I2.2," SUBSET:",A8)',
     .         ITEMP(2),ITEMP(3),ITEMP(4),ITEMP(5),SUBSET
        IERR = 1
        RETURN
      ENDIF

      XIDENT    = BMISS
      K         = 0
C
C XIDENT(1)             SAID  0 01 007 /   263  SATELLITE ID            
C
      K         = K + 1
      IF(ITEMP(1).EQ.7) THEN
        XIDENT(K) = 240
      ELSE IF(ITEMP(1).EQ.8) THEN
        XIDENT(K) = 241
      ELSE IF(ITEMP(1).EQ.9) THEN
        XIDENT(K) = 242
      ELSE IF(ITEMP(1).EQ.10) THEN
        XIDENT(K) = 243
      ELSE IF(ITEMP(1).EQ.11) THEN
        XIDENT(K) = 244
      ELSE IF(ITEMP(1).EQ.12) THEN
        XIDENT(K) = 245
      ELSE IF(ITEMP(1).EQ.13) THEN
        XIDENT(K) = 246
      ELSE IF(ITEMP(1).EQ.14) THEN
        XIDENT(K) = 247
      ELSE IF(ITEMP(1).EQ.15) THEN
        XIDENT(K) = 248
      ENDIF
C
C XIDENT(2)             YEAR  0 04 001 /  1025  YEAR                    
C
      K         = K + 1
      XIDENT(K) = ITEMP(2)
C
C XIDENT(3)             MNTH  0 04 002 /  1026  MONTH                   
C
      K         = K + 1
      IF(ITEMP(3).NE.99999) XIDENT(K) = ITEMP(3)
C
C XIDENT(4)             DAYS  0 04 003 /  1027  DAY                     
C
      K         = K + 1
      IF(ITEMP(4).NE.99999) XIDENT(K) = ITEMP(4)
C
C XIDENT(5)             HOUR  0 04 004 /  1028  HOUR                    
C
      K         = K + 1
      IF(ITEMP(5).NE.99999) XIDENT(K) = ITEMP(5)
C
C XIDENT(6)             MINU  0 04 005 /  1029  MINUTE                  
C
      K         = K + 1
      IF(ITEMP(6).NE.99999) XIDENT(K) = ITEMP(6)
C
C XIDENT(7)             SECO  0 04 006 /  1030  SECOND                  
C
      K         = K + 1
      IF(ITEMP(7).NE.99999) XIDENT(K) = ITEMP(7)
C
C XIDENT(8)             ORBN  0 05 040 /  1320  ORBIT NUMBER            
C
      K         = K + 1
      IF(ITEMP(9).NE.99999) XIDENT(K) = ITEMP(9)
C
C XIDENT(9)             SCNN  0 05 201 /  1481  SCAN NUMBER            
C
      K         = K + 1
      IF(ITEMP(8).NE.99999) XIDENT(K) = ITEMP(8)
C
      XLOC      = BMISS
      BRT       = BMISS
C
      DO 2000 ISCAN = 1 , 64
        IBSCAN    = 12 + (ISCAN - 1) * 16
        K         = 0
C
C XLOC(1)               CLAT  0 05 002 /  1282  LATITUDE                
C
        K         = K + 1
        IF(ITEMP(IBSCAN+1).NE.99999) XLOC(K,ISCAN) =
     1                               FLOAT(ITEMP(IBSCAN+1)) * 0.01
C
C XLOC(2)               CLON  0 06 002 /  1538  LONGITUDE               
C
        K         = K + 1
        IF(ITEMP(IBSCAN+2).NE.99999) THEN
          IF(ITEMP(IBSCAN+2).GT.18000) THEN
            XLOC(K,ISCAN) = FLOAT(ITEMP(IBSCAN+2) - 36000) * 0.01
          ELSE
            XLOC(K,ISCAN) = FLOAT(ITEMP(IBSCAN+2)        ) * 0.01
          ENDIF
        ENDIF
C
C XLOC(3)               SFTG  0 20 217 /  5337  SURFACE TAG             
C
        K         = K + 1
        IF(ITEMP(IBSCAN+10).NE.99999) XLOC(K,ISCAN) = ITEMP(IBSCAN+10)
C
C XLOC(4)               POSN  0 05 202 /  1482  POSITION NUMBER         
C
        K         = K + 1
        IF(ITEMP(IBSCAN+11).NE.99999) XLOC(K,ISCAN) = ITEMP(IBSCAN+11)
C
C
        DO 1000 J = 1, 7
C
          K           = 0
C
C BRT(1,J)              CHNM  0 05 042 /  1322  CHANNEL NUMBER          
C
          K           = K + 1
          BRT(K,J,ISCAN) = J
C
C BRT(2,J)              TMBR  0 12 063 /  3135  RADIANCE TEMPERATURE    
C
          K           = K + 1
          IF(ITEMP(IBSCAN+J+2).NE.99999) BRT(K,J,ISCAN) =
     1                                   FLOAT(ITEMP(IBSCAN+J+2)) * 0.01
C
 1000   CONTINUE
 2000 CONTINUE
C
      XLOC85    = BMISS
      BRT85     = BMISS
C
      DO 5000 ISCAN = 1 , 192
        IBSCAN    = 12 + (ISCAN - 1) * 8
        K         = 0
C
C XLOC(1)               CLAT  0 05 002 /  1282  LATITUDE                
C
        K         = K + 1
        IF(ITM85(IBSCAN+1).NE.99999) XLOC85(K,ISCAN) =
     1                               FLOAT(ITM85(IBSCAN+1)) * 0.01
C
C XLOC(2)               CLON  0 06 002 /  1538  LONGITUDE               
C
        K         = K + 1
        IF(ITM85(IBSCAN+2).NE.99999) THEN
          IF(ITM85(IBSCAN+2).GT.18000) THEN
            XLOC85(K,ISCAN) = FLOAT(ITM85(IBSCAN+2) - 36000) * 0.01
          ELSE
            XLOC85(K,ISCAN) = FLOAT(ITM85(IBSCAN+2)        ) * 0.01
          ENDIF
        ENDIF
C
C XLOC(3)               SFTG  0 20 217 /  5337  SURFACE TAG             
C
        K         = K + 1
        IF(ITM85(IBSCAN+10).NE.99999) XLOC85(K,ISCAN) = ITM85(IBSCAN+5)
C
C XLOC(4)               POSN  0 05 202 /  1482  POSITION NUMBER         
C
        K         = K + 1
        IF(ITM85(IBSCAN+11).NE.99999) XLOC85(K,ISCAN) = ITM85(IBSCAN+6)
C
C
        DO 4000 J = 1, 2
C
          K           = 0
C
C BRT(1,J)              CHNM  0 05 042 /  1322  CHANNEL NUMBER          
C
          K           = K + 1
          BRT85(K,J,ISCAN) = J
C
C BRT(2,J)              TMBR  0 12 063 /  3135  RADIANCE TEMPERATURE    
C
          K           = K + 1
          IF(ITM85(IBSCAN+J+2).NE.99999) BRT85(K,J,ISCAN) =
     1                                   FLOAT(ITM85(IBSCAN+J+2)) * 0.01
C
 4000   CONTINUE
 5000 CONTINUE
C  
C-----------------------------------------------------------------------
C  SUBROUTINE BRTRPT WILL WRITE SSMI BRIGHTNESS TEMPERATURES INTO
C  THE BUFR DATABASE. LAYOUT OF THE ARRAYS CONTAINING THE
C  SSMI REPORT IS AS FOLLOWS:
C-----------------------------------------------------------------------
C
C  THE LIST OF SSMI DATA AS CONVERTED AND WRITTEN TO BUFR FOLLOWS:
C
C     ARRAY            MNEMONIC   DESCRIPTOR   DESCRIPTION
C     -----            --------   ----------   -----------
C
C     XIDENT(1)        SAID       001007       SATELLITE ID
C     XIDENT(2)        YEAR       004001       YEAR
C     XIDENT(3)        MNTH       004002       MONTH
C     XIDENT(4)        DAYS       004003       DAY
C     XIDENT(5)        HOUR       004004       HOUR
C     XIDENT(6)        MINU       004005       MINUTE
C     XIDENT(7)        SECO       004006       SECOND
C     XIDENT(8)        ORBN       005040       ORBIT NUMBER
C     XIDENT(9)        SCNN       005201       SCAN NUMBER
C
C     XLOC(1,1:64)     CLAT       005002       LATITUDE
C     XLOC(2,1:64)     CLON       006002       LONGITUDE
C     XLOC(3,1:64)     SFTG       020217       SURFACE TAG
C     XLOC(4,1:64)     POSN       005202       POSITION NUMBER
C
C     BRT(1,1:448)     CHNM       005042       CHANNEL NUMBER
C     BRT(2,1:448)     TMBR       012063       RADIANCE TEMPERATURE
C
C     XLOC85(1,1:192)  CLAT85     005203       LATITUDE
C     XLOC85(2,1:192)  CLON85     006203       LONGITUDE
C     XLOC85(3,1:192)  SFTG85     020???       SURFACE TAG
C     XLOC85(4,1:192)  POSN85     005204       POSITION NUMBER
C
C     BRT85(1,1:384)   CHNM85     005205       CHANNEL NUMBER
C     BRT85(2,1:384)   TMBR85     012???       RADIANCE TEMPERATURE
C
C-----------------------------------------------------------------------

      IF(SUBSETP.EQ.'NONEURAL') GO TO 20000
      IORBN = NINT(XIDENT(8))
      ISCAN = NINT(XIDENT(9))
      DO IRT = 1,64
C THIS ROUTINE EXPECTS LONGITUDE TO BE 0-360 E; BUFR NOW RETURNS -180-0
C  FOR WEST AND 0-180 FOR EAST
         IFLAG(IRT) = 0
         XLON = XLOC(2,IRT)
         XLAT = XLOC(1,IRT)
         IF(XLON.LT.0.0)  XLON = XLON + 360.
C-----------------------------------------------------------------------
C             LOOP THROUGH THE 64 RETRIEVALS IN A SCAN
C-----------------------------------------------------------------------
C STORE THE LATITUDE (*100 DEGREES; + : NORTH, - : SOUTH)
         IF(XLAT.LT.-90.00 .OR. XLAT.GT.90.00)  THEN

C.......................................................................
C BAD LATITUDE
            LAERR = LAERR + 1
            PRINT 2777, IRT,ISCAN,IORBN,XLAT
 2777       FORMAT(' ##BRTRPT: BAD LAT: RETR.',I3,', SCAN',I6,
     $       ', ORBIT',I8,'; INPUT LAT=',F9.2,' - ALL DATA IN THIS ',
     $       'RETRIEVAL SET TO MISSING')
            IFLAG(IRT) = 1
C.......................................................................

         END IF
C STORE THE LONGITUDE (*100 DEGREES EAST)
         IF(XLON.LT.0.0 .OR. XLON.GT.360.00)  THEN

C.......................................................................
C BAD LONGITUDE
            LOERR = LOERR + 1
            PRINT 2778, IRT,ISCAN,IORBN,NINT(XLON*100.)
 2778       FORMAT(' ##BRTRPT: BAD LON: RETR.',I3,', SCAN',I6,
     $       ', ORBIT',I8,'; INPUT LON=',I7,' - ALL DATA IN THIS ',
     $       'RETRIEVAL SET TO MISSING')
            IFLAG(IRT) = 1
C.......................................................................

         END IF

C-----------------------------------------------------------------------
C             LOOP THROUGH THE 64 RETRIEVALS IN A SCAN
C-----------------------------------------------------------------------
         IF(IFLAG(IRT).NE.0)  GO TO 6000
C STORE THE 7 BRIGHTNESS TEMPS (*100 DEGREES KELVIN)
C  -- CHANNELS ARE IN THIS ORDER FOR A PARTICULAR RETRIEVAL:
C  19 GHZ V, 19 GHZ H, 22 GHZ V, 37 GHZ V, 37 GHZ H, 85 GHZ V, 85 GHZ H
         IGOOD = 0
         DO LCH = 1 , 7
            ICHNN = NINT(BRT(1,LCH,IRT))
            IF(ICHNN.LE.7)  THEN
               IF(BRT(2,LCH,IRT).LT.XMSG)  THEN
                  IGOOD = 1
               END IF
            END IF
         END DO

         IF(NPRALG.GT.0)  THEN
            DO IT = 1,7
               XDATA(IRT,IT) = XMSG
            END DO
            IF(IGOOD.EQ.1)  THEN
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COME HERE FOR IN-LINE CALC. OF WIND SPEED, TOT. PREC. WATER,
C  CLOUD LIQ. WATER AND SST VIA NEURAL NET 3 ALG.
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C GET LAND/SEA TAG AND CHECK FOR LAT/LON OVER LAND OR ICE
               BALON=XLON
C              IALON = MOD(36000-IBUFTN((27*IRT)-16),36000)
C              IX = 361. - REAL(IALON)/100.
               IX = 1. + XLON
               JY = 91 - NINT(XLAT + 0.50)
               DMIN = AMIN1(DMIN,SSTDAT(IX,JY))
               DMAX = AMAX1(DMAX,SSTDAT(IX,JY))
               CALL LANDSEA(INLSF,XLAT,BALON,LSTAG,*9997)

C      ..... REJECT IF OVER LAND (USE LAND/SEA TAG HERE)
               IF(LSTAG.NE.0)  THEN
                  NLR = NLR + 1
                  IFLAG(IRT) = 1
                  GO TO 6000
               END IF

C      ..... REJECT IF OVER ICE (USE SEA-SURFACE TEMPERATURE HERE)
               IF(SSTDAT(IX,JY).LE.272.96)  THEN
                  NIR = NIR + 1
                  IFLAG(IRT) = 1
                  GO TO 6000
               END IF

               DO IT = 1,7
                  XDATA(IRT,IT)=BRT(2,IT,IRT)
                  IF((IT.NE.2.AND.XDATA(IRT,IT).LT.100.00).OR.
     $               (IT.EQ.2.AND.XDATA(IRT,IT).LT. 80.00))  THEN
                     LBTER(IT) = LBTER(IT) + 1
                     PRINT 2779,IT,ISCAN,IORBN,(XDATA(IRT,JT),JT=1,7)
 2779 FORMAT(' ##BRTRPT: BT, CHN',I2,' BAD: SCAN',I6,', ORBIT',
     $ I8,'; BT:',7F7.2,'-CANNOT CALC. PRODS VIA ALG.')
                     IFLAG(IRT) = 1
                     GO TO 6000
                  END IF
               END DO
            ELSE

C......................................................................
C PROBLEM - CAN'T CALCULATE PRODUCTS VIA ANY ALG., ALL B.TEMPS MISSING
               PRINT 2879, ISCAN,IORBN,(XDATA(IRT,IT),IT=1,7)
 2879          FORMAT(' ##BRTRPT: ALL B.TMPS MSSNG: SCAN',I6,',',
     $          ' ORBIT',I8,'; BT:',7F7.2,'-CANNOT CALC PRODS VIA ALG.')
C......................................................................

               IFLAG(IRT) = 1
            END IF
         END IF
 6000    CONTINUE
      END DO
C CALL SUBR. IN_LINE TO INITIATE IN-LINE PRODUCT CALCULATION
      IF(NPRALG.GT.0) THEN

         CALL IN_LINE(XDATA,IFLAG,SWNN,TPWNN,LQWNN,SSTNN,64)

         DO IRT = 1,64
            IF(IFLAG(IRT).EQ.0) THEN
               PRD(1,IRT) = SWNN(IRT)
               PRD(2,IRT) = TPWNN(IRT)
               PRD(3,IRT) = LQWNN(IRT)
               METFET((IRT*2)-1) = 12    ! cloud
               METFET((IRT*2)-0) = BMISS ! cancel
               PRD(4,IRT) = SSTNN(IRT) + 273.15
            ELSE
               PRD(1,IRT) = BMISS
               PRD(2,IRT) = BMISS
               PRD(3,IRT) = BMISS
               METFET((IRT*2)-1) = BMISS
               METFET((IRT*2)-0) = BMISS
               PRD(4,IRT) = BMISS
            ENDIF

CDAK        IF(MOD(KNTSCN,100).EQ.0)  PRINT 2721, ATXT(1),
CDAK         SWNN(IRT),TPWNN(IRT),CLWNN(IRT),SSTNN(IRT),XDATA(IRT,1),
CDAK         (XDATA(IRT,KKK),KKK=3,5),(XDATA(IRT,4)-XDATA(IRT,5))
C2721 FORMAT(' BRTRPT: ',A2,' SPD',F6.1,'  TPW',F6.1,'  CLW',
CDAK $ F6.1,'  SST',F6.1/'  TB19V',F6.1,'  TB22V',F6.1,'  TB37V',F6.1,
CDAK $ '  TB37H',F6.1,'  TD37',F5.1)
         END DO
c        write(6,9876) (prd(3,irt),irt=1,64)
c9876    format(' in prdrpt, ch2o = '/(1x,1p7e10.3))
c        write(6,9875) (prd(4,irt),irt=1,64)
c9875    format(' in prdrpt, sst = '/(1x,1p7e10.3))

      ELSE

         DO IRT=1,64
            PRD(1,IRT) = BMISS
            PRD(2,IRT) = BMISS
            PRD(3,IRT) = BMISS
            METFET((IRT*2)-1) = BMISS
            METFET((IRT*2)-0) = BMISS
            PRD(4,IRT) = BMISS
         END DO

C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END IF
C-----------------------------------------------------------------------
      KNTSCN = KNTSCN + 1
      IERR = 0

      GO TO 20000
C.......................................................................
 9993 CONTINUE
C PROBLEM: SEA-SURFACE TEMPERATURE NOT FOUND IN GRIB INDEX FILE -- SET
C  IERR = 3 AND JUMP TO BUFR MESSAGE CREATION WITH PRODUCTS SET TO
C  MISSING FOR ALL REPORTS IN THIS SCAN
      PRINT 2908, INGBI
 2908 FORMAT(/' ##BRTRPT: SEA-SURFACE TEMPERATURE NOT FOUND IN GRIB ',
     $ 'INDEX FILE IN UNIT ',I2,' - IERR = 3'/)
      IERR = 3
      GO TO 10000
C.......................................................................
 9994 CONTINUE
C PROBLEM: SEA-SURFACE TEMPERATURE GRIB MESSAGE HAS A DATE THAT IS
C  EITHER MORE THAN 7-DAYS PRIOR TO DATE OF FIRST SCAN READ OR MORE
C  THAN 7-DAYS AFTER THE DATE OF THE FIRST SCAN READ -- SET IERR = 4
C  AND JUMP TO BUFR MESSAGE CREATION WITH PRODUCTS SET TO MISSING FOR
C  ALL REPORTS IN THIS SCAN
      PRINT 2909
 2909 FORMAT(/' ##BRTRPT: SST GRIB MSG HAS DATE WHICH IS EITHER 7-DAYS',
     $ ' PRIOR TO'/14X,'OR 7-DAYS LATER THAN DATE OF FIRST SCAN READ -',
     $ ' IERR = 4'/)
      IERR = 4
      GO TO 10000
C.......................................................................
 9995 CONTINUE
C PROBLEM: BYTE-ADDRESSABLE READ ERROR FOR GRIB FILE CONTAINING SEA-
C  SURFACE TEMPERATURE FIELD -- SET IERR = 5 AND JUMP TO BUFR MESSAGE
C  CREATION WITH PRODUCTS SET TO MISSING FOR ALL REPORTS IN THIS SCAN
      PRINT 2910
 2910 FORMAT(/' ##BRTRPT: BYTE-ADDRESSABLE READ ERROR FOR GRIB FILE ',
     $ 'CONTAINING SEA-SURFACE TEMPERATURE FIELD - IERR = 5'/)
      IERR = 5
      GO TO 10000
C.......................................................................
 9996 CONTINUE
C PROBLEM: ERROR RETURNED FROM GRIB DECODER - GETGB - FOR SEA-SURFACE
C  TEMPERATURE FIELD -- SET IERR = 6 AND JUMP TO BUFR MESSAGE CREATION
C  WITH PRODUCTS SET TO MISSING FOR ALL REPORTS IN THIS SCAN
      PRINT 2911
 2911 FORMAT(/' ##BRTRPT: ERROR RETURNED FROM GRIB DECODER GETGB, ',
     $ 'ENCODE PRODUCTS AS MISSING IN BUFR - IERR = 6'/)
      IERR = 6
      GO TO 10000
C.......................................................................
 9997 CONTINUE
C PROBLEM: ERROR OPENING R. ACCESS FILE HOLDING LAND/SEA TAGS -- SET
C  IERR = 2 AND JUMP TO BUFR MESSAGE CREATION WITH PRODUCTS SET TO
C  MISSING FOR ALL REPORTS IN THIS SCAN
      PRINT 2912, INLSF
 2912 FORMAT(' ##BRTRPT: ERROR OPENING R. ACCESS LAND/SEA FILE IN ',
     $ 'UNIT ',I2,' - ENCODE PRODUCTS AS MISSING IN BUFR - IERR = 2'/)
      IERR = 2
C.......................................................................
10000 CONTINUE

      DO IRT=1,64
         PRD(1,IRT) = BMISS
         PRD(2,IRT) = BMISS
         PRD(3,IRT) = BMISS
         METFET((IRT*2)-1) = BMISS
         METFET((IRT*2)-0) = BMISS
         PRD(4,IRT) = BMISS
      END DO
C.......................................................................
20000 CONTINUE

C  GET THE WRITE UNIT AND WRITE A SUBSET
C  -------------------------------------

      CALL UFBINT(IUNT,XIDENT,9,  1,IRET, XIDST)
      CALL UFBINT(IUNT,XLOC  ,4, 64,IRET, XLCST)
      CALL UFBINT(IUNT,BRT   ,2, 64,IRET, BRTST)
      CALL UFBREP(IUNT,BRT   ,2,448,IRET, BRTST)
      CALL UFBINT(IUNT,XLOC85,4,192,IRET,XL85ST)
      CALL UFBINT(IUNT,BRT85 ,2,192,IRET,BR85ST)
      CALL UFBREP(IUNT,BRT85 ,2,384,IRET,BR85ST)
      CALL WRITSB(IUNT)

      IF(SUBSETP.NE.'NONEURAL') THEN
         CALL UFBINT(IUNP,XIDENT,9,  1,IRET,XIDST)
         CALL UFBINT(IUNP,XLOC  ,4, 64,IRET,XLCST)
         CALL UFBINT(IUNP,PRD   ,4, 64,IRET,PRDST)
         CALL UFBREP(IUNP,METFET,1,128,IRET,'METFET')
         CALL WRITSB(IUNP)
      ENDIF

      IF(IERR.EQ.0) IERR = 0

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    IN_LINE     PREPARES FOR IN-LINE CALCULATION OF PRODS
C   PRGMMR: BERT B. KATZ     ORG: NP2        DATE: 2000-07-07
C
C ABSTRACT: BASED ON INPUT 7-CHANNEL SSM/I BRIGHTNESS TEMPERATURES.
C   CALCULATES THE WIND SPEED, TPW, CLOUD LIQ. WATER AND SST PRODUCTS
C   FOR THE NEURAL NET 3 ALGORITHM.
C
C PROGRAM HISTORY LOG:
C   ??-??-??  W. GEMMILL (W/NMC21) -- ORIGINAL AUTHOR
C 1995-01-04  D. A. KEYSER -- INCORPORATED INTO W3MISCAN AND
C       STREAMLINED CODE
C 1996-05-07  D. A. KEYSER (NP22) -- IN-LINE NEURAL NETWORK 1 ALGORITM
C       REPLACED BY NEURAL NETWORK 2 ALGORITHM
C 1996-07-30  D. A. KEYSER (NP22) -- CAN NOW PROCESS WIND SPEED FROM
C       BOTH ALGORITHMS IF DESIRED
C 1998-01-28  D. A. KEYSER (NP22) -- REPLACED NEURAL NET 2 ALGORITHM
C       WHICH CALCULATED ONLY WIND SPEED PRODUCT WITH NEURAL NET 3
C       ALGORITHM WHICH CALCULATES BOTH WIND SPEED AND TOTAL
C       PRECIPITABLE WATER PRODUCTS (AMONG OTHERS) BUT, UNLIKE NN2,
C       DOES NOT RETURN A RAIN FLAG VALUE (IT DOES SET ALL RETRIEVALS
C       TO MISSING THAT FAIL RAIN FLAG AND ICE CONTAMINATION TESTS)
C 1998-03-02  BERT B. KATZ (NP2) -- MODIFIED TO PRODUCE ONLY NEURAL
C       NET 3 WIND AND TPW PRODUCTS TO INCLUDE IN SSM/I BRIGHTNESS
C       TEMPERATURE TANK. VECTORIZED CODE.
C 1998-12-01  BERT B. KATZ (NP2) -- UPDATED DOCBLOCK.
C 1999-01-12  BERT B. KATZ (NP2) -- CORRECTED SPECIFICATION OF ARRAYS
C       IFLAG (CHANGED FROM REAL TO INTEGER) AND LQWNN (CHANGED FROM
C       INTEGER TO REAL).
C 2000-07-07  BERT B. KATZ (NP2) -- ADDED CLOUD LIQUID WATER AND SST 
C       TO NEURAL NET OUTPUT.
C
C USAGE:    CALL IN_LINE(BTAA,IFLAG,SWNN,TPWNN,LQWNN,SSTNN,NVECT)
C   INPUT ARGUMENT LIST:
C     BTAA     - 7-WORD ARRAY CONTAINING 7 CHANNELS OF BRIGHTNESS
C              - TEMPERATURE (KELVIN)
C     IFLAG    - ARRAY CONTAINING INFORMATION ABOUT WHICH POINTS
C                ARE TO BE EXCLUDED FROM CALCULATION
C     NVECT    - NUMBERT OF POINTS FOR WHICH PRODUCTS ARE TO BE
C                CALCULATED
C
C   OUTPUT ARGUMENT LIST:
C     SWNN     - CALCULATED WIND SPEED BASED ON NEURAL NET 3 ALGORITHM
C              - (METERS/SECOND)
C     TPWNN    - CALCULATED TOTAL COLUMN PRECIPITABLE WATER BASED ON
C              - NEURAL NET 3 ALGORITHM (MILLIMETERS)
C     LQWNN    - CALCULATED TOTAL COLUMN CLOUD LIQUID WATER BASED ON
C              - NEURAL NET 3 ALGORITHM (MILLIMETERS)
C     SSTNN    - CALCULATED SEA SURFACE TEMPERATURE BASED ON
C              - NEURAL NET 3 ALGORITHM (DEG. C)
C     IFLAG    - ARRAY CONTAINING INFORMATION ABOUT WHICH POINTS
C                ARE TO BE EXCLUDED FROM CALCULATION
C
C REMARKS: CALLED BY SUBROUTINE BRTRPT.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE IN_LINE(BTAA,IFLAG,SWNN,TPWNN,LQWNN,SSTNN,NVECT)
      REAL  BTAA(NVECT,7),SWNN(NVECT),TPWNN(NVECT),LQWNN(NVECT)
      REAL  SSTNN(NVECT)
      INTEGER IFLAG(NVECT),JERR(NVECT)

      COMMON/MISCEE/DMAX,DMIN,LFLAG,LICEC

      SAVE

      SWNN  = 99999.
      TPWNN = 99999.
      SSTNN = 99999.
      LQWNN = 99999.
      JERR = 0

C COMPUTE WIND SPEED, TPW, CLW & SST FROM NEURAL NET 3 ALGORITHM (1997)

      DO I=1,NVECT
        IF(IFLAG(I).NE.0) JERR(I) = 10
      ENDDO
      CALL NN3_DRVR(BTAA,SWNN,TPWNN,LQWNN,SSTNN,JERR,NVECT)
      DO I=1,NVECT
        IF(JERR(I).EQ.1)  THEN
           LFLAG = LFLAG + 1
           IFLAG(I) = 11
        ELSE IF(JERR(I).EQ.2)  THEN
           LICEC = LICEC + 1
           IFLAG(I) = 12
        ENDIF
      ENDDO
c     write(6,9876) (lqwnn(i),i=1,nvect)
c9876 format(' in in_line, ch2o = '/(1x,1p7e10.3))
c     write(6,9875) (sstnn(i),i=1,nvect)
c9875 format(' in in_line, sst = '/(1x,1p7e10.3))

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    LANDSEA     RETURNS LAND/SEA TAG FOR GIVEN LAT/LON
C   PRGMMR: D. A. KEYSER     ORG: NP22       DATE: 95-01-04
C
C ABSTRACT: FINDS AND RETURNS THE LOW RESOLUTION LAND/SEA TAG NEAREST
C   TO THE REQUESTED LATITUDE AND LONGITUDE.
C
C PROGRAM HISTORY LOG:
C   78-01-20  J. K. KALINOWSKI (S11213) -- ORIGINAL AUTHOR
C   78-10-03  J. K. KALINOWSKI (S1214) -- CHANGES UNKNOWN
C   85-03-01  N. DIGIROLAMO (SSAI) -- CONVERSION TO VS FORTRAN
C   95-01-04  D. A. KEYSER -- INCORPORATED INTO W3MISCAN AND
C       STREAMLINED CODE
C   98-03-01  BERT B. KATZ -- MODIFIED TO USE NEW VERSION OF READ_LS:
c       READS ENTIRE LAND/SEA TAG FILE JUST ONCE.
C   98-12-05  BERT B. KATZ -- UPDATED DOCBLOCK.
C   99-01-12  BERT B. KATZ -- CORRECTED INITIALIZATION OF IFLAG SO
C       THAT LAND/SEA TAGS ARE ACTUALLY READ IN.
C
C USAGE:    CALL LANDSEA(INLSF,BLAT,BLNG,LSTAG,*)
C   INPUT ARGUMENT LIST:
C     INLSF    - UNIT NUMBER OF DIRECT ACCESS NESDIS LAND/SEA FILE
C     BLAT     - LATITUDE (WHOLE DEGREES: RANGE IS 0. TO +90. NORTH,
C              - 0. TO -90. SOUTH)
C     BLNG     - LONGITUDE (WHOLE DEGREES: RANGE IS 0. TO +179.99 EAST,
C              - 0. TO -180. WEST)
C
C   OUTPUT ARGUMENT LIST:
C     LSTAG    - LAND/SEA TAG {=0 - SEA; =1 - LAND; =2 - COASTAL
C              - INTERFACE (HIGHER RESOLUTION TAGS ARE AVAILABLE);
C              - =3 - COASTAL INTERFACE (NO HIGHER RESOLUTION TAGS
C              - EXIST)}
C
C REMARKS: CALLED BY SUBROUTINES SSMIUNPK, BRTRPT.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE LANDSEA(INLSF,BLAT,BLNG,LSTAG,*)
      CHARACTER*1  LPUT
      REAL  RGS(3)
C LPUT CONTAINS A REGION OF LAND/SEA TAGS(RETURNED FROM CALL TO READ_LS)
      COMMON/MISCDD/LPUT(21960,3)

      SAVE

C RGS IS ARRAY HOLDING SOUTHERN BOUNDARIES OF EACH LAND/SEA TAG REGION
      DATA  RGS/-85.,-30.,25./,IFLAG/1/
C INITIALIZE LAND/SEA TAG AS 1 (OVER LAND)
      LSTAG = 1
C FIND NEAREST POINT OF A HALF-DEGREE (LAT,LONG) GRID
C ..ALAT IS LATITUDE TO THE NEAREST HALF-DEGREE
      ALAT = INT((BLAT+SIGN(.25,BLAT))/.5) * .5
C ..ALNG IS LONGITUDE TO THE NEAREST HALF-DEGREE
      ALNG = INT((BLNG+SIGN(.25,BLNG))/.5) * .5
      IF(NINT(ALNG*10.).EQ.1800)  ALNG = -180.
C IDENTIFY DATABASE REGION IN WHICH TO FIND CORRECT TAG
      NUMRGN = 1
      IF(IABS(NINT(ALAT*10)).GT.850)  THEN
         RETURN
      ELSE  IF(NINT(ALAT*10).GT.275)  THEN
         NUMRGN = 3
      ELSE  IF(NINT(ALAT*10.).GE.-275)  THEN
         NUMRGN = 2
      END IF
      IF(IFLAG.EQ.1)  THEN
         CALL READ_LS(INLSF,*98,*99)
         IFLAG = 0
      END IF
C FIND THE BYTE & BIT PAIR W/I DATA BASE REGION CONTAINING DESIRED TAG
      TRM1  = ((ALAT - RGS(NUMRGN)) * 1440.) + 360.
      LSTPT = TRM1 + (2. * ALNG)
C ..NBYTE IS THE BYTE IN LPUT CONTAINING THE TAG
      NBYTE = (180 * 8) + (LSTPT/4 * 8)
      NSHFT = (2 * (MOD(LSTPT,4) + 1)) - 2
C PULL OUT THE TAG
      CALL GBYTEC(LPUT(1,NUMRGN),LSTAG,NBYTE+NSHFT,2)
      RETURN
   98 CONTINUE
C  COME HERE IF LAND/SEA TAG FILE COULD NOT BE OPENED
      IFLAG = 1
      RETURN 1
C-----------------------------------------------------------------------
   99 CONTINUE
C COME HERE IF LAND/SEA TAG COULD NOT BE RETURNED
C  (IN THIS CASE IT WILL REMAIN SET TO 1 INDICATING OVER LAND)
      IFLAG = 1
      RETURN
C-----------------------------------------------------------------------
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    READ_LS     READS 6 RECORDS FROM LAND/SEA TAG DTABASE
C   PRGMMR: BERT B. KATZ     ORG: NP22       DATE: 1978-01-20
C
C ABSTRACT: OPENS FILE AND READS SIX RECORDS FROM A LOW RESOLUTION 
C   LAND/SEA DATABASE AND STORES INTO COMMON.
C
C PROGRAM HISTORY LOG:
C   78-01-20  J. K. KALINOWSKI (S11213) -- ORIGINAL AUTHOR
C   95-01-04  D. A. KEYSER -- INCORPORATED INTO W3MISCAN AND
C       STREAMLINED CODE; MODIFIED TO BE MACHINE INDEPENDENT THRU
C       USE OF STANDARD FORTRAN DIRECT ACCESS READ
C   98-03-01  BERT B. KATZ -- MODIFIED FOR USE BY SSM/I TANKING
C       PROCEDURES.  SAVES ALL REGIONS OF LAND/SEA TAG INFORMATION 
C       INTO EXPANDED ARRAY.
C   98-12-01  BERT B. KATZ -- UPDATED DOCBLOCK.
C
C USAGE:    CALL READ_LS(INLSF,*,*)
C   INPUT ARGUMENT LIST:
C     INLSF    - UNIT NUMBER OF DIRECT ACCESS NESDIS LAND/SEA FILE
C
C   INPUT FILES:
C     UNIT INLSF - DIRECT ACCESS NESDIS LAND/SEA FILE
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C
C REMARKS: CALLED BY SUBROUTNE LANDSEA.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE READ_LS(INLSF,*,*)
      CHARACTER*1  LPUT

C LPUT CONTAINS A REGION OF LAND/SEA TAGS (COMPRISED OF 2 RECORDS FROM
C  LAND/SEA FILE) -- 180 BYTES OF DOCUMENTATION FOLLOWED BY 21780 BYTES
C  OF LAND/SEA TAGS

      COMMON/MISCDD/LPUT(21960,3)

      OPEN(UNIT=INLSF,ACCESS='DIRECT',IOSTAT=IERR,RECL=10980)
      IF(IERR.EQ.0) THEN
         PRINT 67, INLSF
   67    FORMAT(//4X,'** READ_LS: OPEN R. ACCESS NESDIS LAND/SEA',
     $    ' FILE IN UNIT ',I2/)
      ELSE
         PRINT 2000, INLSF,IERR
 2000    FORMAT(' ##READ_LS: ERROR OPENING R. ACCESS NESDIS LAND/SEA ',
     $    'FILE IN UNIT ',I2,' - IOSTAT = ',I5)
         RETURN 1
      ENDIF
      NREC=1
      READ(INLSF,REC=NREC,ERR=10)  (LPUT(II,1),II=1,10980)
      NREC=NREC+1
      READ(INLSF,REC=NREC,ERR=10)  (LPUT(II,1),II=10981,21960)
      NREC=NREC+1
      READ(INLSF,REC=NREC,ERR=10)  (LPUT(II,2),II=1,10980)
      NREC=NREC+1
      READ(INLSF,REC=NREC,ERR=10)  (LPUT(II,2),II=10981,21960)
      NREC=NREC+1
      READ(INLSF,REC=NREC,ERR=10)  (LPUT(II,3),II=1,10980)
      NREC=NREC+1
      READ(INLSF,REC=NREC,ERR=10)  (LPUT(II,3),II=10981,21960)
      RETURN
C-----------------------------------------------------------------------
   10 CONTINUE
C ERROR READING IN A RECORD FROM LAND-SEA FILE -- RETURN (TAG WILL BE
C  SET TO 1 MEANING OVER LAND IN THIS CASE)
      PRINT 1000, NREC,INLSF
 1000 FORMAT(' ##READ_LS: ERROR READING IN LAND-SEA DATA RECORD',
     $ I7,' IN UNIT ',I2,' -- SET TAG TO LAND'/)
      RETURN 2
C-----------------------------------------------------------------------
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    READ_SST    READS IN NH AND SH 1-DEG. SEA-SFC TEMPS.
C   PRGMMR: D. A. KEYSER     ORG: NP22       DATE: 2002-02-11
C
C ABSTRACT: READS IN GLOBAL SEA-SURFACE TEMPERATURE FIELD ON A ONE-
C   DEGREE GRID FROM GRIB FILE.
C
C PROGRAM HISTORY LOG:
C   ??-??-??  W. GEMMILL (NP21) -- ORIGINAL AUTHOR
C   95-01-04  D. A. KEYSER -- INCORPORATED INTO W3MISCAN AND
C       STREAMLINED CODE; CONVERTED SST INPUT FILE FROM VSAM/ON84 TO
C       GRIB TO ALLOW CODE COMPILE AND RUN ON THE CRAY MACHINES.
C   98-12-01  BERT KATZ (NP2) -- REMOVED SUBS W3FS21 AND W3FS22 AND
C       REPLACED BY W3DIFDAT FOR Y2K AND FORTRAN 90 COMPLIANCE,
C 2002-02-11  KEYSER  DATE CHECK FOR SST FILE READ IN IS NOW +/- 7-DAYS
C       FROM DATE OF FIRST SCAN READ IN (BEFORE IT RANGED FROM 9-DAYS
C       PRIOR TO THE CURRENT WALLCLOCK TIME TO 7.5-DAYS AFTER AND WAS
C       BASED ON THE OUTPUT TIME RANGE FROM W3TRNARG)
C     
C
C USAGE:    CALL READ_SST(INGBI,INGBD,IDATE,*,*,*,*)
C   INPUT ARGUMENT LIST:
C     INGBI    - UNIT NUMBER OF GRIB INDEX FILE FOR GRIB FILE
C              - CONTAINING GLOBAL 1-DEGREE SEA-SURFACE TEMP FIELD
C     INGBD    - UNIT NUMBER OF GRIB FILE CONTAINING GLOBAL 1-DEGREE
C              - SEA-SURFACE TEMP FIELD
C     IDATE    - DATE OF FIRST INPUT SCAN IN FORM YYYYMMDDHH.
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C
C REMARKS: CALLED BY SUBROUTINES SSMIUNPK, BRTRPT.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE READ_SST(INGBI,INGBD,IDATE,*,*,*,*)
      PARAMETER (MAXPTS=360*180)
      LOGICAL*1  LBMS(360,180)
      INTEGER  KPDS(200),KGDS(200)
      INTEGER  LPDS(200),LGDS(200)
      INTEGER  IDAT(5),JDAT(8),MDATE(8)
      REAL     TDIFF(5)
      COMMON/MISCCC/SSTDAT(360,180)

      SAVE

      KPDS = -1
      KGDS = -1
      N = -1
      KPDS(5)  = 11
      KPDS(6)  =  1
      KPDS(7)  =  0
      KPDS(8)  = -1
      KPDS(9)  = -1
      KPDS(10) = -1
      CALL GETGB(INGBD,INGBI,MAXPTS,0,KPDS,KGDS,KF,K,LPDS,LGDS,
     &           LBMS,SSTDAT,IRET)
C.......................................................................
C ABNORMAL RETURN IF SST NOT FOUND IN GRIB INDEX FILE
      IF(IRET.NE.0)  THEN
        WRITE(6,*)' ERROR READING SST USING GETGB.  IRET = ',IRET
        IF (IRET.EQ.96) RETURN 1
        IF (IRET.EQ.97) RETURN 3
        IF (IRET.EQ.98) RETURN 3
        IF (IRET.EQ.99) RETURN 3
        RETURN 4
      ENDIF
C.......................................................................
C READ SUCCESSFUL
      IDAT(1) = IDATE / 1000000
      IDAT(2) = MOD(IDATE,1000000) / 10000
      IDAT(3) = MOD(IDATE,10000) / 100
      IDAT(4) = MOD(IDATE,100)
      IDAT(5) = 0
      JDAT(1:3) = IDAT(1:3)
      JDAT(4)   = 0
      JDAT(5:6) = IDAT(4:5)
      JDAT(7:8) = 0
      MDATE(1)   = 100 * (LPDS(21) - 1) + LPDS(8)
      MDATE(2:3) = LPDS(9:10)
      MDATE(4)   = 0
      MDATE(5:6) = LPDS(11:12)
      MDATE(7:8) = 0
      CALL W3DIFDAT(MDATE,JDAT,2,TDIFF)
      IF(TDIFF(2).LT.-168.0.OR.TDIFF(2).GT.168.0)  THEN
C.......................................................................
C COME HERE IF SST GRIB MSG HAS A DATE THAT IS EITHER MORE THAN 7-DAYS
C  PRIOR TO THE DATE OF THE FIRST SCAN READ (INPUT ARG. "IDATE") OR
C  MORE THAN 7-DAYS AFTER THE DATE OF THE FIRST SCAN READ
         PRINT 27, (MDATE(I),I=1,3),(MDATE(I),I=4,5)
   27    FORMAT(/' ##READ_SST: SST GRIB MSG HAS DATE:',I5,4I3,' AS A ',
     $    'RESULT......')
         RETURN 2
C.......................................................................
      END IF
      PRINT 60, (MDATE(I),I=1,3),(MDATE(I),I=4,5)
   60 FORMAT(/4X,'** READ_SST: SEA-SFC TEMP SUCCESSFULLY READ IN FROM ',
     $ 'GRIB FILE, DATE IS: ',I5,4I3/)

      RETURN
C.......................................................................
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    NN3_ALG     CALC. SSM/I PRODS FROM NEURAL NET 3 ALG.
C   PRGMMR: V. KRASNOPOLSKY  ORG: NP20       DATE: 1996-07-15
C
C ABSTRACT: THIS NN CALCULATES W (IN M/S), V (IN MM), L (IN MM), AND
C   SST (IN DEG C).  THIS NN WAS TRAINED ON BLENDED F11 DATA SET
C   (SSMI/BUOY MATCHUPS PLUS SSMI/OWS MATCHUPS 15 KM X 15 MIN) UNDER
C   CLEAR + CLOUDY CONDITIONS.
C
C PROGRAM HISTORY LOG:
C   96-07-15  V. KRASNOPOLSKY -- ORIGINAL AUTHOR
C   98-03-12  BERT KATZ -- VECTORIZED VERSION OF CODE.
C   98-12-01  BERT KATZ -- UPDATED DOCBLOCK.
C
C USAGE:    CALL NN3_ALG(X,JERR,Y,NVECT)
C   INPUT ARGUMENT LIST:
C     X        - 5-WORD ARRAY CONTAINING BRIGHTNESS TEMPERATURE IN THE
C              - ORDER: T19V (WORD 1), T19H (WORD 2), T22V (WORD 3),
C              - T37V (WORD 4), T37H (WORD 5) (ALL IN KELVIN)
C     JERR     - CONTAINS NONZERO FLAG FOR FILLING RESULTS WITH 
C              - MISSIMG VALUES (99999.)
C
C   OUTPUT ARGUMENT LIST:
C     Y        - 4-WORD ARRAY CONTAINING CALCULATED PRODUCTS IN THE
C              - ORDER: WIND SPEED (M/S) (WORD 1), COLUMNAR WATER
C              - VAPOR (TOTAL PRECIP. WATER) (MM) (WORD 2),  COLUMNAR
C              - LIQUID WATER (MM) (WORD 3), SEA SURFACE TEMPERATURE
C              - (DEG. C) (WORD 4)
C
C REMARKS: CALLED BY SUBROUTINE NN3_DRVR.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE NN3_ALG(X,JERR,Y,NVECT)
      INTEGER  HID,OUT

C IN IS THE NUMBER OF NN INPUTS, HID IS THE NUMBER OF HIDDEN NODES,
C  OUT IS THE NUMBER OF OUTPUTS

      PARAMETER (IN =5, HID =12, OUT =4)
      DIMENSION  X(NVECT,IN),Y(NVECT,OUT),W1(IN,HID),W2(HID,OUT),
     $ B1(HID),B2(OUT),X2(NVECT,HID),X3(NVECT,OUT),A(OUT),B(OUT)
      INTEGER JERR(NVECT)

C W1 HOLDS INPUT WEIGHTS

      DATA ((W1(I,J),J = 1,HID),I = 1,IN)/
     $-0.0435901, 0.0614709,-0.0453639,-0.0161106,-0.0271382, 0.0229015,
     $-0.0650678, 0.0704302, 0.0383939, 0.0773921, 0.0661954,-0.0643473,
     $-0.0108528,-0.0283174,-0.0308437,-0.0199316,-0.0131226, 0.0107767,
     $ 0.0234265,-0.0291637, 0.0140943, .00567931,-.00931768,
     $-.00860661, 0.0159747,-0.0749903,-0.0503523, 0.0524172, 0.0195771,
     $ 0.0302056, 0.0331725, 0.0326714,-0.0291429, 0.0180438, 0.0281923,
     $-0.0269554, 0.102836,  0.0591511, 0.134313, -0.0109854,-0.0786303,
     $ 0.0117111, 0.0231543,-0.0205603,-0.0382944,-0.0342049,
     $ 0.00052407,0.110301, -0.0404777, 0.0428816, 0.0878070, 0.0168326,
     $ 0.0196183, 0.0293995, 0.00954805,-.00716287,0.0269475,
     $-0.0418217,-0.0165812, 0.0291809/

C W2 HOLDS HIDDEN WEIGHTS

      DATA ((W2(I,J),J = 1,OUT),I = 1,HID)/
     $-0.827004, -0.169961,-0.230296, -0.311201, -0.243296, 0.00454425,
     $ 0.950679,  1.09296,  0.0842604, 0.0140775, 1.80508, -0.198263,
     $-0.0678487, 0.428192, 0.827626,  0.253772,  0.112026, 0.00563793,
     $-1.28161,  -0.169509, 0.0019085,-0.137136, -0.334738, 0.224899,
     $-0.189678,  0.626459,-0.204658, -0.885417, -0.148720, 0.122903,
     $ 0.650024,  0.715758, 0.735026, -0.123308, -0.387411,-0.140137,
     $ 0.229058,  0.244314,-1.08613,  -0.294565, -0.192568, 0.608760,
     $-0.753586,  0.897605, 0.0322991,-0.178470,  0.0807701,
     $-0.781417/

C B1 HOLDS HIDDEN BIASES

      DATA (B1(I), I=1,HID)/
     $ -9.92116,-10.3103,-17.2536,  -5.26287, 17.7729,-20.4812,
     $ -4.80869,-11.5222,  0.592880,-4.89773,-17.3294, -7.74136/

C B2 HOLDS OUTPUT BIAS

      DATA (B2(I), I=1,OUT)/-0.882873,-0.0120802,-3.19400,1.00314/

C A(OUT), B(OUT) HOLD TRANSFORMATION COEFFICIENTS

      DATA (A(I), I=1,OUT)/18.1286,31.8210,0.198863,37.1250/
      DATA (B(I), I=1,OUT)/13.7100,32.0980,0.198863,-5.82500/

C START NEURAL NETWORK

C  - INITIALIZE X2

      DO I = 1,HID
         DO L = 1 , NVECT
            X2(L,I) = B1(I)
         END DO
         DO J = 1,IN
            DO L = 1 , NVECT
               X2(L,I) = X2(L,I) + (X(L,J) * W1(J,I))
            END DO
         END DO
         DO L = 1 , NVECT
            X2(L,I) = TANH(X2(L,I))
         END DO
      END DO

C - INITIALIZE X3

      DO K = 1,OUT
         DO L = 1 , NVECT
            X3(L,K) = B2(K)
         END DO
         DO J = 1,HID
            DO L = 1 , NVECT
               X3(L,K) = X3(L,K) + (W2(J,K) * X2(L,J))
            END DO
         END DO

C --- CALCULATE Y

         DO L = 1 , NVECT
            IF(JERR(L).NE.0) THEN
               Y(L,K) = 99999.
            ELSE

C --------- Remove negative values of SPN, SST, V --------------------

               Y(L,K) = MAX((A(K) * TANH(X3(L,K))) + B(K),0.0)
            END IF
         END DO
      END DO
c     write(6,9876) (y(l,3),l=1,nvect)
c9876 format(' in nn3_alg, ch2o = '/(1x,1p7e10.3))
c     write(6,9875) (y(l,4),l=1,nvect)
c9875 format(' in nn3_alg, sst = '/(1x,1p7e10.3))

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    PRDRPT      EXTRACT DATA FROM SINGLE SSM/I SCAN    
C   PRGMMR: D. A. KEYSER     ORG: NP22       DATE: 2004-09-12
C                                                                       
C ABSTRACT: EXTRACTS SSM/I DERIVED PRODUCT DATA FROM ORBITAL SCANS,   
C   PLACES INTO ARRAY FOR INCLUSION INTO A BUFR MESSAGE.     
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C 1996-09-03  KATZ                                                 
C 1997-03-14  KATZ    CHANGE LONGITUDE RANGE FROM 0-360 TO -180-180.
C 1997-05-13  KATZ    ADD SATID FOR DMSP-14.
C 1998-12-01  KATZ    CHANGES FOR Y2K AND FORTRAN 90 COMPLIANCE, 
C                     INCLUDING CHANGES TO FEED OUTPUT TO TRANJB 
C                     SCRIPT, RATHER THAN WRITE DIRECTLY TO TANKS.
C 1999-02-10  KATZ    ADDED ARGUMENT FOR OUTPUT BUFR FILE.
C                     MOVED FILE OPENING TO MAIN ROUTINE.
C 2000-02-24  KEYSER  NOW ACCOUNTS FOR DMSP-15 SATELLITE (248
C                     IN BUFR CODE TABLE 0-01-007)
C 2004-09-12  KEYSER  NOW ENCODES TOTAL PRECIPITABLE WATER USING THE
C                     WMO MNEMONIC "TPWT" INSTEAD OF THE LOCAL MNEMONIC
C                     "PH2O", ENCODES SNOW DEPTH USING THE WMO MNEMONIC
C                     "TOSD" INSTEAD OF THE LOCAL MNEMONIC "SNDP",
C                     ENCODES OCEAN SURFACE WIND SPEED USING THE WMO
C                     MNEMONIC "WSPD" (WIND SPEED) INSTEAD OF THE LOCAL
C                     MNEMONIC "WSOS", AND ENCODES CLOUD WATER USING
C                     THE WMO MNEMONIC SEQUENCE "METFET VILWC METFET"
C                     {WHERE THE FIRST METFET IS SET TO 12 (CLOUD),
C                     VILWC IS VERT. INTEGR. LIQUID WATER CONTENT, AND
C                     THE SECOND METFET IS SET TO MISSING (CANCEL)}
C                     INSTEAD OF THE LOCAL MNEMONIC "CH2O"
C                                                                       
C USAGE:    CALL PRDRPT(IUNT,ITEMP,SUBSET,IERR)
C   INPUT ARGUMENT LIST:                                                
C     IUNT     - UNIT NUMBER FOR OUTPUT BUFR FILE OF NESDIS PRODUCTS.
C     ITEMP    - SINGLE SSM/I SCAN OF DERIVED PRODUCT DATA,
C                CONTAINED IN 1036-MEMBER ARRAY.
C     SUBSET   - 8 CHARACTER BUFR DATA TYPE.
C
C   OUTPUT ARGUMENT LIST:
C     IERR     - ERROR RETURN. NON-ZERO IF DATA FAILS THE
C                 0 - SUCCESSFUL RETURN
C                 1 - BAD DATE ON A SCAN - DO NOT PROCESS SCAN
C                                                                       
C   OUTPUT FILES:
C     UNIT 51  - POINTS TO THE OUTPUT BUFR FILE. TRANJB WILL PLACE THE 
C                BUFR MESSAGES INTO THE PROPER TANKS.
C
C REMARKS:                                                              
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  NCEP WCOSS
C                                                                       
C$$$                                                                    
      SUBROUTINE PRDRPT(IUNT,ITEMP,SUBSET,IERR)
      INTEGER       ITEMP(*)

      DIMENSION     XIDENT(9),XLOC(4,64),PRDUCT(12,64)
      REAL          METFET(128)

      CHARACTER*80  XIDST,XLCST,PRDST
      CHARACTER*8   SUBSET

      DATA XIDST,XLCST,PRDST
     ./
     .'SAID YEAR MNTH DAYS HOUR MINU SECO ORBN SCNN                ',
     .'CLAT CLON SFTG POSN                                         ',
     .'VILWC REQV WSPD SMOI ICON ICAG ICED TPWT TMSK TOSD RFLG SFTP'
     ./

      BMISS = GETBMISS()
C                                                                       
C                                ADD REPORT TO DATA ARRAY               
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C           EXPLANATION OF DATA IDENTIFYING COMMENTS BELOW
C
C STORAGE               MNEM  F XX YYY /INT OF  DESCRIPTIVE TEXT
C LOCATION              ONIC           /FXXYYY 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  CHECK FOR REPORT WITHIN THE TRANSLATION WINDOW
C  ----------------------------------------------

      IF(ITEMP(2).LT.0 .OR. 
     .   ITEMP(3).LT.1 .OR. ITEMP(3).GT.12 .OR.
     .   ITEMP(4).LT.1 .OR. ITEMP(4).GT.31 .OR.
     .   ITEMP(5).LT.0 .OR. ITEMP(5).GT.24 .OR.
     .   ITEMP(6).LT.0 .OR. ITEMP(6).GT.60 .OR.
     .   ITEMP(7).LT.0 .OR. ITEMP(7).GT.60) THEN
        PRINT '("BAD DATE:",I4,3I2.2," SUBSET:",A8)',
     .         ITEMP(2),ITEMP(3),ITEMP(4),ITEMP(5),SUBSET
        IERR = 1
        RETURN
      ENDIF

      XIDENT    = BMISS
      K         = 0
C
C XIDENT(1)             SAID  0 01 007 /   263  SATELLITE ID            
C
      K         = K + 1
      IF(ITEMP(1).EQ.7) THEN
        XIDENT(K) = 240
      ELSE IF(ITEMP(1).EQ.8) THEN
        XIDENT(K) = 241
      ELSE IF(ITEMP(1).EQ.9) THEN
        XIDENT(K) = 242
      ELSE IF(ITEMP(1).EQ.10) THEN
        XIDENT(K) = 243
      ELSE IF(ITEMP(1).EQ.11) THEN
        XIDENT(K) = 244
      ELSE IF(ITEMP(1).EQ.12) THEN
        XIDENT(K) = 245
      ELSE IF(ITEMP(1).EQ.13) THEN
        XIDENT(K) = 246
      ELSE IF(ITEMP(1).EQ.14) THEN
        XIDENT(K) = 247
      ELSE IF(ITEMP(1).EQ.15) THEN
        XIDENT(K) = 248
      ENDIF
C
C XIDENT(2)             YEAR  0 04 001 /  1025  YEAR                    
C
      K         = K + 1
      XIDENT(K) = ITEMP(2)
C
C XIDENT(3)             MNTH  0 04 002 /  1026  MONTH                   
C
      K         = K + 1
      IF(ITEMP(3).NE.99999) XIDENT(K) = ITEMP(3)
C
C XIDENT(4)             DAYS  0 04 003 /  1027  DAY                     
C
      K         = K + 1
      IF(ITEMP(4).NE.99999) XIDENT(K) = ITEMP(4)
C
C XIDENT(5)             HOUR  0 04 004 /  1028  HOUR                    
C
      K         = K + 1
      IF(ITEMP(5).NE.99999) XIDENT(K) = ITEMP(5)
C
C XIDENT(6)             MINU  0 04 005 /  1029  MINUTE                  
C
      K         = K + 1
      IF(ITEMP(6).NE.99999) XIDENT(K) = ITEMP(6)
C
C XIDENT(7)             SECO  0 04 006 /  1030  SECOND                  
C
      K         = K + 1
      IF(ITEMP(7).NE.99999) XIDENT(K) = ITEMP(7)
C
C XIDENT(8)             ORBN  0 05 040 /  1320  ORBIT NUMBER            
C
      K         = K + 1
      IF(ITEMP(9).NE.99999) XIDENT(K) = ITEMP(9)
C
C XIDENT(9)             SCNN  0 05 201 /  1481  SCAN NUMBER            
C
      K         = K + 1
      IF(ITEMP(8).NE.99999) XIDENT(K) = ITEMP(8)
C
      XLOC      = BMISS
      PRDUCT    = BMISS
      METFET    = BMISS
C
      DO 5000 ISCAN = 1 , 64
        IBSCAN    = 12 + (ISCAN - 1) * 16
        K         = 0
C
C XLOC(1)               CLAT  0 05 002 /  1282  LATITUDE                
C
        K         = K + 1
        IF(ITEMP(IBSCAN+1).NE.99999) XLOC(K,ISCAN) =
     1                               FLOAT(ITEMP(IBSCAN+1)) * 0.01
C
C XLOC(2)               CLON  0 06 002 /  1538  LONGITUDE               
C
        K         = K + 1
        IF(ITEMP(IBSCAN+2).NE.99999) THEN
          IF(ITEMP(IBSCAN+2).GT.18000) THEN
            XLOC(K,ISCAN) = FLOAT(ITEMP(IBSCAN+2) - 36000) * 0.01
          ELSE
            XLOC(K,ISCAN) = FLOAT(ITEMP(IBSCAN+2)        ) * 0.01
          ENDIF
        ENDIF
C
C XLOC(3)               SFTG  0 20 217 /  5337  SURFACE TAG             
C
        K         = K + 1
        IF(ITEMP(IBSCAN+3).NE.99999) XLOC(K,ISCAN) = ITEMP(IBSCAN+3)
C
        K           = 0
C
C PRDUCT(1)             VILWC 0 21 031 /        VERTICALLY-INTEGRATED
C                                               LIQUID WATER CONTENT
C                                               (METFET DEFINES AS
C                                               CLOUD - CLOUD WATER)
C
        K           = K + 1
        IF(ITEMP(IBSCAN+4).NE.99999) PRDUCT(K,ISCAN) =
     1                               FLOAT(ITEMP(IBSCAN+4)) * 0.01
        METFET((ISCAN*2)-1) = 12    ! cloud
        METFET((ISCAN*2)-0) = BMISS ! cancel
C
C PRDUCT(2)             REQV  0 13 014 /  3342  RAINFALL RATE
C
        K           = K + 1
        IF(ITEMP(IBSCAN+6).NE.99999) PRDUCT(K,ISCAN) =
     1                               FLOAT(ITEMP(IBSCAN+6)) / 3600.0
C
C PRDUCT(3)             WSPD  0 11 002 /        WIND SPEED (OCEAN)
C
        K           = K + 1
        IF(ITEMP(IBSCAN+7).NE.99999) PRDUCT(K,ISCAN) =
     1                               FLOAT(ITEMP(IBSCAN+7)) * 0.1
C
C PRDUCT(4)             SMOI  0 13 197 /  3525  SOIL MOISTURE
C
        K           = K + 1
        IF(ITEMP(IBSCAN+8).NE.99999) PRDUCT(K,ISCAN) =
     1                               FLOAT(ITEMP(IBSCAN+8)) * 0.001
C
C PRDUCT(5)             ICON  0 20 208 /  5328  ICE CONCENTRATION 
C
        K           = K + 1
        IF(ITEMP(IBSCAN+9).NE.99999) PRDUCT(K,ISCAN) =
     1                               FLOAT(ITEMP(IBSCAN+9))
C
C PRDUCT(6)             ICAG  0 20 209 /  5329  ICE AGE 
C
        K           = K + 1
        IF(ITEMP(IBSCAN+10).NE.99999) PRDUCT(K,ISCAN) = ITEMP(IBSCAN+10)
C
C PRDUCT(7)             ICED  0 20 210 /  5330  ICE EDGE 
C
        K           = K + 1
        IF(ITEMP(IBSCAN+11).NE.99999) PRDUCT(K,ISCAN) = ITEMP(IBSCAN+11)
C
C PRDUCT(8)             TPWT  0 13 016 /        PRECIPITABLE WATER
C
        K           = K + 1
        IF(ITEMP(IBSCAN+12).NE.99999) PRDUCT(K,ISCAN) =
     1                                FLOAT(ITEMP(IBSCAN+12)) * 0.1
C
C PRDUCT(9)             TMSK  0 12 061 /  3133  SKIN TEMPERATURE
C
        K           = K + 1
        IF(ITEMP(IBSCAN+13).NE.99999) PRDUCT(K,ISCAN) = ITEMP(IBSCAN+13)
C
C PRDUCT(10)            TOSD  0 13 013 /        SNOW DEPTH       
C
        K           = K + 1
        IF(ITEMP(IBSCAN+14).NE.99999) PRDUCT(K,ISCAN) =
     1                                FLOAT(ITEMP(IBSCAN+14)) * 0.001
C
C PRDUCT(11)            RFLG  0 33 217 /  8665  RAIN FLAG (WIND QUALITY)
C
        K           = K + 1
        IF(ITEMP(IBSCAN+15).NE.99999) PRDUCT(K,ISCAN) = ITEMP(IBSCAN+15)
C
C PRDUCT(12)            SFTP  0 20 216 /  5336  CALCULATED SURFACE TYPE
C
        K           = K + 1
        IF(ITEMP(IBSCAN+16).NE.99999) PRDUCT(K,ISCAN) = ITEMP(IBSCAN+16)
 5000 CONTINUE
C
C-----------------------------------------------------------------------
C  SUBROUTINE PRDRPT WILL WRITE SSMI PRODUCTS INTO THE BUFR
C  DATABASE. LAYOUT OF THE ARRAY CONTAINING THE SSMI REPORT
C  IS AS FOLLOWS:
C-----------------------------------------------------------------------
C
C  THE LIST OF SSMI DATA AS CONVERTED AND WRITTEN TO BUFR FOLLOWS:
C
C     ARRAY            MNEMONIC   DESCRIPTOR   DESCRIPTION
C     -----            --------   ----------   -----------
C
C     XIDENT(1)        SAID       001007       SATELLITE ID
C     XIDENT(2)        YEAR       004001       YEAR
C     XIDENT(3)        MNTH       004002       MONTH
C     XIDENT(4)        DAYS       004003       DAY
C     XIDENT(5)        HOUR       004004       HOUR
C     XIDENT(6)        MINU       004005       MINUTE
C     XIDENT(7)        SECO       004006       SECOND
C     XIDENT(8)        ORBN       005040       ORBIT NUMBER
C     XIDENT(9)        SCNN       005201       SCAN NUMBER
C
C     XLOC(1,1:64)     CLAT       005002       LATITUDE
C     XLOC(2,1:64)     CLON       006002       LONGITUDE
C     XLOC(3,1:64)     SFTG       020217       SURFACE TAG
C     XLOC(4,1:64)     POSN       005202       POSITION NUMBER
C
C     PRD( 1,1:64)     VILWC      013192       VERTICALLY-INTEGRATED
C                                              LIQUID WATER CONTENT
C                                              (METFET DEFINES AS
C                                               CLOUD - CLOUD WATER)
C     METFET(1:127:2)  METFET     008011       METEOROLOGICAL FEATURE
C                                              (SET: = 12 - CLOUD)
C     METFET(2:128:2)  METFET     008011       METEOROLOGICAL FEATURE
C                                              (CANCEL: = MISSING)
C     PRD( 2,1:64)     REQV       013014       RAINFALL RATE
C     PRD( 3,1:64)     WSPD       011002       WIND SPEED (OCEAN)
C     PRD( 4,1:64)     SMOI       013193       SOIL MOISTURE
C     PRD( 5,1:64)     ICON       020208       ICE CONCENTRATION
C     PRD( 6,1:64)     ICAG       020209       ICE AGE
C     PRD( 7,1:64)     ICED       020210       ICE EDGE
C     PRD( 8,1:64)     TPWT       013016       PRECIPITABLE WATER
C     PRD( 9,1:64)     TMSK       012061       SKIN TEMPERATURE
C     PRD(10,1:64)     TOSD       013013       SNOW DEPTH
C     PRD(11,1:64)     RFLG       033217       RAIN FLAG (WIND QUALITY)
C     PRD(12,1:64)     SFTP       020216       CALCULATED SURFACE TYPE
C
C
C-----------------------------------------------------------------------

C  GET THE WRITE UNIT AND WRITE A SUBSET
C  -------------------------------------
 
      CALL UFBINT(IUNT,XIDENT, 9,  1,IRET,XIDST)
      CALL UFBINT(IUNT,XLOC  , 4, 64,IRET,XLCST)
      CALL UFBINT(IUNT,PRDUCT,12, 64,IRET,PRDST)
      CALL UFBREP(IUNT,METFET, 1,128,IRET,'METFET')
      CALL WRITSB(IUNT)

      IERR = 0
      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    NN3_DRVR    CALC. SSM/I PRODS FROM NEURAL NET 3 ALG.
C   PRGMMR: V. KRASNOPOLSKY  ORG: NP20       DATE: 1997-02-02
C
C ABSTRACT: THIS RETRIEVAL ALGORITHM IS A NEURAL NETWORK IMPLEMENTATION
C   OF THE SSM/I TRANSFER FUNCTION.  IT RETRIEVES THE WIND SPEED (W)
C   AT THE HEIGHT 20 METERS, COLUMNAR WATER VAPOR (V), COLUMNAR LIQUID
C   WATER (L) AND SST. THE NN WAS TRAINED USING BACK-PROPAGATION
C   ALGORITHM.  TRANSFER FUNCTION IS DESCRIBED AND COMPARED WITH
C   CAL/VAL AND OTHER ALGORITHMS IN OMB TECHNICAL NOTE NO. 137.  SEE
C   REMARKS FOR DETAILED INFO ON THIS ALGORITHM.  THIS IS AN IMPROVED
C   VERSION OF THE EARLIER NEURAL NETWORK 2 ALGORITHM.
C
C PROGRAM HISTORY LOG:
C   97-02-02  V. KRASNOPOLSKY -- ORIGINAL AUTHOR
C   98-03-12  BERT KATZ -- VECTORIZED VERSION OF CODE
C   98-12-01  BERT KATZ -- UPDATED DOCBLOCK 
C
C USAGE:    CALL NN3_DRVR(XT,SPN,V,L,SST,JERR,NVECT)
C   INPUT ARGUMENT LIST:
C     XT       - 7-WORD ARRAY CONTAINING BRIGHTNESS TEMPERATURE IN THE
C              - ORDER: T19V (WORD 1), T19H (WORD 2), T22V (WORD 3),
C              - T37V (WORD 4), T37H (WORD 5), T85V (WORD 6), T85H
C              - (WORD 7) (ALL IN KELVIN)
C
C   OUTPUT ARGUMENT LIST:
C     SPN      - WIND SPEED (METERS/SECOND) AT THE HEIGHT OF 20 METERS
C     V        - COLUMNAR WATER VAPOR (TOTAL PRECIP. WATER) (MM)
C     L        - COLUMNAR LIQUID WATER (MM)
C     SST      - SEA SURFACE TEMPERATURE (DEG. C)
C     JERR     - ERROR RETURN CODE:
C                    = 0 -- GOOD RETRIEVALS
C                    = 1 -- RETRIEVALS COULD NOT BE MADE DUE TO ONE OR
C                           MORE BRIGHTNESS TEMPERATURES OUT OF RANGE
C                           (I.E, FAILED THE RAIN FLAG TEST)
C                    = 2 -- RETRIEVALS COULD NOT BE MADE DUE TO ICE
C                           CONTAMINATION
C                   {FOR EITHER 1 OR 2 ABOVE, ALL RETRIEVALS SET TO
C                    99999. (MISSING)}
C     NVECT    - NUMBER OF POINTS PROCESSED
C
C REMARKS: CALLED BY SUBROUTINE IN_LINE.
C
C      Description of training and test data set:
C      ------------------------------------------
C      The training set consists of 3460 matchups which were received
C      from two sources:
C         1.  3187 F11/SSMI/buoy matchups were filtered out from a
C             preliminary version of the new NRL database which was
C             kindly provided by G. Poe (NRL). Maximum available wind
C             speed is 24 m/s.
C         2.  273 F11/SSMI/OWS matchups were filtered out from two
C             datasets collected by high latitude OWS LIMA and MIKE.
C             These data sets were kindly provided by D. Kilham
C             (University of Bristol).  Maximum available wind speed
C             is 26.4 m/s.  
C
C      Satellite data are collocated with both buoy and OWS data in
C      space within 15 km and in time within 15 min.
C
C      The test data set has the same structure, the same number of
C      matchups and maximum buoy wind speed.
C
C      Description of retrieval flags:
C      -------------------------------
C      Retrieval flags by Stogryn et al. are used.  The algorithm
C      produces retrievals under CLEAR + CLOUDY conditions, that is
C      if:
C
C                T37V - T37H > 50.   => CLEAR condition
C                or
C                T37V - T37H =< 50.|
C                T19H =< 185.  and |
C                T37H =< 210.  and | => CLOUDY conditions
C                T19V  < T37V      |
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE NN3_DRVR(XT,SPN,V,L,SST,JERR,NVECT)
      PARAMETER (IOUT =4, IN =5)
      LOGICAL  LQ1,LQ2,LQ3,LQ4,LQ5
      REAL  XT(NVECT,7),Y(NVECT,IOUT),V(NVECT),L(NVECT),SST(NVECT)
      REAL  SPN(NVECT)
      INTEGER JERR(NVECT)

C --------  Retrieval flag (Stogryn) -------------------------

      DO I=1,NVECT

C  T19H =< 185

         LQ1 = (XT(I,2).LE.185.)

C  T37H =< 210

         LQ2 = (XT(I,5).LE.210.)

C  T19V < T37V

         LQ3 = (XT(I,1).LT.XT(I,4))

C  T37V - T37H =< 50.

         LQ4 = ((XT(I,4) - XT(I,5)).LE.50.)
         LQ5 = (LQ1.AND.LQ2.AND.LQ3)
         IF(.NOT.LQ5.AND.LQ4) THEN
            JERR(I) = 1
         END IF
      END DO

C ------ Remove ice contamination ------------------------------------

      DO I = 1 , NVECT
         SI85 = -174.4 + (0.715 * XT(I,1)) + (2.439 * XT(I,3)) -
     $     (0.00504 * XT(I,3) * XT(I,3)) - XT(I,6)
         TT = 44. + (0.85 * XT(I,1))
         IF(SI85.GE.10.)  THEN
            IF(XT(I,3).LE.TT)  JERR(I) = 2
            IF((XT(I,3).GT.264.).AND.((XT(I,3)-XT(I,1)).LT.2.))
     $          JERR(I) = 2
         END IF
      END DO

C --------------- Call NN ----------------------

C  NN WIND SPEED

      CALL NN3_ALG(XT,JERR,Y,NVECT)

      DO I = 1 , NVECT
         SPN(I)=Y(I,1)
         V(I)=Y(I,2)
         L(I)=Y(I,3)
         SST(I)=Y(I,4)
      END DO
c     write(6,9876) (l(i),i=1,nvect)
c9876 format(' in nn3_drvr, ch2o = '/(1x,1p7e10.3))
c     write(6,9875) (sst(i),i=1,nvect)
c9875 format(' in nn3_drvr, sst = '/(1x,1p7e10.3))

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SSMIUNPK
C   PRGMMR: KEYSER           ORG: NP23        DATE: 2012-12-31
C
C ABSTRACT: READS ONE SSM/I SCAN LINE FROM A SHARED PROCESSING IDB
C   DATA SET.  EACH CALL RETURNS 64 RETRIEVALS WHICH MAKE UP A SCAN
C   LINE.  WHEN BRIGHTNESS TEMPERATURES ARE REQUESTED, ONE ARRAY
C   CONTAINS 64 SCANS OF THE 7 BRIGHTNESS TEMPERATURES AND A SECOND
C   ARRAY CONTAINS 64 SCANS OF HIGH-DENSITY 85 GHz BRIGHTNESS
C   TEMPERATURES.  ALL SCANS (RECORDS) ARE PROCESSED REGARDLESS OF
C   THEIR TIME.  WHEN A VALID SCAN IS READ THE PROGRAM RETURNS TO THE
C   CALLING PROGRAM.  THE INPUT IDB DATA SETS CAN BE OF TWO TYPES.
C   EITHER:
C     1) RETRIEVED PRODUCTS (ORIGINATING FROM EDR DATA SETS) OR
C     2) BRIGHTNESS TEMPERATURES (ORIGINATING FROM SDR DATA SETS).
C   THE TYPE IS CONTROLLED BY THE INPUT ARGUMENT "ITYPE".
C
C PROGRAM HISTORY LOG:
C 1993-12-30  D. A. KEYSER (NMC22) -- ORIGINAL AUTHOR
C 1994-09-22  D. A. KEYSER (NMC22) -- EARLIER FORM COULD ONLY PROCESS
C       RETRIEVED PRODUCTS ORIGINATING FROM EDR DATA SETS, SUBROUTINE
C       EXPANDED TO ALSO PROCESS BRIGHTNESS TEMPERATURES ORIGINATING
C       FROM SDR DATA SETS (VIA NEW INPUT ARGUMENT "ITYPE")
C 1994-12-08  D. A. KEYSER (NMC22) -- ADDED ABILITY TO READ IN BT'S
C       (7) FROM THE SDR DATA SETS AND GENERATE THE WIND SPEED PRODUCT
C       IN-LINE USING EITHER THE NEURAL NETWORK ALGORITHM OR THE
C       OPERATIONAL GOODBERLET ALGORITHM (VIA INPUT ARGUMENT "ITYPE");
C       IF THE ARGUMENT "IER" IS SET TO "-99" UPON INPUT, THEN THIS
C       SUBROUTINE BYPASSES ALL TIME CHECKS AND ASSUMES THAT THE INPUT
C       DATA SET(S) CONTAIN ONLY DATA RECORDS (SCANS) ALL OF WHICH ARE
C       TO BE PROCESSED REGARDLESS OF THEIR TIME
C 1995-05-16  D. A. KEYSER (NMC22) -- PREVIOUSLY, ONLY THE HEADER
C       RECORD FOR THE 1ST ORBIT IN THE DATA SET WAS CHECKED TO
C       SEE IF THE TYPE OF DATA SET (IDB OR SDR) AGREES WITH WHAT THE
C       USER EXPECTS. NOW IF THERE IS DISAGREEMENT, THE HEADER RECORD
C       FOR THE 2ND ORBIT IN THE DATA SET IS ALSO CHECKED. THIS
C       IS BECAUSE THE FIRST ORBIT MAY CONTAIN BAD (ZEROED) RECORDS
C 1996-10-08  BERT KATZ (NP2) -- MODIFIED HANDLING OF INTERNAL TIME
C       STAMPS (START-OF-ORBIT, END-OF-ORBIT, SYSTEM WRITE TIME) WHICH
C       ARE OCCASIONALLY INCONSISTENT.  THE ASSUMPTION IS MADE THAT
C       THE SYSTEM WRITE TIME IS CORRECT, AND THAT START-OF-ORBIT
C       PRECEDES END-OF-ORBIT PRECEDES SYSTEM WRITE TIME. IN THIS WAY,
C       THE MAXIMUM AMOUNT OF DATA IS RENDERED USABLE.
C 1998-09-02  BERT KATZ (NP2) -- ADDED HIGH-DENSITY 85 GHz BRIGHTNESS
C       TEMPERATURE OUTPUT.
C 1998-12-01  BERT KATZ (NP2) -- REPLACED USAGE OF SUBROTINES      
C       W3FS12, W3FS13, W3FS21, AND W3FS22 BY W3MOVDAT, W3DOXDAT, AND
C       W3DIFDAT TO ACHIEVE Y2K AND FORTRAN 90 COMPLIANCE.  ALSO
C       REPLACED USE OF  SUBROUTINES SMIUNP03, . . . , SMIUNP09 
C       BY CORRESPONDING SUBROUTINES MISC03, . . . ,   MISC09 TO REDUCE
C       REDUNDANT CODE.  USE OF UNIT NUMBERS FOR GRIB SST FILE, 
C       GRIB INDEX FILE, AND LAND-SEA TAG FILE WAS MADE CONSISTENT
C       WITH REST OF CODE.
C 2000-02-22  BERT KATZ (NP2) -- REPLACED USE OF SUBROUTINE MISC09     
C       BY BAREAD FROM BACIO LIBRARY.  THIS ENABLED PORT OF TRANSSMI
C       FROM THE CRAY TO THE IBM/SP.
C 2000-06-15  D. A. KEYSER (NP22) -- CORRECTED SCALING FACTOR FOR
C       RAINFALL RATE - MUST MULTIPLY PACKED VALUE BY 0.2 TO GET IT
C       INTO MM/HR (BEFORE NO SCALING WAS ASSUMED)
C 2001-01-16  D. A. KEYSER (NP22) -- REMOVED LOGIC PERTAINING TO
C       IN-LINE CALC. OF WIND SPEED VIA NN OR GOODBERLET ALG. SINCE
C       IT IS NEVER APPROPRIATE HERE
C 2012-12-31  D. C. STOKES (NP23) -- INPUT SSM/I DATA AS STREAM RATHER
C       THAN AS BLOCKED RECORDS SINCE UPSTEAM PROCESSING ON WCOSS NO
C       LONGER BLOCKS INPUT FILE
C      
C
C USAGE:    CALL SSMIUNPK(INFIL,ITYPE,IBUFTN,IBUF85,KOUNT,NRECN,IER)
C   INPUT ARGUMENT LIST:
C     INFIL    - UNIT NUMBER OF IDB DATA SET HOLDING SCAN DATA
C              - FOR A PARTICULAR DMSP SATELLITE
C     ITYPE    - CONTROLS TYPE OF SSM/I DATA TO PROCESS (=1- RETRIEVED
C              - PRODUCTS/EDR OR =2- BRIGHTNESS TEMPERATURE/SDR)
C     KOUNT    - INDICATOR FOR FIRST CALL TO SUBROUTINE (SEE REMARKS)
C
C   OUTPUT ARGUMENT LIST:
C     IBUFTN   - OUTPUT BUFFER HOLDING DATA FOR A SCAN (1036 WORDS -
C              - SEE REMARKS FOR FORMAT, WILL DIFFER BASED ON "ITYPE")
C     IBUF85   - OUTPUT BUFFER HOLDING DATA FOR A SCAN OF HIGH DENSITY
C              - 85 GHz BRIGHTNESS TEMPERATURES (1548 WORDS -
C              - SEE REMARKS FOR FORMAT)
C     KOUNT    - COMPLETION CODE OR COUNT OF SUCCESSFUL RETURNS
C              - (SEE REMARKS)
C     NRECN    - LOGICAL RECORD NUMBER (SCAN) W/I FILE PROCESSED
C     IER      - ERROR RETURN CODE (SEE REMARKS)
C
C   INPUT FILES :
C     UNIT INFIL - IDB SHARED PROCESSING DATA SET HOLDING SSM/I DATA 
C                - (TYPE OF DATA MUST CORRESPOND TO VALUE OF "ITYPE") 
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C
C REMARKS: 
C
C           FOR KOUNT:
C               INITIALIZED TO ZERO UPON INPUT TO THIS SUBROUTINE
C                  (FIRST CALL).  THEREAFTER, INCREMENTED BY 1 FOR
C                  EACH RECORD (SCAN) SUCCESSFULLY RETURNED.  UPON   ,
C                  COMPLETION IT IS SET TO THE NEGATIVE OF 'KOUNT'.
C               IF OUTPUT KOUNT .LT. 0  PROCESSING COMPLETE (DONE)
C               IF OUTPUT KOUNT .EQ. 0  NO REPORTS FOUND (DONE)
C               IF OUTPUT KOUNT .GT. 0  CUMULATIVE NO. FOUND SO FAR
C              NOTE: IF INPUT ARGUMENT 'INFIL' DIFFERS FROM WHAT IT
C                    WAS IN PREVIOUS CALL TO THIS SUBROUTINE THEN
C                    KOUNT IS ALWAYS SET TO ZERO.  FOR THIS REASON,
C                    THE CALLING PROGRAM SHOULD NOT CHANGE TO A NEW
C                    UNIT NUMBER UNTIL ALL SCANS FROM THE PREVIOUS
C                    UNIT NUMBER HAVE BEEN PROCESSED
C           FOR IER (UPON OUTPUT):
C               IF IER = 0  SUCCESSFUL RETURN OF RECORD FROM UNIT
C                           'INFIL'
C               IF IER = 2  PROCESSING ERROR IN TIME IN DIRECTORY
C                           (HEADER) RECORD IN UNIT 'INFIL'
C               IF IER = 3  READ ERROR: ORBIT DIRECTORY (HEADER) OR
C                           DATA RECORD IN UNIT 'INFIL'
C
C
C   NOTE:
C
C     CONTENTS OF ARRAY 'IBUFTN' (HOLDING ONE COMPLETE SCAN OF
C       64 INDIVIDUAL RETRIEVALS -- 1036 WORDS):
C
C    =====> FOR ITYPE = 1 OR 2
C
C          WORD   CONTENTS
C          ----   --------
C            1    SATELLITE ID
C            2    2-DIGIT YEAR OF CENTURY FOR SCAN
C            3    2-DIGIT MONTH OF YEAR FOR SCAN
C            4    2-DIGIT DAY OF MONTH FOR SCAN
C            5    2-DIGIT HOUR OF DAY FOR SCAN
C            6    2-DIGIT MINUTE OF HOUR FOR SCAN
C            7    2-DIGIT SECOND OF MINUTE FOR SCAN
C            8    SCAN NUMBER IN ORBIT
C            9    ORBIT NUMBER
C           10    SPARE (SET TO 88888)
C           11    SPARE (SET TO 88888)
C           12    SPARE (SET TO 88888)
C
C    =====> FOR ITYPE = 1
C
C           13    RETRIEVAL NO. 1 LATITUDE  (*100 DEGREES: + N, - S)
C           14    RETRIEVAL NO. 1 LONGITUDE (*100 DEGREES EAST)
C           15    RETRIEVAL NO. 1 SURFACE TAG (CODE FIGURE)
C           16    RETRIEVAL NO. 1 CLOUD WATER (*100 MILLIMETERS)
C           17    RETRIEVAL NO. 1 SPARE (SET TO 88888)
C           18    RETRIEVAL NO. 1 RAIN RATE (MILLIMETERS/HOUR)
C           19    RETRIEVAL NO. 1 WIND SPEED (*10 METERS/SECOND)
C           20    RETRIEVAL NO. 1 SOIL MOISTURE (MILLIMETERS)
C           21    RETRIEVAL NO. 1 SEA-ICE CONCENTRATION (PER CENT)
C           22    RETRIEVAL NO. 1 SEA-ICE AGE (CODE FIGURE)
C           23    RETRIEVAL NO. 1 ICE EDGE (CODE FIGURE)
C           24    RETRIEVAL NO. 1 TOTAL PRECIP. WATER (*10 MILLIMETERS)
C           25    RETRIEVAL NO. 1 SURFACE TEMPERATURE (DEG. KELVIN)
C           26    RETRIEVAL NO. 1 SNOW DEPTH (MILLIMETERS)
C           27    RETRIEVAL NO. 1 RAIN FLAG (CODE FIGURE)
C           28    RETRIEVAL NO. 1 CALCULATED SURFACE TYPE (CODE FIGURE)
C      29-1036    REPEAT 13 - 28  FOR 63 MORE RETRIEVALS
C
C    =====> FOR ITYPE = 2
C
C           13    RETRIEVAL NO. 1 LATITUDE  (*100 DEGREES: + N, - S)
C           14    RETRIEVAL NO. 1 LONGITUDE (*100 DEGREES EAST)
C           15    RETRIEVAL NO. 1 19 GHZ V BRIGHTNESS TEMP (*100 DEG. K)
C           16    RETRIEVAL NO. 1 19 GHZ H BRIGHTNESS TEMP (*100 DEG. K)
C           17    RETRIEVAL NO. 1 22 GHZ V BRIGHTNESS TEMP (*100 DEG. K)
C           18    RETRIEVAL NO. 1 37 GHZ V BRIGHTNESS TEMP (*100 DEG. K)
C           19    RETRIEVAL NO. 1 37 GHZ H BRIGHTNESS TEMP (*100 DEG. K)
C           20    RETRIEVAL NO. 1 85 GHZ V BRIGHTNESS TEMP (*100 DEG. K)
C           21    RETRIEVAL NO. 1 85 GHZ H BRIGHTNESS TEMP (*100 DEG. K)
C           22    RETRIEVAL NO. 1 SURFACE TAG (CODE FIGURE)
C           23    RETRIEVAL NO. 1 POSITION NUMBER
C           24    RETRIEVAL NO. 1 SPARE (SET TO 88888)
C           25    RETRIEVAL NO. 1 SPARE (SET TO 88888)
C           26    RETRIEVAL NO. 1 SPARE (SET TO 88888)
C           27    RETRIEVAL NO. 1 SPARE (SET TO 88888)
C           28    RETRIEVAL NO. 1 SPARE (SET TO 88888)
C      29-1036    REPEAT 13 - 28  FOR 63 MORE RETRIEVALS
C
C     CONTENTS OF ARRAY 'IBUF85' (HOLDING ONE COMPLETE SCAN OF
C       HIGH DENSITY 85 GHz BRIGHTNESS TEMPERATURE -- 1548 WORDS):
C                
C    =====> FOR ITYPE = 2
C
C          WORD   CONTENTS
C          ----   --------
C            1    SATELLITE ID
C            2    2-DIGIT YEAR OF CENTURY FOR SCAN
C            3    2-DIGIT MONTH OF YEAR FOR SCAN
C            4    2-DIGIT DAY OF MONTH FOR SCAN
C            5    2-DIGIT HOUR OF DAY FOR SCAN
C            6    2-DIGIT MINUTE OF HOUR FOR SCAN
C            7    2-DIGIT SECOND OF MINUTE FOR SCAN
C            8    SCAN NUMBER IN ORBIT
C            9    ORBIT NUMBER
C           10    SPARE (SET TO 88888)
C           11    SPARE (SET TO 88888)
C           12    SPARE (SET TO 88888)
C
C           13    RETRIEVAL NO. 1 LATITUDE  (*100 DEGREES: + N, - S)
C           14    RETRIEVAL NO. 1 LONGITUDE (*100 DEGREES EAST)
C           15    RETRIEVAL NO. 1 85 GHZ V BRIGHTNESS TEMP (*100 DEG. K)
C           16    RETRIEVAL NO. 1 85 GHZ H BRIGHTNESS TEMP (*100 DEG. K)
C           17    RETRIEVAL NO. 1 SURFACE TAG (CODE FIGURE)
C           18    RETRIEVAL NO. 1 POSITION NUMBER
C           19    RETRIEVAL NO. 1 SPARE (SET TO 88888)
C           20    RETRIEVAL NO. 1 SPARE (SET TO 88888)
C      21-1548    REPEAT 13 - 20  FOR 191 MORE RETRIEVALS
C
C             (NOTE:  ALL MISSING DATA ARE SET TO 99999)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE SSMIUNPK(INFIL,ITYPE,IBUFTN,IBUF85,KOUNT,NRECN,IER)

      CHARACTER*1  TAGT(2)
      CHARACTER*8  CBUFF(419),CBUFF1

      INTEGER  INDIRC(21),JNDIRC(21,30),JDATE(8),KDATE(8),IBUFTN(1036),
     $ JBUFTN(1036),LDATE(8),IDATA(16,64),KDATA(16),IRECL(2),IKY(2),
     $ LBTER(3:9)
      INTEGER IBUF85(1548),JBUF85(1548),KDT85(8),IDT85(8,192)
      INTEGER IDATIN(8),IDTOUT(8),IDTREF(8)
      REAL TIMINC(5),TDIFF(5)

      COMMON/SMIUNPAA/IREC,INDIRC,JNDIRC

      EQUIVALENCE (JBUFTN(13),IDATA),(JBUF85(13),IDT85)

      SAVE

      DATA  KNTTOT/0/,KNTEMP/0/,KNTUNP/0/,KNTDIR/0/,KNTTSC/0/,
     $ IRECL/1300,3348/,IKY/10,26/,INFIL_L/0/,ITYPEL/0/,TAGT/'E','S'/,
     $ LAERR/0/,LOERR/0/,LBTER/7*0/

C A NEW UNIT NUMBER OR ITYPE FOR THIS CALL WILL SET KOUNT TO ZERO
C  REGARDLESS OF WHAT THE INPUT VALUE FOR KOUNT IS
      IF(INFIL.NE.INFIL_L.OR.ITYPE.NE.ITYPEL)  KOUNT = 0
      INFIL_L    = INFIL
      ITYPEL = ITYPE
      NWORDS = IRECL(ITYPE) / 8
      ICHREM = MOD(IRECL(ITYPE),8)
      IF(KOUNT.GT.0)  THEN
C***********************************************************************
C IF NOT THE FIRST TIME IN, GO DIRECTLY TO READ A SCAN
         GO TO 700
C***********************************************************************
      ELSE  IF(KOUNT.LT.0)  THEN
C A KOUNT LESS THAN ZERO MEANS A PROBLEM
         PRINT 102, KOUNT
  102 FORMAT(' ##SSMIUNPK: INPUT ARGUMENT "KOUNT" (',I7,') IS ',
     $  'NEGATIVE -- ABNORMAL TERMINATION'/)
         GO TO 1200
      ELSE
C***********************************************************************
C IF FIRST TIME IN, OPEN INPUT FILE AS STREAM ACCESS (UNFORMATTED)
C***********************************************************************
         OPEN(INFIL,ACCESS='STREAM',FORM='UNFORMATTED')
         CALL W3UTCDAT(IDTREF)
         IDTREF(1)   = IDTREF(1) - 1
         IDTREF(2:3) = 1
         IDTREF(4:8) = 0
      END IF
C***********************************************************************
C FIRST CALL TO SUBROUTINE FOR THIS SATELLITE AND/OR TYPE WILL COME HERE
C      TEST THAT SPECIFIED TYPE (ITYPE) AGREES WITH TYPE IN FILE
C      SCAN EACH ORBIT'S DIRECTORY (HEADER) RECORD FOR VALID TIME
C***********************************************************************
      IF(ITYPE.EQ.1)  THEN
         PRINT 66, INFIL
   66 FORMAT(//' ** SSMIUNPK - VERSION 12/31/2012: PROCESSING ',
     $ 'DATA IN INPUT UNIT ',I3,' WITH ITYPE OF 1 -- PRODUCTS ',
     $ 'ORIGINATING IN EDR DATA SETS'/)
      ELSE
         PRINT 167, INFIL
  167 FORMAT(//' ** SSMIUNPK - VERSION 12/31/2012: PROCESSING ',
     $ 'DATA IN INPUT UNIT ',I3,' WITH ITYPE OF 2 -- B. TEMPS ',
     $ 'ORIGINATING IN SDR DATA SETS'/)
      END IF
      PRINT 704
  704 FORMAT(' SSMIUNPK: NO TIME CHECKS WILL BE PERFORMED ON SCANS - ',
     $ 'ALL SCANS READ IN ARE PROCESSED'/)
      IREC = 1
      print *, 'nwords = ',nwords
      print *, 'ichrem = ',ichrem
      IF(ICHREM.EQ.0) THEN
         READ(INFIL,ERR=1001,END=1201) (CBUFF(I),I=1,NWORDS)
      ELSE
         READ(INFIL,ERR=1001,END=1201) (CBUFF(I),I=1,NWORDS),
     $                                  CBUFF(NWORDS+1)(1:ICHREM)
      ENDIF
  200 CONTINUE
C IF EBCDIC CHAR. ON THIS MACHINE, MUST CONVERT "PRODUCT ID" TO EBCDIC
      if(mova2i('A').eq.193)  print *, 'call w3ai39 invoked'
      IF(MOVA2I('A').EQ.193)  CALL W3AI39(CBUFF,16)
      print *, 'cbuff(1) = ',cbuff(1)
      print *, 'cbuff(2) = ',cbuff(2)
      IF(CBUFF(2)(7:7).NE.TAGT(ITYPE))  THEN
C IF ABOVE TEST SATISFIED, IT MAY BE DUE TO A ZEROED OUT HEADER
C  RECORD FOR THE FIRST ORBIT IN THE FILE 
         PRINT 8891
 8891    FORMAT(//'  ########## FIRST RECORD IN DATA SET POSSIBLY ',
     $    'CONTAINS GARBAGE!!!!!'//)
         IER = 2
         NRECN = 0
         KOUNT = - KOUNT
         RETURN
      END IF
      INDIRC = 0
      ISBIT = 0
      ISWRD = 1
      INDIRC(2) = IREC
      INDIRC(1) = (INDIRC(2)/1725) + 1
      CALL GBYTEC(CBUFF(ISWRD),ILNGT1,  0,16)
      CALL GBYTEC(CBUFF(ISWRD),IYEAR, 160,16)
      INDIRC(14) = IYEAR
      INDIRC(5) = IYEAR
      IF(MOD(IYEAR,400).EQ.0) THEN
        IYCLEN = 366
      ELSE IF(MOD(IYEAR,100).EQ.0 .OR. MOD(IYEAR,4).NE.0) THEN
        IYCLEN = 365
      ELSE
        IYCLEN = 366
      ENDIF
      IF(MOD(IYEAR-1,400).EQ.0) THEN
        IYPLEN = 366
      ELSE IF(MOD(IYEAR-1,100).EQ.0 .OR. MOD(IYEAR-1,4).NE.0) THEN
        IYPLEN = 365
      ELSE
        IYPLEN = 366
      ENDIF
      CALL GBYTESC(CBUFF(ISWRD),INDIRC(15),176, 8,0,4)
      IDATIN(1:3) = INDIRC(14:16)
      IDATIN(4:8) = 0
      CALL W3DOXDAT(IDATIN,JDOFWK,JDYWRT,JULDAY)
      ISBIT = ISBIT + (ILNGT1 * 16)
      ISWRD = ISBIT/64 + 1
      IADON = MOD(ISBIT,64)
      CALL GBYTEC(CBUFF(ISWRD),ILNGT2,      0+IADON,16)
      CALL GBYTEC(CBUFF(ISWRD),INDIRC(20),112+IADON,16)
      ISBIT = ISBIT + (ILNGT2 * 16)
      ISWRD = ISBIT/64 + 1
      IADON = MOD(ISBIT,64)
      CALL GBYTEC(CBUFF(ISWRD),ILNGT3,0+IADON,16)
      ISBIT = ISBIT + (ILNGT3 * 16)
      ISWRD = ISBIT/64 + 1
      IADON = MOD(ISBIT,64)
      CALL GBYTEC(CBUFF(ISWRD),ILNGT4,0+IADON,16)
      ISBIT = ISBIT + (ILNGT4 * 16)
      ISWRD = ISBIT/64 + 1
      IADON = MOD(ISBIT,64)
      CALL GBYTEC(CBUFF(ISWRD),ILNGT5,0+IADON,16)
      ISBIT = ISBIT + (ILNGT5 * 16)
      ISWRD = ISBIT/64 + 1
      IADON = MOD(ISBIT,64)
      CALL GBYTEC (CBUFF(ISWRD),INDIRC(19),64+IADON,32)
      CALL GBYTEC (CBUFF(ISWRD),JDYBEG,    96+IADON,16)
      CALL GBYTESC(CBUFF(ISWRD),INDIRC(8), 112+IADON,8,0,3)
      CALL GBYTEC (CBUFF(ISWRD),JDYEND,    136+IADON,16)
      CALL GBYTESC(CBUFF(ISWRD),INDIRC(11),152+IADON,8,0,3)
      CALL GBYTEC (CBUFF(ISWRD),INDIRC(21),216+IADON,8)
C INDIRC(3) IS STARTING DATA RECORD FOR THIS ORBIT
      INDIRC(3) = 2
C INDIRC(4) IS ENDING   DATA RECORD FOR THIS ORBIT
      INDIRC(4) = INDIRC(20) + 1
      PRINT 110, (INDIRC(I),I=1,4)
  110 FORMAT(/' @@@ SSMIUNPK:  ORBIT NUMBER',I5,' WITH DIRECTORY ',
     $ 'RECORD NUMBER ',I5/' SCANS BEGIN AT RECORD',I6,' AND END AT',I6)
      IDATIN(1) = INDIRC(5)
      IDATIN(2:3) = 1
      IDATIN(4:8) = 0
      TIMINC(1) = FLOAT(JDYBEG - 1)
      TIMINC(2:5) = 0.0
      CALL W3MOVDAT(TIMINC,IDATIN,IDTOUT)
      INDIRC(6:7) = IDTOUT(2:3)
      PRINT 112, (INDIRC(I),I=5,10)
  112 FORMAT(' START TIME : ',6I6,' BEFORE CORRECTION')
      TIMINC(1) = FLOAT(JDYEND - 1)
      CALL W3MOVDAT(TIMINC,IDATIN,IDTOUT)
      MONEND = IDTOUT(2)
      IDYEND = IDTOUT(3)
      PRINT 113, MONEND,IDYEND,(INDIRC(I),I=11,13)
  113 FORMAT('   END TIME : ',6X,5I6,' BEFORE CORRECTION')
      IF(MOD(JDYWRT,IYCLEN)+1.EQ.JDYBEG) THEN
         JDYBEG = JDYWRT
      ELSE
         IF(JDYWRT.EQ.JDYBEG) THEN
            IF(INDIRC(8).GT.INDIRC(17)) THEN
               JDYBEG = JDYWRT - 1
            ELSE IF(INDIRC(8).EQ.INDIRC(17) .AND.
     $              INDIRC(9).GT.INDIRC(18)) THEN
               JDYBEG = JDYWRT - 1
            ENDIF
            IF(JDYBEG.EQ.0) JDYBEG = IYPLEN
         ENDIF
      ENDIF
      IF(MOD(JDYWRT,IYCLEN)+1.EQ.JDYEND) THEN
         JDYEND = JDYWRT
      ELSE
         IF(JDYWRT.EQ.JDYEND) THEN
            IF(INDIRC(11).GT.INDIRC(17)) THEN
               JDYEND = JDYWRT - 1
            ELSE IF(INDIRC(11).EQ.INDIRC(17) .AND.
     $              INDIRC(12).GT.INDIRC(18)) THEN
               JDYEND = JDYWRT - 1
            ENDIF
            IF(JDYEND.EQ.0) JDYEND = IYPLEN
         ENDIF
      ENDIF
      IF(JDYBEG.EQ.JDYEND) THEN
         IF(INDIRC(8).GT.INDIRC(11)) THEN
            JDYEND = JDYWRT
            INDIRC(8)  = INDIRC(8) - 3
            INDIRC(11:12) = INDIRC(17:18)
            INDIRC(13) = 0
         ELSE IF(INDIRC(8).EQ.INDIRC(11) .AND.
     $           INDIRC(9).GE.INDIRC(12)) THEN
            JDYEND = JDYWRT
            INDIRC(8)  = INDIRC(8) - 3
            INDIRC(11:12) = INDIRC(17:18)
            INDIRC(13) = 0
         ENDIF
         IF(INDIRC(8).LT.0) THEN
           INDIRC(8:10)  = 0
         ENDIF
      ENDIF
      IF(MOD(JDYBEG,IYPLEN)+1.EQ.JDYEND) THEN
         IF(INDIRC(8).LT.INDIRC(11)) THEN
            JDYBEG = JDYEND
         ELSE IF(INDIRC(8).EQ.INDIRC(11) .AND.
     $           INDIRC(9).LT.INDIRC(12)) THEN
            JDYBEG = JDYEND
         ENDIF
      ENDIF
      IF(JDYWRT.EQ.1 .AND. JDYBEG.EQ.IYPLEN)  THEN
C WATCH OUT WHEN THE SYSTEM TIME MOVES INTO THE NEXT YEAR BUT THE DATA
C  TIME IS STILL IN THE PREVIOUS YEAR (MUST OBTAIN CORRECT DATA YEAR)
         IYEAR = IYEAR - 1
         INDIRC(5) = IYEAR
      END IF
      IDATIN(1) = INDIRC(5)
      TIMINC(1) = FLOAT(JDYBEG - 1)
      CALL W3MOVDAT(TIMINC,IDATIN,IDTOUT)
      INDIRC(6:7) = IDTOUT(2:3)
      PRINT 114, (INDIRC(I),I=5,10)
  114 FORMAT(' START TIME : ',6I6,' AFTER CORRECTION')
      TIMINC(1) = FLOAT(JDYEND - 1)
      CALL W3MOVDAT(TIMINC,IDATIN,IDTOUT)
      MONEND = IDTOUT(2)
      IDYEND = IDTOUT(3)
      PRINT 115, MONEND,IDYEND,(INDIRC(I),I=11,13)
  115 FORMAT('   END TIME : ',6X,5I6,' AFTER CORRECTION')
      PRINT 116, (INDIRC(I),I=14,21)
  116 FORMAT(' WRITE TIME : ',5I6/' ORBIT NO. SINCE LAUNCH ',I6,
     $ ' NO. OF SCANS ',I6,' SAT. ID. ',I6)
      JNDIRC(1:21,INDIRC(1)) = INDIRC(1:21)
      JDATE(1:3) = INDIRC(5:7)
      JDATE(4) = 0
      JDATE(5:7) = INDIRC(8:10)
      JDATE(8) = 0
      CALL W3DIFDAT(JDATE,IDTREF,3,TDIFF)
      XJMIN1 = TDIFF(3)
      PRINT 118, (JDATE(I),I=1,3),(JDATE(I),I=5,7),XJMIN1,IDTREF(1)
  118 FORMAT(5X,'SSMIUNPK: EARLIEST DATE IN ORBIT:',6I5,' -- ',
     $ F14.2,' MINUTES SINCE 0000 UTC 1 JAN ',I4,'.')
C CONVERT TIME OF LATEST REPORT IN ORBIT TO NUMBER OF MIN. SINCE
C  REFERENCE DATE
      IF(JDYEND.EQ.JDYBEG) THEN
         KDATE(1:3) = INDIRC(5:7)
      ELSE
         IF(JDYEND.NE.1) THEN
            KDATE(1) = INDIRC(5)
         ELSE
            KDATE(1) = INDIRC(5) + 1
         ENDIF
         IDATIN(1) = KDATE(1)
         TIMINC(1) = FLOAT(JDYEND - 1)
         CALL W3MOVDAT(TIMINC,IDATIN,IDTOUT)
         KDATE(2:3) = IDTOUT(2:3)
      ENDIF
      KDATE(4) = 0
      KDATE(5:7) = INDIRC(11:13)
      KDATE(8) = 0
      CALL W3DIFDAT(KDATE,IDTREF,3,TDIFF)
      XJMIN2 = TDIFF(3)
C DO EARLIEST AND LATEST DATES FOR REPORTS IN ORBIT MAKE SENSE?
      PRINT 119, (KDATE(I),I=1,3),(KDATE(I),I=5,7),XJMIN2,IDTREF(1)
  119 FORMAT(5X,'SSMIUNPK: LATEST   DATE IN ORBIT:',6I5,' -- ',
     $ F14.2,' MINUTES SINCE 0000 UTC 1 JAN ',I4,'.')
      IF(XJMIN2.LT.XJMIN1)  THEN
         PRINT 202
  202 FORMAT(' ##SSMIUNPK: LATEST DATE IN THIS ORBIT IS EARLIER ',
     $ 'THAN EARLIEST DATE !'/' PROCESSING ERROR - DO NOT PROCESS ',
     $ 'THIS ORBIT'/)
         GO TO 2001
      ENDIF
      IF(XJMIN2.GE.XJMIN1+1440.0)  THEN
         PRINT 203
  203 FORMAT(' ##SSMIUNPK: ORBIT IS MORE THAN A DAY LONG !'/
     $ ' PROCESSING ERROR - DO NOT PROCESS THIS ORBIT'/)
         GO TO 2001
      ENDIF
      KNTDIR = KNTDIR + 1
C***********************************************************************
C                    SET-UP FOR READ OF A SCAN
C***********************************************************************
  700 CONTINUE
      IREC = IREC + 1
C CHECK TO MAKE SURE THAT THE RECORD IS INDEED A DATA RECORD AND NOT A
C  HEADER RECORD
      IF(ICHREM.EQ.0) THEN
         READ(INFIL,ERR=1001,END=1201) (CBUFF(I),I=1,NWORDS)
      ELSE
         READ(INFIL,ERR=1001,END=1201) (CBUFF(I),I=1,NWORDS),
     $                               CBUFF(NWORDS+1)(1:ICHREM)
      ENDIF
      CBUFF1 = CBUFF(1)
      IF(MOVA2I('A').EQ.193)  CALL W3AI39(CBUFF1,8)
      IF(CBUFF1(5:8).EQ.'FNOC')  THEN
         PRINT 728, IREC
  728 FORMAT(' ##SSMIUNPK: REC.',I6,' IS AN ORBIT DIRECTORY (HEADER) ',
     $ 'RECORD AND NOT A SCAN - PROCEED TO DIRECTORY PROCESSING')
         GO TO 200
      END IF
      ISBIT = 0
      ISWRD = 1
      CALL GBYTEC(CBUFF(ISWRD),ILNGT1, 0,16)
      IF(ILNGT1.LE.0)  THEN
C IF ILNGT1 IS 0 GO ON TO THE NEXT SCAN IN THIS ORBIT (INITIALIZED REC.)
         PRINT 128, IREC,ILNGT1
  128 FORMAT(' ##SSMIUNPK: SCAN IN REC.',I6,' HAS HEADER BLOCK ',
     $ 'WITH LENGTH',I6,' I*2 WORDS - INDICATES EMPTY SCAN REC. - GO ',
     $ 'ON TO NEXT SCAN')
         KNTEMP = KNTEMP + 1
         GO TO 700
      ELSE  IF(ILNGT1.NE.6)  THEN
C CURRENT VALUE FOR ILNGT1 SHOULD BE 6 - NOTE IF VALUE IS SOMETHING ELSE
         PRINT 129, IREC,ILNGT1
  129 FORMAT(' SSMIUNPK: SCAN IN REC.',I6,' HAS HEADER BLOCK W/ ',
     $ 'LENGTH',I6,' I*2 WORDS - (NOT EXPECTED VALUE OF 6), DO PROCESS',
     $ ' HOWEVER')
      END IF
C GET THE SCAN NUMBER AND TEST IF ZERO (SCAN UNPROCESSABLE)
      CALL GBYTEC(CBUFF(ISWRD),ISCNTR,32,16)
      IF(ISCNTR.EQ.0)  THEN
C IF ISCNTR=0 GO ON TO NEXT SCAN IN THIS ORBIT (THIS SCAN UNPROCESSABLE)
         PRINT 130, IREC
  130 FORMAT(' ##SSMIUNPK: SCAN IN REC.',I6,' HAS ZERO FOR SCAN ',
     $ 'COUNTER - INDICATES UNPROCESSABLE SCAN REC. - GO ON TO NEXT ',
     $ 'SCAN')
         KNTUNP = KNTUNP + 1
         GO TO 700
      END IF
      CALL GBYTEC(CBUFF(ISWRD),ISTIME,48,32)
C READ SUCCESSFUL - GET YEAR, MONTH, DAY, HOUR, AND MINUTE OF SCAN
      LDATE(1:3) = INDIRC(5:7)
      LDATE(4) = 0
      LDATE(5) = ISTIME/3600
      LDATE(6) = (MOD(ISTIME,3600))/60
      LDATE(7) = MOD(ISTIME,60)
      LDATE(8) = 0
C CONVERT THIS TO NUMBER OF MINUTES SINCE REFERENCE DATE
      CALL W3DIFDAT(LDATE,IDTREF,3,TDIFF)
      XNOW = TDIFF(3)
C DO EARLIEST DATE IN ORBIT AND SCAN TIME FOR REPORT MAKE SENSE?
      IF(XNOW.LT.XJMIN1) THEN
         XNOW = XNOW + 1440.0
         IF(XNOW.GT.XJMIN2) THEN
            PRINT 122, IREC,(LDATE(I),I=1,3),(LDATE(I),I=5,7),INDIRC(19)
  122 FORMAT(' ##SSMIUNPK: SCAN IN REC.',I6,' NOT IN SCAN TIME ',
     $ 'INT. - SCAN TIME:',6I5,' - ORBIT',I6,' - GO TO NEXT SCAN')
            KNTTSC = KNTTSC + 1
            GO TO 700
         ELSE
            TIMINC(1) = 0.0
            TIMINC(2) = 24.0
            CALL W3MOVDAT(TIMINC,LDATE,IDTOUT)
            LDATE = IDTOUT
         ENDIF
      ELSE IF(XNOW.GT.XJMIN2) THEN
         PRINT 122, IREC,(LDATE(I),I=1,3),(LDATE(I),I=5,7),INDIRC(19)
         KNTTSC = KNTTSC + 1
         GO TO 700
      ENDIF
C-----------------------------------------------------------------------
C      THIS SCAN PASSED ALL TIME CHECKS -- PACK SCAN INTO IBUFTN
C-----------------------------------------------------------------------
C INITIALIZE JBUFTN ARRAY AS MISSING
      JBUFTN = 99999
C PACK SATELLITE ID INTO JBUFTN(1)
      JBUFTN(1) = INDIRC(21)
C PACK SCAN YEAR, MONTH, DAY, HOUR, MINUTE, SECOND INTO JBUFTN(2)-(7)
      JBUFTN(2:4) = LDATE(1:3)
      JBUFTN(5:7) = LDATE(5:7)
C PACK SCAN NUMBER INTO JBUFTN(8)
      JBUFTN(8) = ISCNTR
C PACK ORBIT NUMBER INTO JBUFTN(9)
      JBUFTN(9) = INDIRC(19)
C JBUFTN(10)-(12) ARE SPARES (SET TO 88888)
      JBUFTN(10:12) = 88888
C BEGIN HIGH-DENSITY 85 GHz CHANNEL DATA
      IF(ITYPE.EQ.2) THEN
C INITIALIZE JBUF85 ARRAY AS MISSING
         JBUF85 = 99999
C MOVE SATELLITE ID, SCAN YEAR, MONTH, DAY, HOUR, MINUTE, SECOND,
C      SCAN NUMBER, ORBIT NUMBER, SPARES (SET TO 88888) INTO JBUF85
         JBUF85(1:12) = JBUFTN(1:12)
C INITIALIZE COUNTER FOR LOCATION IN ARRAY IDT85
         IHD = 0
      ENDIF
      ISBIT = ISBIT + (ILNGT1 * 16)
      ISBIT = ISBIT + (3 * 16) - (IKY(ITYPE) * 16)
C JBUFTN(13)-(1036) HOLD 16 PIECES OF INFO. FOR EACH OF THE 64 RETR.
C**********************************************************************
C**********************************************************************
      DO 88  ISC = 1,64
         ISBIT = ISBIT + (IKY(ITYPE) * 16)
         ISWRD = ISBIT/64 + 1
         IADON = MOD(ISBIT,64)
         CALL GBYTESC(CBUFF(ISWRD),KDATA(1), 0+IADON,16,0,2)
C SCALE THE LATITUDE AS *100 DEGREES (+ : NORTH, - : SOUTH)
         IF(KDATA(1).LT.18001)  IDATA(1,ISC) = KDATA(1) - 9000
C SCALE THE LONGITUDE AS *100 DEGREES (EAST) (NO CHANGE)
         IF(KDATA(2).LT.36001)  IDATA(2,ISC) = KDATA(2)
C CHECK FOR BAD LATITUDE
         IF(IDATA(1,ISC).LT.-9000.OR.IDATA(1,ISC).GT.9000)  THEN
            LAERR = LAERR + 1
            PRINT 777,ISC,IREC,KDATA(1),(IDATA(III,ISC),III=1,2)
  777 FORMAT(' ##SSMIUNPK: BAD LAT: RETR.',I3,', SCAN',I6,
     $ '; INPUT LAT=',I7,', IDATA LAT/LON=',2I7,', - SET ALL DATA ',
     $ 'MISSING & GET NEXT RETR.')
            GO TO 88
         END IF
C CHECK FOR BAD LONGITUDE
         IF(IDATA(2,ISC).LT.0.OR.IDATA(2,ISC).GT.36000)  THEN
            LOERR = LOERR + 1
            PRINT 778,ISC,IREC,KDATA(2),(IDATA(III,ISC),III=1,2)
  778 FORMAT(' ##SSMIUNPK: BAD LON: RETR.',I3,', SCAN',I6,
     $ '; INPUT LON=',I7,', IDATA LAT/LON=',2I7,', - SET ALL DATA ',
     $ 'MISSING & GET NEXT RETR.')
            GO TO 88
         END IF
C***********************************************************************
C                PRODUCTS IN EDR FILES COME HERE
C***********************************************************************
         IF(ITYPE.EQ.1)  THEN
            CALL GBYTESC(CBUFF(ISWRD),KDATA(3),  32+IADON, 8,0,14)
C SET THE SURFACE TAG (0-1,3-6) (NO CHANGE)
            IF(KDATA(3).LT.254)  IDATA(3,ISC) = KDATA(3)
C SCALE THE CLOUD WATER AS *100 KG/M**2
         IF(KDATA(4).LT.254)IDATA(4,ISC)=NINT((REAL(KDATA(4))/20.)*100.)
C SET THE SPARE TO 88888
            IDATA(5,ISC) = 88888
C SCALE THE RAIN RATE AS MM/HOUR 
            IF(KDATA(6).LT.254)  IDATA(6,ISC) = NINT(0.2*REAL(KDATA(6)))
C SCALE THE WIND SPEED AS *10 M/SEC (NO CHANGE)
            IF(KDATA(7).LT.254)  IDATA(7,ISC) = KDATA(7)
C SCALE THE SOIL MOISTURE AS MM (NO CHANGE)
            IF(KDATA(8).LT.254)  IDATA(8,ISC) = KDATA(8)
C SCALE THE SEA ICE CONCENTRATION AS PERCENT
            IF(KDATA(9).LT.254)  IDATA(9,ISC) = KDATA(9) * 5
C SET THE SEA ICE AGE (0,1) (NO CHANGE)
            IF(KDATA(10).LT.254)  IDATA(10,ISC) = KDATA(10)
C SET THE ICE EDGE (0,1) (NO CHANGE)
            IF(KDATA(11).LT.254)  IDATA(11,ISC) = KDATA(11)
C SCALE THE WATER VAPOR AS *10 KG/M**2
C  (THIS IS ALSO TOTAL PRECIPITABLE WATER SCALED AS *10 MM)
        IF(KDATA(12).LT.254)IDATA(12,ISC)=NINT((REAL(KDATA(12))/2.)*10.)
C SCALE THE SURFACE TEMPERATURE AS DEGREES KELVIN
            IF(KDATA(13).LT.254)  IDATA(13,ISC) = KDATA(13) + 180
C SCALE THE SNOW DEPTH AS MM
            IF(KDATA(14).LT.254)  IDATA(14,ISC) = KDATA(14) * 5
C SET THE RAIN FLAG (0-3) (NO CHANGE)
            IF(KDATA(15).LT.254)  IDATA(15,ISC) = KDATA(15)
C SET THE CALCULATED SURFACE TYPE (1-20) (NO CHANGE)
            IF(KDATA(16).LT.254)  IDATA(16,ISC) = KDATA(16)
         ELSE
C***********************************************************************
C             BRIGHTNESS TEMPERATURES IN SDR FILES COME HERE
C***********************************************************************
            CALL GBYTESC(CBUFF(ISWRD),KDATA(3),  32+IADON,16,0, 7)
            CALL GBYTESC(CBUFF(ISWRD),KDATA(10),144+IADON, 8,0, 2)
C SCALE 7 BRIGHTNESS TEMPERATURES AS *100 DEGREES KELVIN (NO CHANGE)
C  -- CHANNELS ARE IN THIS ORDER: 19 GHZ V, 19 GHZ H, 22 GHZ V,
C  -                    37 GHZ V, 37 GHZ H, 85 GHZ V, 85 GHZ H
            DO 77  IT = 3,9
               IF(ITYPE.EQ.2)  THEN
                  IDATA(IT,ISC) = KDATA(IT)
               ELSE
                  IF((IT.NE.4.AND.KDATA(IT).LT.10000).OR.
     $               (IT.EQ.4.AND.KDATA(IT).LT. 8000))  THEN
                     LBTER(IT) = LBTER(IT) + 1
                    PRINT 779,IT-2,ISC,IREC,(KDATA(III),III=3,9)
  779 FORMAT(' ##SSMIUNPK: BT, CHN',I2,' BAD: RETR.',I3,', SCAN',
     $ I6,'; BT:',7I6,' - SET ALL DATA MISSING & GET NEXT RETR.')
                     GO TO 88
                  END IF
               END IF
   77       CONTINUE
C SET THE SURFACE TAG (0-1,3-6) (NO CHANGE)
C SET THE POSITION NUMBER (NO CHANGE)
            IDATA(10,ISC) = KDATA(10)
            IDATA(11,ISC) = KDATA(11)
C THE LAST 5 WORDS ARE SPARES (SET TO 88888)
            IDATA(12:16,ISC) = 88888
C BEGIN HIGH-DENSITY 85 GHz CHANNEL DATA
            IF(ITYPE.EQ.2) THEN
               IADON = IADON + 80
               DO 81 IT = 1,3
                  IADON = IADON + 80
                  IHD = IHD + 1
                  CALL GBYTESC(CBUFF(ISWRD),KDT85(1), 0+IADON,16,0,2)
C SCALE THE LATITUDE AS *100 DEGREES (+ : NORTH, - : SOUTH)
                  IF(KDT85(1).LT.18001)  IDT85(1,IHD) = KDT85(1) - 9000
C SCALE THE LONGITUDE AS *100 DEGREES (EAST) (NO CHANGE)
                  IF(KDT85(2).LT.36001)  IDT85(2,IHD) = KDT85(2)
C CHECK FOR BAD LATITUDE
                  IF(IDT85(1,IHD).LT.-9000.OR.IDT85(1,IHD).GT.9000) THEN
                     LAERR = LAERR + 1
                     PRINT 775,IHD,IREC,KDT85(1),
     $                         (IDT85(III,IHD),III=1,2)
  775    FORMAT(' ##SSMIUNPK: BAD LAT: RETR.',I3,', SCAN',I6,
     $    '; INPUT LAT=',I7,', IDT85 LAT/LON=',2I7,', - SET ALL DATA ',
     $    'MISSING & GET NEXT RETR.')
                     GO TO 81
                  END IF
C CHECK FOR BAD LONGITUDE
                  IF(IDT85(2,IHD).LT.0.OR.IDT85(2,IHD).GT.36000)  THEN
                     LOERR = LOERR + 1
                     PRINT 776,IHD,IREC,KDT85(2),
     $                         (IDT85(III,IHD),III=1,2)
  776    FORMAT(' ##SSMIUNPK: BAD LON: RETR.',I3,', SCAN',I6,
     $    '; INPUT LON=',I7,', IDT85 LAT/LON=',2I7,', - SET ALL DATA ',
     $    'MISSING & GET NEXT RETR.')
                     GO TO 81
                  END IF
                  CALL GBYTESC(CBUFF(ISWRD),KDT85(3), 32+IADON,16,0, 2)
                  CALL GBYTESC(CBUFF(ISWRD),KDT85(5), 64+IADON, 8,0, 2)
C SCALE 2 BRIGHTNESS TEMPERATURES AS *100 DEGREES KELVIN (NO CHANGE)
C  -- CHANNELS ARE IN THIS ORDER: 85 GHZ V, 85 GHZ H
                  IDT85(3,IHD) = KDT85(3)
                  IDT85(4,IHD) = KDT85(4)
C SET THE SURFACE TAG (0-1,3-6) (NO CHANGE)
C SET THE POSITION NUMBER (NO CHANGE)
                  IDT85(5,IHD) = KDT85(5)
                  IDT85(6,IHD) = KDT85(6)
C THE LAST 2 WORDS ARE SPARES (SET TO 88888)
                  IDT85(7,IHD) = 88888
                  IDT85(8,IHD) = 88888
   81          CONTINUE
            ENDIF
         END IF
   88 CONTINUE
C**********************************************************************
C**********************************************************************
C TRANSFER JBUFTN ARRAY TO IBUFTN ARRAY (OUTPUT ARGUMENT)
      IBUFTN = JBUFTN
C TRANSFER JBUF85 ARRAY TO IBUF85 ARRAY (OUTPUT ARGUMENT)
      IF(ITYPE.EQ.2) IBUF85 = JBUF85
      NRECN = IREC
      KOUNT = KOUNT + 1
      KNTTOT = KNTTOT + 1
      IER = 0
C RETURN TO CALLING PROGRAM
      RETURN
C***********************************************************************
 1200 CONTINUE
C ALL FINISHED FOR THIS SATELLITE AND ITYPE, NO OTHER SCANS W/I DESIRED
C  TIME RANGE -- SET KOUNT NEGATIVE AND RETURN TO CALLING PROGRAM
      KOUNT = - KOUNT
      PRINT 124, ITYPE,KNTTOT,KNTDIR,KNTEMP,KNTUNP,KNTTSC,
     $           LAERR,LOERR
  124 FORMAT(/' SSMIUNPK: +++++ ALL CALLS TO SSMIUNPK HAVE BEEN ',
     $ 'MADE FOR THIS SATELLITE WITH ITYPE =',I2//35X,
     $ '** SSMIUNPK: SUMMARY FOR ALL SATELLITES AND ITYPES ',
     $ 'PROCESSED THUS FAR **'//
     $ 35X,'TOTAL NUMBER OF SCANS PROCESSED:                       ',I7/
     $ 35X,'NUMBER OF DIRECTORY (HEADER) RECORDS PROCESSED:        ',I7/
     $ 35X,'NUMBER OF SCANS SKIPPED DUE TO BEING EMPTY (ZEROED):   ',I7/
     $ 35X,'NUMBER OF SCANS SKIPPED DUE TO BEING UNPROCESSABLE:    ',I7/
     $ 35X,'NUMBER OF SCANS SKIPPED DUE TO BEING OUTSIDE SCAN TIME:',I7/
     $/35X,'NUMBER OF DATUM WITH LATITUDE OUT OF RANGE:            ',I7/
     $ 35X,'NUMBER OF DATUM WITH LONGITUDE OUT OF RANGE:           ',I7)
      IER = 0
      RETURN
C-----------------------------------------------------------------------
 1001 CONTINUE
C ERROR READING IN A DIRECTORY (HEADER) OR DATA RECORD -- SET IER TO 3
C  AND RETURN
      PRINT 3012, IREC
 3012 FORMAT(/' ##SSMIUNPK: ERROR READING IN DIRECTORY OR DATA RECORD',
     $ I7,' -- NO MORE SCANS CAN BE PROCESSED -- IER = 3'/)
      IER = 3
      NRECN = 0
      KOUNT = - KOUNT
      RETURN
C-----------------------------------------------------------------------
 2001 CONTINUE
C PROCESSING ERROR IN TIME IN DIRECTORY (HEADER) RECORD -- SET IER TO 2
C  AND RETURN
      PRINT 3014, IREC
 3014 FORMAT(/' ##SSMIUNPK: PROCESSING ERROR IN TIME IN DIRECTORY ',
     $ 'RECORD',I7,' -- NO MORE SCANS CAN BE PROCESSED -- IER = 2'/)
      IER = 2
      NRECN = 0
      KOUNT = - KOUNT
      RETURN
C-----------------------------------------------------------------------
 1201 CONTINUE
      PRINT 209, ITYPE
  209 FORMAT(/' SSMIUNPK: ALL VALID SCANS UNPACKED FOR THIS ',
     $ 'SATELLITE WITH ITYPE =',I2,' -- DONE'/)
      GO TO 1200
      END
