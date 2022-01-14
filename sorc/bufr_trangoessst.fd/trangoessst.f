C-----------------------------------------------------------------------
      PROGRAM TRANGOESSST
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: TRANGOESSST  PACKS GOES SST REPORTS INTO BUFR FILE
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2014-01-20
C
C ABSTRACT: READS IN GOES SST REPORTS FROM THE ONCE-A-DAY FILES
C   AND PACKS THEM INTO A BUFR FILE.
C
C PROGRAM HISTORY LOG:
C 2004-11-12  BERT B. KATZ ,    NP2  
C 2007-10-25  BERT B. KATZ ,    NP2  CORRECTED RANGE OF LONGITUDES
C                                    FROM 180.0 DEG. AND UP TO 
C                                    -180.0 DEG. AND UP.
C 2012-11-13  DIANE C. STOKES,  NP2  MODIFIED TO RUN ON WCOSS 
C 2014-01-20  DENNIS A. KEYSER, NP2  NO LONGER ENCODES BUFR DICTIONARY TABLE
C                                    MESSAGES AT TOP OF OUTPUT FILE(S) FROM
C                                    UNIT 20  - WILL ENSURE THAT "OFFICIAL"
C                                    bufrtab.012 IS ALWAYS ENCODED INTO NCEP
C                                    BUFR TANK WHEN NEW TANK IS CREATED BY
C                                    BUFR_TRANJB
C 2014-12-16  DIANE C. STOKES,  NP2  MODIFIED DATE PARSING DUE TO NEW 
C                                    FILENAME PATTERN
C
C USAGE:
C
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT. W3TRNARG PARSES ARGUMENTS FROM
C              - STANDARD INPUT.
C     UNIT 11  - BINARY SST FILE
C     UNIT 12  - BINARY COUNT FILE. COUNTS VARY FROM 1 TO 24.
C     UNIT 20  - BUFR MNEMONIC TABLE
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT LUNOT
C              - OUTPUT FILE TO BE APPENDED TO BUFR TANK OF GOES
C              - SST DATA.
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       W3LIB  - W3TRNARG W3TAGB W3TAGE ERREXIT W3MOVDAT
C        BUFR  - OPENBF   CLOSBF   OPENMG  UFBINT  WRITSB
C       BACIO  - BAOPENR BAREAD
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =   1 - UNABLE TO PARSE INPUT ARGUMENTS IN W3TRNARG
C          =   2 - INVALID SUBDIRECTORY ARGUMENT INPUT TO W3TRNARG
C          =   6 - ENVIRONMENT VARIABLE FOR INPUT FILE DOES NOT EXIST
C          =   7 - INPUT FILENAME TOO LONG FOR VARIABLE LENGTH
C          =   8 - NON-SPECIFIC ERROR GETTING INPUT FILENAME
C          =   9 - UNEXPECTED STATUS GETTING INPUT FILENAME
C          =  11 - ERROR READING INPUT RECORD
C          =  22 - ERROR OPENING INPUT BINARY SST FILE
C          =  33 - ERROR PARSING DATE FROM FILE NAME
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP, IBM iDataPlex
C
C$$$
C-----------------------------------------------------------------------
                                                                        
      PARAMETER (LONS=3000,LATS=2100)
      CHARACTER*8  SUBSET                                   
                                                                        
      CHARACTER*12 SUBDIR,TANKID                                        
      CHARACTER*80 APPCHR                                               
      CHARACTER*8 TLFLAG                                                
                                                                        
      CHARACTER(120) CFILE
      CHARACTER(1) CDATA1(LONS,LATS),CDATA2(LONS,LATS)
      CHARACTER(80) IDST1

      character(7) envvar
      integer locuscor(5)
      integer idatin(8),idatout(8)
      real rdate(5)
                                                                        
      DIMENSION XDATA(10)
      REAL(8) XDATA
                                                                        
      DATA LUNDX /20/                                                   
      DATA LUNOT /51/                                                   
      
      DATA IDST1  /'YEAR MNTH DAYS HOUR MINU SECO CLAT CLON SST1 ACAV'/

      DATA BMISS  /10E10/
C
      CALL W3TAGB('BUFR_TRANGOESSST',2014,339,050,'NP2    ')
C-----------------------------------------------------------------------
      CALL W3TRNARG(SUBDIR,LSUBDR,TANKID,LTNKID,APPCHR,LAPCHR,          
     1              TLFLAG,JDATE,KDATE,IERR)                            
      IF(IERR.NE.0) THEN                                                
        WRITE(6,'('' UNABLE TO PARSE ARGS TO TRANSLATION ROUTINE - '',  
     1            '' RETURN CODE = '',I5)') IERR                        
        CALL ERREXIT(IERR)                                                 
      ENDIF                                                             
C-----------------------------------------------------------------------
      SUBSET = 'NC'//SUBDIR(LSUBDR-2:LSUBDR)//TANKID(LTNKID-2:LTNKID)
                                                                        
      IWT = 0                                                           

C VARIABLE ICLEAR IS USED BELOW FOR 'ACAV'. SINCE SOME TIME BEFORE SEP
C 2010, ICLEAR HAS BEEN UNDEFINED, AND 'ACAV' GETTING WRITTEN TO THE 
C BUFR FILE AS ZERO.  IT SEEMS 'ACAV' INFO WAS COLLECTED IN PAST 
C VERSIONS OF THIS CODE.  PERHAPS IT SHOULD BE "MISSING" OR EXCLUDED 
C HERE BUT FOR NOW, INITIALIZE TO ZERO TO BE CONSISTENT WITH WHAT USERS 
C HAVE BEEN SEEING.

      ICLEAR = 0
      
C  OPEN AND READ THRU THE INPUT BUFR FILE                               
C  --------------------------------------                               
                                                                        
ccccc CALL OPENBF(LUNOT,'OUT',LUNDX)
      CALL OPENBF(LUNOT,'NODX',LUNDX)
                                                                        
      NCHAR=LONS*LATS
      envvar='FORT11 '
      call get_environment_variable(envvar, CFILE, length, iestatus)
      select case(iestatus)
        case(0)
          continue
        case(1)
          print*,'environment variable ',trim(envvar),' does not exist'
          call errexit(6)
        case(-1)
          print*,'env variable ',trim(envvar),' is set to string of',
     1     length,' characters which does not fit in CFILE.'
          call errexit(7)
        case(3)
          print*,'non-specific error(s) from GET_ENVIRONMENT_VARIABLE'
          call errexit(8)
        case default
          print*,'unexpected status from GET_ENVIRONMENT_VARIABLE'
          call errexit(9)
      end select

C We are assuming here the filename ends with pattern YYYY_DDD
      read(CFILE(length-7:length-4),'(I4)',err=9990) IYR
      read(CFILE(length-2:length),'(I3)',err=9990) idayr

      CALL BAOPENR(11,CFILE,IRET)
      IF(IRET.NE.0) GO TO 9991

      idatin(1) = IYR
      idatin(2) = 1
      idatin(3) = 1
      idatin(4:8) = 0
      rdate(1) = real(idayr-1)
      rdate(2:5) = 0.0
      call w3movdat(rdate,idatin,idatout)
      MON = idatout(2)
      IDAY = idatout(3)
      IHR = 12
      MIN = 0
      ISEC = 0

C  CHECK FOR REPORT WITHIN THE TRANSLATION WINDOW                       
C  ----------------------------------------------                       
                                                                        
      IDATE = IYR*1000000 + MON*10000 + IDAY*100 + IHR                  
                                                                        
      IERR = 0
      IF(IYR.LT.0 .OR. IYR.GT.9999 .OR.                                 
     .   MON.LT.1 .OR. MON.GT.12 .OR.                                   
     .   IDAY.LT.1 .OR. IDAY.GT.31 .OR.                                 
     .   IHR.LT.0 .OR. IHR.GT.24 .OR.                                   
     .   MIN.LT.0 .OR. MIN.GT.60) THEN                                  
        PRINT '("BAD DATE:",I4.4,3I2.2," SUBSET:",A8)',                 
     .         IYR,MON,IDAY,IHR,SUBSET                                  
        IERR = 1                                                        
      ENDIF                                                             
C
      CALL BAREAD(11,0,NCHAR,NREAD,CDATA1)
      IF(NREAD.NE.NCHAR) GO TO 9993

      CALL OPENMG(LUNOT,SUBSET,IDATE)                      

      DO I=1,LATS
        DO J=1,LONS
          ICOUNT = MOVA2I(CDATA1(J,I))
          IF(ICOUNT.NE.0 .AND. ICOUNT.NE.2 .AND. ICOUNT.NE.4) THEN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C           EXPLANATION OF DATA IDENTIFYING COMMENTS BELOW
C
C STORAGE               MNEM  F XX YYY /INT OF  DESCRIPTIVE TEXT
C LOCATION              ONIC           /FXXYYY 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
             SSTEMP = 270.0 + 0.15 * REAL(ICOUNT)
C
             XDATA     = 10.E10
             K         = 0
C
C XDATA(1)              YEAR  0 04 001 /  1025  YEAR                    
C
             K         = K + 1
             XDATA(K) = IYR
C
C XDATA(2)              MNTH  0 04 002 /  1026  MONTH                   
C
             K         = K + 1
             XDATA(K)  = MON
C
C XDATA(3)              DAYS  0 04 003 /  1027  DAY                     
C
             K         = K + 1
             XDATA(K)  = IDAY
C
C XDATA(4)              HOUR  0 04 004 /  1028  HOUR                    
C
             K         = K + 1
             XDATA(K)  = IHR
C
C XDATA(5)              MINU  0 04 005 /  1029  MINUTE                  
C
             K         = K + 1
             XDATA(K)  = MIN
C
C XDATA(6)              SECO  0 04 006 /  1030  SECOND                  
C
             K         = K + 1
             XDATA(K)  = ISEC
C
C XDATA(7)              CLAT  0 05 002 /  1282  LATITUDE                
C
             K         = K + 1
             XDATA(K)  = 60.0 - 0.05 * FLOAT(I-1)
C
C XDATA(8)              CLON  0 06 002 /  1538  LONGITUDE               
C
             K         = K + 1
             XDATA(K)  = -180.0 + 0.05 * FLOAT(J-1)
C 
C XDATA(9)              SST1  0 22 042 /  5674  SEA SURFACE TEMP        
C 
             K         = K + 1
             XDATA(K)  = SSTEMP
C 
C XDATA(10)             ACAV  0 08 022 /  2070  TOTAL NO. WRT AVERAGE   
c
             K         = K + 1
             XDATA(K)  = REAL(ICLEAR)
C 
C 
C-----------------------------------------------------------------------
C  SUBROUTINE TRANSST WILL WRITE SATELLITE SST DATA INTO
C  THE BUFR DATABASE. LAYOUT OF THE ARRAY CONTAINING THE
C  SST REPORT IS AS FOLLOWS:
C
C XDATA(1)   =    YEAR  004001   YEAR                    
C XDATA(2)   =    MNTH  004002   MONTH                   
C XDATA(3)   =    DAYS  004003   DAY                     
C XDATA(4)   =    HOUR  004004   HOUR                    
C XDATA(5)   =    MINU  004005   MINUTE                  
C XDATA(6)   =    SECO  004006   SECOND                  
C XDATA(7)   =    CLAT  005002   LATITUDE                
C XDATA(8)   =    CLON  006002   LONGITUDE               
C XDATA(9)   =    SST1  022042   SEA SURFACE TEMP        
C XDATA(10)  =    ACAV  008022   TOTAL NO. WRT AVERAGE   
C
C-----------------------------------------------------------------------

C  GET THE RIGHT UNIT AND WRITE A SUBSET
C  -------------------------------------

             CALL UFBINT(LUNOT,XDATA,10,1,IRET,IDST1)
             CALL WRITSB(LUNOT)
             IWT = IWT + 1
          ENDIF
        ENDDO
      ENDDO

C  WHEN FINISHED MAKE SURE ALL BUFFERS ARE FLUSHED THEN EXIT            
C  ---------------------------------------------------------            
                                                                        
      CALL CLOSBF(LUNOT)                                                
      PRINT*,'*** PROCESSING ENDED NORMALLY ***'                        
      PRINT*,'*** WROTE:',IWT                                           
      PRINT*,'*** PROCESSING ENDED NORMALLY ***'                        
      STOP
 9990 WRITE(6,'('' UNABLE TO PARSE DATE FROM FILE NAME '')')
      CALL ERREXIT(33)
      CALL W3TAGE('BUFR_TRANGOESSST')
      STOP
 9993 WRITE(6,'('' UNABLE TO READ INPUT RECORD '')')
      CALL ERREXIT(11)
      CALL W3TAGE('BUFR_TRANGOESSST')
      STOP
 9991 WRITE(6,'('' UNABLE TO OPEN FILE '',A)') CFILE 
      CALL ERREXIT(22)
      CALL W3TAGE('BUFR_TRANGOESSST')
      STOP
      END                                                               
