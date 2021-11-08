C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM:  BUFR_TRANOMPSTC
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2014-08-25
C
C ABSTRACT: READS IN BUFR MESSAGES AND SUBSETS FROM A RAW NPP OMPS-TC
C           (OZONE) BUFR FILE AND WRITES THEM BACK TO A BUFR FILE
C           WHICH IS IN THE PROPER FORM FOR DATABASING BY THE
C           BUFR_TRANJB PROGRAM.  THIS PROGRAM HANDLES OLD AND NEW
C           FORMATS OF RAW FILES (NEW FORMAT INCLUDES LAT/LON CORNER
C           INFORMATION AT THE BEGINNING OF EACH BUFR SUBSET.)
C
C PROGRAM HISTORY LOG:
C 2014-08-25  D. KEYSER - ORIGINAL AUTHOR
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT. W3TRNARG PARSES ARGUMENTS FROM
C              - STANDARD INPUT.
C     UNIT 11  - RAW NPP OMPS-TC (OZONE) BUFR FILE
C     UNIT 19  - FOREIGN BUFR TABLE FILE CONTAINING BUFR TABLES A,
C              - B, AND D (FOR READING UNIT 11).
C     UNIT 20  - BUFR TABLE FILE CONTAINING BUFR TABLES A, B, AND
C                D (FOR WRITING UNIT 51).
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 51  - OUTPUT BUFR FILE IN PROPER FORM FOR BUFR_TRANJB WHICH
C                WILL APPEND THE BUFR MESSAGES INTO THE PROPER DATABASE
C                TANKS.
C
C   SUBPROGRAMS CALLED:
C     SYSTEM:  - GETENV
C     LIBRARY:
C       W3NCO  - W3TRNARG W3TAGB   W3TAGE   ERREXIT
C     BUFRLIB  - OPENBF   CLOSBF   OPENMB   UFBSEQ   WRITCP   IREADMG
C              - IREADSB  DATELEN  WRDLEN   RDMSGW   IUPBS01  PKVS01
C              - GETBMISS
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =   1 - UNABLE TO PARSE INPUT ARGUMENTS IN W3TRNARG
C          = 253 - NO REPORTS WRITTEN OUT
C
C   REMARKS:
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      PROGRAM BUFR_TRANOMPSTC

      PARAMETER (NDAT=59)

      REAL(8)       BUFRI(NDAT)
      REAL(8)       BUFRO(NDAT)

      DIMENSION     MBAY(15000)

      CHARACTER*8   SUBSET,TLFLAG,SUBFGN
      CHARACTER*80  APPCHR,SUBDIR,TANKID
      CHARACTER*500 FORGNTABLE
      
      DATA LUNIN /11/
      DATA LINDX /19/
      DATA LUNDX /20/
      DATA LUNOT /51/

C-----------------------------------------------------------------------
      CALL W3TAGB('BUFR_TRANOMPSTC',2014,0237,0081,'NP22')
      PRINT *, ' '
      PRINT *, ' ==> Welcome to BUFR_TRANOMPSTC -- Version 08/25/2014'
      PRINT *, ' '
      CALL W3TRNARG(SUBDIR,LSUBDR,TANKID,LTNKID,APPCHR,LAPCHR,
     1              TLFLAG,JDATE,KDATE,IERR)
      IF(IERR.NE.0) THEN
        WRITE(6,'('' UNABLE TO PARSE ARGS TO TRANSLATION ROUTINE - '',
     1            '' RETURN CODE = '',I5)') IERR
        CALL W3TAGE('BUFR_TRANOMPSTC')
        CALL ERREXIT(IERR)
      ENDIF
      SUBSET = 'NC'//SUBDIR(LSUBDR-2:LSUBDR)//TANKID(LTNKID-2:LTNKID)
ccccc print *, 'SUBSET = ',SUBSET
C-----------------------------------------------------------------------

C  Obtain BUFRLIB value for missing
C  --------------------------------

      BMISS = GETBMISS()
ccccc print *, 'bmiss returned as ',bmiss

      IRD = 0
      IWT = 0

      CALL DATELEN(10)

C  Temporarily open input BUFR file to obtain BUFR message subtype.
C  This will determine if the raw file is in the old format (sutype 16)
C  or in the new format (subtype 18). (New format includes lat/lon
C  corner information at beginning of each BUFR subset.)
C  --------------------------------------------------------------------

      CALL OPENBF(LUNIN,'INX',LUNIN)
      CALL WRDLEN
      CALL RDMSGW(LUNIN,MBAY,IER)
      IF(IER.NE.0)  then
         PRINT *
         PRINT *, '=====> Error reading unit ',LUNIN
         PRINT *
         CALL ERREXIT(253)
      ENDIf

      IEDTN = IUPBS01(MBAY,'BEN')
      PRINT *
      PRINT *, 'Input BUFR messages are edition ',IEDTN
      PRINT *
      CALL PKVS01('BEN',IEDTN)
      PRINT *, 'Output BUFR messages forced to be edition ',IEDTN
      PRINT *

      MSBT = IUPBS01(MBAY,'MSBT')
      PRINT *
      PRINT *, 'Input BUFR messages have message subtype ',MSBT
      PRINT *

      CALL CLOSBF(LUNIN)

C  OPEN AND READ THRU THE INPUT BUFR FILE
C  --------------------------------------

      CALL GETENV('forgntable',FORGNTABLE)
ccccc print *, 'FORGNTABLE = ',trim(FORGNTABLE)

C  If this is old format input file (w/o lat/lon corner info), override
C  foreign BUFR mnemonic table location connected to unit LINDX in
C  executing script {that one applies to new format input file (w/ lat/
C  lon corner info)}
C  --------------------------------------------------------------------

      IF(MSBT.EQ.16)  OPEN(LINDX,FILE=trim(FORGNTABLE)//'.no_corners')
ccccc print *, 'prepare to call openbf'
      CALL OPENBF(LUNIN,'IN',LINDX)
ccccc print *, 'openbf unit ',lunin,', table ',lindx

ccccc CALL OPENBF(LUNOT,'OUT',LUNDX)
      CALL OPENBF(LUNOT,'NODX',LUNDX)

C  READ THROUGH THE MESSAGES/SUBSETS IN THE FILE
C  ---------------------------------------------

      DO WHILE(IREADMG(LUNIN,SUBFGN,IDATE).EQ.0)
         DO WHILE(IREADSB(LUNIN).EQ.0)
            CALL OPENMB(LUNOT,SUBSET,IDATE)

C  READ IN THE ENTIRE REPORT SEQUENCE
C  ----------------------------------

            BUFRI = BMISS
            CALL UFBSEQ(LUNIN,BUFRI,NDAT,1,IRET,SUBFGN)
ccccc       IF(IRD.EQ.0) print *, 'SUBFGN = ',SUBFGN
            IRD = IRD+1
ccccc       if(ird.eq.1) then
ccccc          do i = 1,ndat
ccccc             print *, 'i,bufri(i): ',i,bufri(i)
ccccc          enddo
ccccc       endif

C  TRANSFER SEQUENCE TO OUTPUT ARRAY
C  ---------------------------------

            BUFRO = BMISS
            IF(SUBFGN.EQ.'NC008018') THEN

C  New format raw file involves a simple 1:1 array transfer
C  --------------------------------------------------------

               BUFRO = BUFRI

            ELSE IF(SUBFGN.EQ.'NC008016') then

C  Old format raw file involves encoding "missing" into first 13 words
C  of output array (lat/lon corner infromation not available) follwed
C  by a simple 1:1 array transfer
C  -------------------------------------------------------------------

               BUFRO(14:NDAT) = BUFRI(1:NDAT-13)
            ELSE

               PRINT *
               PRINT *, '=====> Input BUFR message type not ',
     $                  'recognized,',SUBFGN
               PRINT *
               CALL ERREXIT(253)
            ENDIF
ccccc       if(ird.eq.1) then
ccccc          do i = 1,ndat
ccccc             print *, 'i,bufro(i): ',i,bufro(i)
ccccc          enddo
ccccc       endif
            
C  WRITE OUT THE COMPRESSED SUBSET
C  -------------------------------

            CALL UFBSEQ(LUNOT,BUFRO,NDAT,1,IRET,SUBSET)
            CALL WRITCP(LUNOT)
            IWT=IWT+1

         ENDDO
      ENDDO

C  WHEN FINISHED MAKE SURE ALL BUFFERS ARE FLUSHED THEN EXIT
C  ---------------------------------------------------------

      CALL CLOSBF(LUNIN)
      CALL CLOSBF(LUNOT)
      PRINT*,'*** PROCESSING ENDED NORMALLY ***'
      PRINT*,'*** READ :',IRD
      PRINT*,'*** WROT :',IWT
      PRINT*,'*** PROCESSING ENDED NORMALLY ***'
      IF(IWT.EQ.0) THEN
        WRITE(6,2003)
 2003   FORMAT(' NO REPORTS PROCESSED -- DISABLING ALL SUBSEQUENT ',
     1         'PROCESSING.')
        CALL W3TAGE('BUFR_TRANOMPSTC')
        CALL ERREXIT(253)
      ENDIF
      CALL W3TAGE('BUFR_TRANOMPSTC')

      STOP
      END

