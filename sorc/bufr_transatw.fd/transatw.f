C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_TRANSATW
C   PRGMMR: KEYSER/LING/MELCHIOR   ORG: NP22       DATE: 2019-04-02
C
C ABSTRACT: READS IN SATELLITE WIND REPORTS IN WMO BUFR FORMAT,
C   REFORMATS AND PACKS INTO A BUFR FILE WHICH CAN BE DATABASED BY
C   TRANJB. THIS CURRENTLY APPLIES TO MESSAGE TYPES NC005010-014,
C   NC005019, NC005050-051, NC005070-071, NC005080 AND NC005090.
C
C PROGRAM HISTORY LOG:
C 2000-05-08  X. SU      - ORIGINAL AUTHOR
C 2000-09-05  D. KEYSER  - REDUCED AMOUNT OF STDOUT PRINT; HARDWIRE THE
C     RECURSIVE FILTER FLAG (RFFL) AS MISSING FOR PICTURE TRIPLET WINDS
C     IN TYPE 005, SUBTYPE 013
C 2001-03-30  D. KEYSER  - NOW CHECKS YYYYMMDDHH OF EACH REPORT TO SEE
C     IF A NEW OUTPUT MESSAGE SHOULD BE OPENED WITH THIS DATE, ENSURES
C     THAT OUTPUT FILE BUFR MESSAGES CONTAIN ONLY REPORTS WITH SAME
C     YYYYMMDDHH AS MESSAGE (THIS IS A REDUNDANT CHECK FOR UNCOMPRESSED
C     FILES SINCE SUBSEQUENT BUFR_TRANJB PROGRAM ALSO DOES THIS, BUT
C     THIS CHECK DOESN'T COST ANY TIME AND IT WILL BE IMPORTANT IF
C     THESE FILES ARE EVER COMPRESSED); ADDED ERROR HANDLING WHEN NO
C     OUTPUT IS CREATED SO THAT SUBSEQUENT TRANJB'S ARE SKIPPED;
C     STREAMLINED CODE; TURNED OFF EXTRANEOUS PRINTOUT
C 2004-09-02  D. KEYSER -- MODIFIED TO PROCESS TERRA AND AQUA MODIS
C     WINDS WHICH HAVE SATELLITE ID VALUES OF 783 AND 784, RESP.
C     (FORMER GET "T" IN FIRST CHARACTER OF STNID, LATTER GET "U")
C 2005-03-14  D. KEYSER -- MODIFIED TO ENCODE TERRA AND AQUA MODIS
C     WINDS IN THE SAME BUFR FORMAT STRUCTURE AS THE GOES GTS WINDS -
C     THIS PROVIDES MORE QUALITY INFORMATION
C 2006-02-02  D. KEYSER - REPLACED CALL TO BUFRLIB ROUTINE IREADIBM
C     WITH CALL TO BUFRLIB ROUTINE IREADMG (IREADIBM OBSOLETE WITH
C     1/31/2006 VERSION OF BUFRLIB)
C 2006-02-02  S. BENDER - MODIFIED TO PROCESS GOES 3.9 um WINDS (ENCODE
C     IN THE SAME BUFR FORMAT STRUCTURE AS THE GOES GTS AND MODIS
C     WINDS)
C 2010-04-07  G. KRASOWSKI -  MODIFIED TO PROCESS NOAA-15 THROUGH -19
C     AND METOP-2 AVHRR WINDS WHICH HAVE SATELLITE ID VALUES OF 206-
C     209, 223 and 004, RESP.
C 2011-11-22  D. KEYSER   MODIFIED TO RECOGNIZE ALL CURRENT AND FUTURE
C     SATELLITES WHICH COULD PRODUCE SATELLITE-DERIVED WINDS (EVEN
C     THOSE TYPES NOT NORMALLY PROCESSED BY THIS PROGRAM); MODIFIED TO
C     RECOGNIZE INPUT "FOREIGN" BUFR TABLE WHICH NOW INCLUDES A FOURTH
C     REPLICATION OF QUALITY INFO ("PCCF") CONTAINING "EXPECTED ERROR"
C     (EE) (OR AT LEAST IS SET UP TO HOLD THIS IF IT IS AVAILABLE IN
C     THE INCOMING RAW FILES) (IN ADDITION TO RFF, QI WITH FORECAST AND
C     QI W/O FORECAST), AND TO POSSIBLY ENCODE THIS INTO AN OUTPUT BUFR
C     TABLE WITH A NEW FOURTH REPLICATION OF QUALITY INFO ("PCCF") FOR
C     MODIS WINDS IN NC005070 AND NC005071; MODIFIED TO NOW POSSIBLY
C     ENCODE ALL FOUR QUALITY INDICATORS ("PCCF") INTO OUTPUT BUFR
C     TABLE GOES MESSAGE TYPES NC005010, NC005011, NC005012 AND
C     NC005014 (THESE THEN LOOK LIKE THE MODIS WIND MESSAGES,
C     STRUCTURE-WISE - ALL ARE IN QUASI-NESDIS VERSION 10 BUFR WINDS
C     FORMAT) - IMPORTANT: THIS CODE WILL ALSO WORK WITH:
C          1) MESSAGE TYPES WHOSE "FOREIGN" BUFR TABLE DOES NOT INCLUDE
C             A PLACE HOLDER FOR EE (I.E., IT HAS ONLY THREE
C             REPLICATIONS OF QUALITY INFO) AND WHOSE OUTPUT BUFR TABLE
C             ALSO DOES NOT INCLUDE A PLACE HOLDER FOR EE (E.G., MODIS
C             IR & WVI WITH ONLY RFF, QI W/ FCST & QI W/O FCST, AND
C             GOES IR, WVI, VIZ & WVS WITH ONLY RFF - THE GOES OUPUT
C             BUFR SUBSETS NOT BEING IN VERSION 10 FORMAT)
C          2) MESSAGE TYPES WHOSE "FOREIGN" BUFR TABLE DOES HAVE EE (OR
C             AT LEAST IS SET UP TO HOLD THIS IF IT IS AVAILABLE) (I.E,
C             IT HAS FOUR REPLICATIONS OF QUALITY INFO) BUT WHOSE
C             OUTPUT BUFR TABLE DOES NOT INCLUDE A PLACE HOLDER FOR EE
C             (E.G., MODIS IR & WVI WITH ONLY RFF, QI W/ FCST & QI W/O
C             FCST, AND GOES IR, WVI, VIZ & WVS WITH ONLY RFF - THE
C             GOES OUTPUT BUFR SUBSETS NOT BEING IN VERSION 10 FORMAT)
C          3) MESSAGE TYPES WHOSE "FOREIGN" BUFR TABLE DOES NOT INCLUDE
C             A PLACE HOLDER FOR EE (I.E., IT HAS ONLY THREE
C             REPLICATIONS OF QUALITY INFO) BUT WHOSE OUTPUT BUFR TABLE
C             DOES INCLUDE A PLACE HOLDER FOR  EE (E.G., AVHRR IR FROM
C             UW/CIMSS) (EE WILL BE MISSING IN OUTPUT)
C      {what I am trying to say here is that this code will work with
C       input and output BUFR tables as they appeared through November
C       2011; it will also work with input BUFR tables that include a
C       placeholder for EE (but with no EE in raw data) and output BUFR
C       tables as they appeared through November 2011; and it will work
C       with input BUFR tables modified to add EE for one or more (or
C       all types) but output BUFR tables not yet modified to handle EE
C       for any types; and it will work with input BUFR tables not yet
C       modified to handle EE for some (or all) types but output BUFR
C       tables modified to add EE for the same types (EE missing in
C       output here); and finally it will work for input and output
C       BUFR tables both modified to add EE, where the GOES message
C       types are also modified to be BUFR V10 format (looking like the
C       MODIS and AVHRR winds structure-wise)
C 2012-01-03  D. KEYSER   CORRECTED LOGIC TO PROPERLY IDENTIFY CASE
C     WHEN INPUT FILE HAS 4 PCCF REPLICATIONS BUT THE 4'TH (EE) IS NOT
C     POPULATED (BEFORE, EE COULD SOMETIMES BE ENCODED AS 0 RATHER THAN
C     AS MISSING IN AN OUTPUT FILE ALSO WITH 4 PCCF REPLICATIONS)
C 2012-11-16  Y. Ling/D. Keyser  Changes to run on WCOSS (e.g.,
C     replaced all real declarations with real(8) for types in ufbxxx
C     calls).
C 2014-01-20  D. KEYSER   MINOR CHANGES.
C 2015-01-21  Y. LING -   MODIFIED TO RECOGNIZE NPP VIIRS POLAR WINDS
C     IN MESSAGE TYPE NC005090 WHICH HAVE BUFR SATELLITE ID 224 (GET
C     "J" IN FIRST CHARACTER OF STNID).
C 2019-04-02  S. MELCHIOR -   MODIFIED TO RECOGNIZE NOAA-20 VIIRS
C     POLAR WINDS IN MESSAGE TYPE NC005090 WHICH HAVE BUFR SATELLITE ID
C     225 (GET "S" IN FIRST CHARACTER OF STNID).
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT. W3TRNARG PARSES ARGUMENTS FROM
C              - STANDARD INPUT.
C     UNIT 11  - WMO BUFR FILE.
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
C     UNIQUE   - SCAN_BUFRTABLE
C     LIBRARY:
C      SYSTEM  - INQUIRE
C       W3NCO  - W3TRNARG W3TAGB   W3TAGE   ERREXIT
C     BUFRLIB  - OPENBF   CLOSBF   OPENMB   UFBINT   UFBREP   WRITSB
C              - IREADMG  IREADSB  DATELEN  UPFTBV   IBFMS    DIGIT
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =   1 - UNABLE TO PARSE INPUT ARGUMENTS IN W3TRNARG
C          =  44 - ERROR SCANNING OUTPUT BUFR TABLE
C          = 253 - NO REPORTS WRITTEN OUT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      PROGRAM BUFR_TRANSATW

      REAL(8),DIMENSION(1)    :: TCMD,SWQM,RSTNID,SWDL,CORN,TPHR
      REAL(8),DIMENSION(2)    :: XLALO
      REAL(8),DIMENSION(3)    :: YMDAY
      REAL(8),DIMENSION(4)    :: GNAPIN,TSIGOUT
      REAL(8),DIMENSION(5)    :: GCLONG,HOUROUT
      REAL(8),DIMENSION(6)    :: RCPDAT
      REAL(8),DIMENSION(8)    :: PCCFOUT,GNAPOUT
      REAL(8),DIMENSION(9)    :: OGCEOUT
      REAL(8),DIMENSION(10)   :: HOURIN,TSIGIN,TMDBST
      REAL(8),DIMENSION(11)   :: PRLC,HAMD
      REAL(8),DIMENSION(12)   :: SATINFO
      REAL(8),DIMENSION(40)   :: PCCFIN
      REAL(8),DIMENSION(2,5)  :: WINDSQ,TMISCOUT
      REAL(8),DIMENSION(2,9)  :: TMISCIN

      INTEGER,DIMENSION(7) :: ISWDL
      INTEGER,DIMENSION(8) :: IDAT
      INTEGER,DIMENSION(31):: IBIT

      LOGICAL V10_BUFR

      CHARACTER*1   STNID1(784),STNID6(7),CDUMMY
      CHARACTER*8   SUBSET,TLFLAG,STNID,SUBFGN
      CHARACTER*80  APPCHR,SUBDIR,TANKID
      CHARACTER*500 FILENAME

      EQUIVALENCE (STNID,RSTNID)

      DATA LUNIN  /11/
      DATA LINDX  /19/
      DATA LUNDX  /20/
      DATA LUNOT  /51/
      DATA BMISS  /10E10/
      DATA MINJAP /150/
      DATA MAXJAP /152/

      DATA IDATE_prev/-99/,LDATE_prev/-99/

C  First character of report id
C  ----------------------------

      DATA STNID1   ! no unique characters left - must recycle old ones 

C                 **  METOP **
C ---- spare(1-2) 3   4   5
     $/    2*'?','H','E','I'

C ---- spare(6-49)
     $,   44* '?'

C                 **  Meteosat  **
C ---- 50  51  52  53  54  55  56  57  58  59 sp(60-69) 70 sp(71-98) 99
     $,'Z','W','X','Y','Z','W','X','Y','Z','W',10* '?', 'Z',28* '?', 'X'

C ---- spare(100-149)
     $,   50* '?'

C                    **  GMS/MTSAT **
C ---- 150 151 152 sp(153-170) 171 172 173 174 175 176 sp(177-198) 199
     $,'R','O','P', 18* '?',   'Q','R','O','P','Q','R', 22* '?',   'Q'

C                    ** NOAA **
C ---- spare(200-205) 206 207 208 209 spare(210-222) 223
     $,    6* '?',    'F','L','M','N',   13* '?',    'G'

C                    ** NPP/NOAA **
C ---- 224 225
     $,'J','S'

C ---- spare(226-249)
     $,   24* '?'

C                    **  GOES **
C ---- 250 251 252 253 254 255 256 257 258 259
     $,'D','A','B','C','D','A','B','C','D','A'


C ---- spare(260-439)
     $,  180* '?'

C                   **  Kalpana **
C ---- 440
     $,'K'

C ---- spare(441-469)
     $,   29* '?'

C                   **  Insat **
C ---- 470 spare(471-498) 499
     $,'V',   28* '?',    'V'

C ---- spare(500-782)
     $,  283* '?'

C                   **  Terra **
C ---- 783
     $,'T'

C                    ** Aqua **
C ---- 784
     $,'U' /


C  Sixth character of report id, deep-layer switch
C   -> Type:             IR     VIS   WV-CT  PTRIP  WV-DL         WV-? 
C                      (LW,SW)
      DATA  STNID6    /  'I',   'Z',   'W',   'P',   'W',   '?',   'W' /
      DATA  ISWDL     /   2 ,    2 ,    2 , 99999,    1 , 99999, 99999 /

C-----------------------------------------------------------------------

      CALL W3TAGB('BUFR_TRANSATW',2019,0092,0073,'NP22') 

      PRINT *, ' '
      PRINT *, ' ==> Welcome to BUFR_TRANSATW -- Version 04/02/2019'
      PRINT *, ' '

      CALL W3TRNARG(SUBDIR,LSUBDR,TANKID,LTNKID,APPCHR,LAPCHR,
     .              TLFLAG,JDATE,KDATE,IERR)
ccccc print *, 'LSUBDR=',LSUBDR
ccccc print *, 'LTNKID=',LTNKID
ccccc print *, 'SUBDIR=',SUBDIR(LSUBDR-2:LSUBDR)
ccccc print *, 'TANKID=',TANKID(LTNKID-2:LTNKID)
      IF(IERR.NE.0) THEN
         WRITE(6,'('' UNABLE TO PARSE ARGS TO TRANSLATION ROUTINE - '',
     .             '' RETURN CODE = '',I5)') IERR
         CALL W3TAGE('BUFR_TRANSATW')
         CALL ERREXIT(IERR)
      ENDIF
      SUBSET = 'NC'//SUBDIR(LSUBDR-2:LSUBDR)//TANKID(LTNKID-2:LTNKID)
ccccc print *, 'SUBSET=',SUBSET
C-----------------------------------------------------------------------

C  Determine how many replications of quality information are specified
C   in the output BUFR table for this message type (subset) - this
C   will determine if the output BUFR table is in quasi-Version 10 BUFR
C   winds format for this message type AND, if it is in Version 10, how
C   many pieces of quality information are present for this message
C   type (or at least how many place holders for quality information
C   are present for this message type)
C   NOTE: All subsets are INPUT in true NESDIS Version 10 BUFR WINDS
C         format (although the number of replications of quality
C         information for this message type in the INPUT BUFR table is
C         not yet known)
C  --------------------------------------------------------------------

      call scan_bufrtable(lundx,subset,ireps)
      IF(ireps.eq.0) then
         print'(" This message type (",A,") is NOT specified in quasi-",
     .    "Version 10 BUFR winds format in output BUFR table")', SUBSET
         print *
         V10_BUFR = .FALSE.
      ELSE
         print'(" This message type (",A,") IS specified in quasi-",
     .    "Version 10 BUFR winds format in output BUFR table")', SUBSET
         print'(" -- number of pieces of quality information in output",
     .    " BUFR table = ",I0)',ireps
         print *
         V10_BUFR = .TRUE.
      ENDIF

      IRD = 0
      IWT = 0
      KTSKPT=0

      CALL DATELEN(10)

C  OPEN AND READ THRU THE INPUT BUFR FILE                               
C  --------------------------------------                               

      CALL OPENBF(LUNIN,'IN',LINDX)
ccccc CALL OPENBF(LUNOT,'OUT',LUNDX)
      CALL OPENBF(LUNOT,'NODX',LUNDX)

      INQUIRE(LINDX,NAME=FILENAME)
      PRINT'(" Table used to read input files: ",A)', trim(FILENAME)

      INQUIRE(LUNDX,NAME=FILENAME)
      PRINT'(" Table used to write output files: ",A)', trim(FILENAME)

C  -------------------------------------------------------------------

C  READ THROUGH THE MESSAGES/SUBSETS IN THE FILE                        
C  ---------------------------------------------                        

      DO WHILE(IREADMG(LUNIN,SUBFGN,IDATE).EQ.0)
ccccc    print *,' IDATE ',idate
ccccc    print *,' SUBFGN IS ',SUBFGN
         IF(IDATE.NE.IDATE_prev)  then
            print *, ' '
            print *, 'OPENING INPUT  MESSAGE WITH NEW DATE ',IDATE,
     .       ' (SUBSET ',SUBFGN,')'
            print *, ' '
         ENDIF
         IDATE_prev = IDATE
         DO WHILE(IREADSB(LUNIN).EQ.0) 

C  READ THE INTERNAL DATE AND CHECK FOR REALISM                         
C  --------------------------------------------                         

            CALL UFBINT(LUNIN,YMDAY,3,1,IRET,'YEAR MNTH DAYS') 
            CALL UFBREP(LUNIN,HOURIN,1,10,IRET,'HOUR')
            CALL UFBREP(LUNIN,TMISCIN,2,9,IRET,'MINU SECO') 
            IYR  = NINT(YMDAY(1))
            MON  = NINT(YMDAY(2))
            IDAY = NINT(YMDAY(3))
            IHR  = NINT(HOURIN(1))
            MIN  = NINT(TMISCIN(1,1))
            ISEC = NINT(TMISCIN(2,1))
            IRD = IRD+1

            IF(IYR .LT.0 .OR.
     .         MON .LT.1 .OR. MON .GT.12 .OR.
     .         IDAY.LT.1 .OR. IDAY.GT.31 .OR.
     .         IHR .LT.0 .OR. IHR .GT.24 .OR.
     .         MIN .LT.0 .OR. MIN .GT.60 .OR. 
     .         ISEC.LT.0 .OR. ISEC.GT.60) THEN
               PRINT '(" BAD DATE:",I4,5I2.2," SUBSET:",A8)',
     .          IYR,MON,IDAY,IHR,MIN,ISEC,SUBFGN
               KTSKPT=KTSKPT+1
            ELSE
               CALL UFBINT(LUNIN,SATINFO,12,1,IRET,
     .    'SAID SCLF SSNX SSNY SWCM SIDP SCCF CCST LSQL SAZA SCBW OFGI')
               CALL UFBINT(LUNIN,XLALO,2,1,IRET,'CLATH CLONH')
               CALL UFBREP(LUNIN,WINDSQ,2,5,IRET,'WDIR WSPD')
               CALL UFBINT(LUNIN,TCMD,1,1,IRET,'TCMD')
               CALL UFBREP(LUNIN,GCLONG,1,5,IRET,'GCLONG')
               CALL UFBREP(LUNIN,PRLC,1,11,IRET,'PRLC')
               CALL UFBREP(LUNIN,HAMD,1,11,IRET,'HAMD')
               CALL UFBREP(LUNIN,TMDBST,1,10,IRET,'TMDBST')

C  iret_PCCF will either be 30 or 40: 30 if EE quality indicator is
C   NOT in this message type in input BUFR data from NESDIS (i.e.,
C   only 3 replications of quality information), 40 if EE quality
C   indicator IS in this message type in input BUFR data from NESDIS
C   (i.e., 4 replications of quality information) (Note: the presence
C    of the EE quality indicator does not necessarily mean it is filled
C    with EE value here - that depends on if EE is present in the raw
C    files being read in)
C  --------------------------------------------------------------------

               CALL UFBREP(LUNIN,PCCFIN,1,40,iret_PCCF,'PCCF')

               IF(IRD .EQ. 1) THEN
                  if(iret_PCCF.eq.40) then
                     print'(" This message type contains 4 ",
     .                "replications of quality information -- RFF, QI ",
     .                "w/ fcst, QI w/o fcst, EE --"/" in input ",
     .                """foreign"" BUFR table (in Version 10 winds ",
     .                "format)")'
                  else  if(iret_PCCF.eq.30) then
                     print'(" This message type contains 3 ",
     .                "replications of quality information -- RFF, QI ",
     .                "w/ fcst, QI w/o fcst --"/" in input ""foreign""",
     .                " BUFR table (in Version 10 winds format)")'
                  else
                     print'(" This message type contains ",I0,
     .                " replications of quality information in input ",
     .                """foreign"" BUFR table"/" (in Version 10 winds ",
     .                "format)")', iret_PCCF/10
                  endif
                  print *
               ENDIF

               CALL UFBREP(LUNIN,TSIGIN,1,10,IRET,'TSIG')
               CALL UFBREP(LUNIN,GNAPIN,1,4,IRET,'GNAP')
               CALL UFBINT(LUNIN,TPHR,1,1,IRET,'TPHR')

               IF(IRD .LE. 3) THEN   !  Turn on extraneous print for
                                     !   first 3 reports in file
ccccc          IF(IRD .LE. 0) THEN   !  Turn off all extraneous print

C  Print bit switched on rather than flag table decimal value for SIDP
C  -------------------------------------------------------------------

                  satinfo6 = bmiss
                  if(ibfms(satinfo(6)).eq.0) then
                     call upftbv(lunin,'SIDP',satinfo(6),31,ibit,nib)
                     if(nib.gt.0) satinfo6 = ibit(1)
                  endif

                  satinfo11 = satinfo(11)
                  if(ibfms(satinfo(11)).ne.0) satinfo11 = 10e17

C  Assuming the input file has a 4'th (EE) replication, we'll assume
C   PCCF, GNAP and GCLONG are all missing in the input file's EE
C   replication if either:
C     1)  the output BUFR table is either not Version 10 or contains
C         only 3 replications of quality information (this affects the
C         below printout only); or
C     2)  Upon reading in GNAP in the EE replication from the input
C         file, it is not equal to 4 (this affects the below printout
C         and forces the values for PCCF, GNAP and GCLONG in the 4'th
C         (EE) replication of quality information to be enocoded as
C         missing
C  --------------------------------------------------------------------

                  if(ireps.LT.4.or.gnapin(4).ne.4) then
                     gclong(5)     = bmiss
                     pccfin(31:40) = bmiss
                     gnapin(4)     = bmiss
                    IF(IRD.EQ.1.and.ireps.eq.4.and.iret_PCCF.eq.40) THEN
                        print'(" Input BUFR file does not contain EE ",
     .                   "quality information even though its BUFR ",
     .                   "table contains place holders for it - encode",
     .                   " PCCF,"/" GNAP and GCLONG as missing in the ",
     .                   "4th (EE) replication of quality information ",
     .                   "in the output BUFR file")'
                        print *
                     endif
                  endif

                  write(6,
     .    '(''SAID SCLF  SSNX     SSNY   SWCM  SIDP  SCCF       CCST '',
     .    ''   LSQL   SAZA           SCBW       OFGI''/
     .    2(f4.0,1x),f7.0,2x,f7.0,2x,f3.0,2x,f4.0,1pe12.5,0pf6.1,3x,
     .    f3.0,2x,f6.1,2x,f17.0,2x,f6.0)')
     .            (satinfo(ii),ii=1,5),satinfo6,(satinfo(ii),ii=7,10),
     .            satinfo11,satinfo(12)
                  write(6,'('' latitude, longitude :'',f7.2,1x,f7.2)')
     .             xlalo
                write(6,'(''FINAL  wind direction speed:'',2(f9.1,1x))')
     .             (windsq(i,1),i=1,2)
                write(6,'(''GUESS  wind direction speed:'',2(f9.1,1x))')
     .             (windsq(i,2),i=1,2)
                write(6,'(''ORIGNL wind direction speed:'',2(f9.1,1x))')
     .             (windsq(i,3),i=1,2)
                write(6,'(''IMG1-2 wind direction speed:'',2(f9.1,1x))')
     .             (windsq(i,4),i=1,2)
                write(6,'(''IMG2-3 wind direction speed:'',2(f9.1,1x))')
     .             (windsq(i,5),i=1,2)
ccccc             write(6,'(''wind direction speed:'',4f10.1)') 
ccccc.             ((windsq(i,j),i=1,2),j=1,5)
                  write(6,'(''Gen. Center. (overall, generating appl.'',
     .             '' 1-4):'',5(f9.1,1x))') gclong
                  write(6,'(''Tracer Corr.:'',f10.1)') tcmd
                  write(6,'(''Pressure (FNL, WINDOW CHN, HISTOGRAM, '',
     .             ''H2O INTERCEPT, CO2 SLICING, ORIG, LAST 5 '',
     .             ''MISSING):''/5x,11(f9.1,1x))') prlc
                  write(6,'(''Hgt. assign. method (FNL, WINDOW CHN, '',
     .             ''HISTOGRAM, H2O INTERCEPT, CO2 SLICING, ORIG, '',
     .             ''LAST 5 MISSING):''/5x,11(f7.0,1x))') hamd
                  write(6,'(''dry bulb temp (WINDOW CHN, HISTOGRAM, '',
     .             '' H2O INTERCEPT, CO2 SLICING, ORIG, LAST 5 '',
     .             ''MISSING):''/5x,10(f7.1,1x))') tmdbst 
                  write(6,'(''Year Month Day.:'',3f8.0)') ymday
                  write(6,'(''hour (OBS, GUESS, ORIG, IMG1-2, IMG2-3)'',
     .             '':'',5(f7.1,1x))') hourin((/1,3,5,7,9/))
                  write(6,'(''minute second (OBS, GUESS, ORIG, '',
     .             ''IMG1-2,IMG2-3):''/5x,2(f7.1,1x),''|'',2(f7.1,1x),
     .             ''|'',2(f7.1,1x),''|'',2(f7.1,1x),''|'',2(f7.1,1x))')
     .             tmiscin((/1,2/),(/1,2,4,6,8/))
                  write(6,'(''WDIR QI w/o forecast:'',f7.1)') pccfin(1) 
                  write(6,'(''WDIR recursive filter:'',f6.1)')
     .             pccfin(11) 
                  write(6,'(''WDIR QI w/  forecast:'',f7.1)') pccfin(21)
                  write(6,'(''WDIR expected error:'',f8.1)') pccfin(31) 
                  write(6,'(''WSPD QI w/o forecast:'',f7.1)') pccfin(2) 
                  write(6,'(''WSPD recursive filter:'',f6.1)')
     .             pccfin(12)
                  write(6,'(''WSPD QI w/  forecast:'',f7.1)') pccfin(22)
                  write(6,'(''WSPD expected error:'',f8.1)') pccfin(32) 
                  write(6,'(''time significance (GUESS, ORIG, IMG1-2,'',
     .             '' IMG2-3):'',4f8.1/)') tsigin((/3,5,7,9/))
                  write(6,'(''gen. application:'',4f8.0/)') gnapin
                  write(6,'(''time per. or displ.:'',f8.0/)') tphr
               END IF

               HOUROUT(1:5)=HOURIN(1:9:2)

               TMISCOUT(1:2,1)=TMISCIN(1:2,1)
               TMISCOUT(1:2,2:5)=TMISCIN(1:2,2:8:2)

               TSIGOUT(1:4)=TSIGIN(3:9:2)

               IF(ireps.EQ.4) THEN

C  Come here if space for EE is allocated in this output message type
C  ------------------------------------------------------------------

                  IF(iret_PCCF.EQ.40) THEN

C     .... come here if space for EE is allocated in this input message
C          type - if EE is present it will be transferred to this
C          output message type
C          ------------------------------------------------------------

                     PCCFOUT(1:4)=PCCFIN(1:31:10)
                     PCCFOUT(5:8)=PCCFIN(2:32:10)

                     GNAPOUT(1:4)=GNAPIN(1:4)
                     GNAPOUT(5:8)=GNAPIN(1:4)

                     OGCEOUT(1:5)=GCLONG(1:5)
                     OGCEOUT(6:9)=GCLONG(2:5)

                  ELSE IF(iret_PCCF.EQ.30) THEN

C     .... come here if EE is NOT in this input message type - it will
C          be set to missing in this output message type
C          -----------------------------------------------------------

                     PCCFOUT(1:3)=PCCFIN(1:21:10)
                     PCCFOUT(4)  =BMISS
                     PCCFOUT(5:7)=PCCFIN(2:22:10)
                     PCCFOUT(8)  =BMISS

                     GNAPOUT(1:3)=GNAPIN(1:3)
                     GNAPOUT(4)  =BMISS
                     GNAPOUT(5:7)=GNAPIN(1:3)
                     GNAPOUT(8)  =BMISS

                     OGCEOUT(1:4)=GCLONG(1:4)
                     OGCEOUT(5)  =BMISS
                     OGCEOUT(6:8)=GCLONG(2:4)
                     OGCEOUT(9)  =BMISS

                  END IF

               ELSE IF(ireps.EQ.3) THEN

C  Come here if space for EE NOT allocated in this output message type
C  -------------------------------------------------------------------

                  IF(iret_PCCF.EQ.40 .or. iret_PCCF.EQ.30) THEN

C     .... come here if space for EE either IS or is NOT allocated in
C          this input message type - in the case of the former it will
C          NOT be transferred to this output message type
C          -----------------------------------------------------------

                     PCCFOUT(1:3)=PCCFIN(1:21:10)
                     PCCFOUT(4:6)=PCCFIN(2:22:10)

                     GNAPOUT(1:3)=GNAPIN(1:3)
                     GNAPOUT(4:6)=GNAPIN(1:3)

                     OGCEOUT(1:4)=GCLONG(1:4)
                     OGCEOUT(5:7)=GCLONG(2:4)

                  END IF

               END IF

               SWQM=2

               STNID = '????????'


C  IN FIRST POSITION OF ID:  SATELLITE ID 
C    GOES     SATELLITE NO. 251, 255, 259      --- GETS CHARACTER 'A'
C    GOES     SATELLITE NO. 252, 256           --- GETS CHARACTER 'B'
C    GOES     SATELLITE NO. 253, 257           --- GETS CHARACTER 'C'
C    GOES     SATELLITE NO. 250, 254, 258      --- GETS CHARACTER 'D'
C    METEOSAT SATELLITE NO.  52,  56,  99      --- GETS CHARACTER 'X'
C    METEOSAT SATELLITE NO.  53,  57           --- GETS CHARACTER 'Y'
C    METEOSAT SATELLITE NO.  50,  54,  58,  70 --- GETS CHARACTER 'Z'
C    METEOSAT SATELLITE NO.  51,  55,  59      --- GETS CHARACTER 'W'
C    GMS      SATELLITE NO. 152                --- GETS CHARACTER 'P'
C    GMS      SATELLITE NO. 150                --- GETS CHARACTER 'R'
C    GMS      SATELLITE NO. 151                --- GETS CHARACTER 'O'
C    MTSAT    SATELLITE NO. 171, 175, 199      --- GETS CHARACTER 'Q'
C    MTSAT    SATELLITE NO. 172, 176           --- GETS CHARACTER 'R'
C    MTSAT    SATELLITE NO. 173                --- GETS CHARACTER 'O'
C    MTSAT    SATELLITE NO. 174                --- GETS CHARACTER 'P'
C    KALPANA  SATELLITE NO. 440                --- GETS CHARACTER 'K'
C    INSAT    SATELLITE NO. 470, 499           --- GETS CHARACTER 'V'
C    TERRA    SATELLITE NO. 783                --- GETS CHARACTER 'T'
C    AQUA     SATELLITE NO. 784                --- GETS CHARACTER 'U'
C    METOP    SATELLITE NO.   3                --- GETS CHARACTER 'H'
C    METOP    SATELLITE NO.   4                --- GETS CHARACTER 'E'
C    METOP    SATELLITE NO.   5                --- GETS CHARACTER 'I'
C    NOAA     SATELLITE NO. 206                --- GETS CHARACTER 'F'
C    NOAA     SATELLITE NO. 207                --- GETS CHARACTER 'L'
C    NOAA     SATELLITE NO. 208                --- GETS CHARACTER 'M'
C    NOAA     SATELLITE NO. 209                --- GETS CHARACTER 'N'
C    NOAA     SATELLITE NO. 223                --- GETS CHARACTER 'G'
C    NPP      SATELLITE NO. 224                --- GETS CHARACTER 'J'
C    NOAA     SATELLITE NO. 225                --- GETS CHARACTER 'S'

               ISATID1=NINT(SATINFO(1))

               IF(ISATID1.GT.0 .AND. ISATID1.LT.785)  THEN
                  IF(STNID1(ISATID1).NE.'?') STNID(1:1)=STNID1(ISATID1)
               END IF
               IF(STNID(1:1).EQ.'?') THEN
                  PRINT *,'Satellite ID ',ISATID1,' is missing or not ',
     .             'valid value, skip'
                  KTSKPT=KTSKPT+1
                  IERR=2
                  CYCLE
               END IF 
C
C    IN SIXTH POSITION OF ID: READ SATELLITE DERIVED WIND COMPUTATION
C    METHOD TO GET INFORMATION ABOUT WHICH WIND TYPE IS IN THE SUBSET 
C    WATER VAPOR CHANNEL (IMAGER AND SOUNDER)....GETS CHARACTER 'W',
C    INFRARED CHANNEL (LONG-WAVE OR SHORT-WAVE)..GETS CHARACTER 'I'
C    VISIBLE CHANNEL.............................GETS CHARACTER 'Z'
C    SPECTRAL CHANNELS OR PICTURE TRIPLET........GETS CHARACTER 'P'
C
 
C    SATELLITE DERIVED WIND METHOD AS READ IN - ISATID5:
C      1 - IR (LONG WAVE OR SHORT WAVE)
C      2 - VISIBLE
C      3 - WATER VAPOR (IMAGER AND SOUNDER) - CLOUD TOP
C      4 - COMBNATION OF SPECTRAL CHANNELS OR PICTURE TRIPLET
C      5 - WATER VAPOR (IMAGER AND SOUNDER) - CLEAR AIR (DEEP LAYER)
C      6 - RESERVED FOR OZONE
C      7 - WATER VAPOR (IMAGER AND SOUNDER) - CLOUD TOP OR CLEAR AIR
C          UNKNOWN

               ISATID5=NINT(SATINFO(5))

               SWDL=BMISS
               IF((ISATID5.GT.0. AND. ISATID5.LT.8) .AND. ISATID5.NE.6)
     .          THEN
                  STNID(6:6)=STNID6(ISATID5)
                  IF(ISWDL(ISATID5).LT.3)  THEN
                     SWDL=ISWDL(ISATID5)
                  ELSE
                     PRINT'(" The value of satellite derived wind ",
     .                "method",F5.1," is missing or not a valid ",
     .                "value")', SWDL
                     KTSKPT=KTSKPT+1
                     IERR=2
                     CYCLE
                  END IF
               ELSE
                  PRINT'(" The value of satellite derived wind method ",
     .             F5.1," is missing or not a valid value")', SWDL
                  KTSKPT=KTSKPT+1
                  IERR=2
                  CYCLE
               END IF

C  CHECK REPORT DATE (YYYYMMDDHH) TO SEE IF A NEW OUTPUT MESSAGE
C  SHOULD BE OPENED (TRANJB TAKES CARE OF THIS FOR UNCOMPRESSED
C  FILES, BUT IT DOESN'T HURT TO HAVE REDUNDANCY BUILT IN HERE)
C  -------------------------------------------------------------

               LDATE = IYR*1000000+MON*10000+IDAY*100+IHR
ccccc          print *,' LDATE ',ldate
ccccc          print *,' SUBSET IS ',SUBSET
               IF(LDATE.NE.LDATE_prev)  then
                  print *, ' '
                  print *, 'OPENING OUTPUT MESSAGE WITH NEW DATE ',
     .             LDATE,' (SUBSET ',SUBSET,')'
                  print *, ' '
               ENDIF
               LDATE_prev = LDATE
               CALL OPENMB(LUNOT,SUBSET,LDATE)

C  WRITE A SUBSET
C  --------------

               IF(.NOT.V10_BUFR) THEN
C   ... OUTPUT that is NOT in quasi-NESDIS Version 10 BUFR winds format
C       in bufrtab.005 comes here (all INPUT is in true NESDIS Version
C       10 BUFR WINDS format)
                  CALL UFBINT(LUNOT,SATINFO,10,1,IRET,
     .             'SAID SCLF SSNX SSNY SWCM SIDP SCCF CCST LSQL SAZA')
                  GCLONG=160.0 ! orig. center is hardwired to NESDIS
                  CALL UFBINT(LUNOT,GCLONG,1,1,IRET,'GCLONG')
                  CALL UFBREP(LUNOT,TMDBST,1,5,IRET,'TMDBST')
                  CALL UFBREP(LUNOT,PRLC,1,6,IRET,'PRLC')
                  CALL UFBREP(LUNOT,HAMD,1,6,IRET,'HAMD')
                  IF(ISATID5.EQ.4) PCCFIN(11) = BMISS ! ptriplet RFFL
                                                      ! set to missing
                  CALL UFBINT(LUNOT,PCCFIN(11),1,1,IRET,'RFFL')
                  CALL UFBINT(LUNOT,SWDL,1,1,IRET,'SWDL')
               ELSE
C   ... OUTPUT that is in quasi-NESDIS Version 10 BUFR winds format in
C       bufrtab.005 comes here (all INPUT is in true NESDIS Version 10
C       BUFR WINDS format)
                  RCPDAT=BMISS
                  RCPDAT(1) = 0.
                  CALL W3UTCDAT(IDAT)
                  RCPDAT(2:4) = REAL(IDAT(1:3))
                  RCPDAT(5:6) = REAL(IDAT(5:6))
                  CORN = 0.
                  CALL UFBINT(LUNOT,SATINFO,12,1,IRET,
     .    'SAID SCLF SSNX SSNY SWCM SIDP SCCF CCST LSQL SAZA SCBW OFGI')
                  CALL UFBREP(LUNOT,OGCEOUT,1,(2*ireps)+1,IRET,'OGCE')
                  CALL UFBREP(LUNOT,TMDBST,1,10,IRET,'TMDBST')
                  CALL UFBREP(LUNOT,PRLC,1,11,IRET,'PRLC')
                  CALL UFBREP(LUNOT,HAMD,1,11,IRET,'HAMD')
                  CALL UFBREP(LUNOT,PCCFOUT,1,2*ireps,IRET,'PCCF')
                  CALL UFBREP(LUNOT,GNAPOUT,1,2*ireps,IRET,'GNAP')
                  CALL UFBINT(LUNOT,RCPDAT,6,1,IRET,
     .             'RCTS RCYR RCMO RCDY RCHR RCMI')
                  CALL UFBINT(LUNOT,CORN,1,1,IRET,'CORN')
                  CALL UFBINT(LUNOT,TPHR,1,1,IRET,'TPHR')
               END IF
               CALL UFBINT(LUNOT,XLALO,2,1,IRET,'CLAT CLON')
               CALL UFBREP(LUNOT,WINDSQ,2,5,IRET,'WDIR WSPD')
               CALL UFBINT(LUNOT,SWQM,1,1,IRET,'SWQM')
               CALL UFBINT(LUNOT,RSTNID,1,1,IRET,'RPID')
               CALL UFBINT(LUNOT,TCMD,1,1,IRET,'TCMD')
               CALL UFBINT(LUNOT,YMDAY,3,1,IRET,'YEAR MNTH DAYS')
               CALL UFBREP(LUNOT,HOUROUT,1,5,IRET,'HOUR')
               CALL UFBREP(LUNOT,TMISCOUT,2,5,IRET,'MINU SECO')
               CALL UFBREP(LUNOT,TSIGOUT,1,4,IRET,'TSIG')

               CALL WRITSB(LUNOT)

               IWT=IWT+1

            END IF
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
 2003    FORMAT(' NO REPORTS PROCESSED -- DISABLING ALL SUBSEQUENT ',
     .    'PROCESSING.')
         CALL W3TAGE('BUFR_TRANSATW')
         CALL ERREXIT(253)
      ENDIF
      CALL W3TAGE('BUFR_TRANSATW')

      STOP
      END

C######################################################################
C######################################################################
C######################################################################

C Read through the output BUFR table in "iunit".
C Find all lines that define the mnemonics in "subset".
C Search each line looking for mnemonic "GQCPRMS" (including quotes).
C Determine how many standard replicatons are defined for "GQCPRMS".
C Return this to calling program as "ireps"
C A value of zero for "ireps" is the default and means that "GQCPRMS"
C  was not found.

       subroutine scan_bufrtable(iunit,subset,ireps)

       character*1 creps
       character*8 subset
       character*80 card
       logical digit

       imatch = 0
       ireps = 0

       do i = 1,10000
       read(iunit,'(A80)',end=88,err=99) card
cc     write(6,44) card
  44   format(1x,'CARD "',A,'"')
       if(card(1:10).eq.'| '//subset) then
          imatch = imatch + 1
          if(imatch.gt.1) then
cc           write(6,44) card
             do istart = 14,69
                if(card(istart:istart+8).eq.'"GQCPRMS"') then
                   creps = card(istart+9:istart+9)
                   if(digit(creps))  read(creps,'(i1)') ireps
                   return
                end if
             end do
          end if
       end if
       end do

  99   continue

       WRITE(6,'('' ERROR SCANNING OUTPUT BUFR TABLE'')')
       CALL W3TAGE('BUFR_TRANSATW')
       CALL ERREXIT(44)

  88   continue

       return
       end

