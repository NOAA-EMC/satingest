C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM:  BUFR_TRANMTYPSBT
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2014-01-24
C
C ABSTRACT: CHANGES THE BUFR TYPE AND SUBTYPE INTERNALLY IN EACH NON-
C   DICTIONARY BUFR MESSAGE IN A BUFR FILE (NORMALLY NCEP-FLAVORED BUT
C   DOESN'T HAVE TO BE).  THE BUFR TYPE AND SUBTYPE TO CHANGE TO ARE IN
C   CHARACTERS 2-4 AND 8-10, RESPECTIVELY IN THE IMPORTED SCRIPT
C   ENVIRONMENT VARIABLE "TANKFILE".  THE NEW BUFR TYPE (ttt) AND
C   SUBTYPE (sss) ARE WRITTEN OVER THEIR PREVIOUS VALUES IN THE
C   APPROPRIATE BYTES OF SECTION 1 OF EACH MESSAGE (I.E., BYTES 9 AND
C   10 FOR EDITION 3 BUFR MESSAGES AND BYTES 11 AND 13 FOR EDITION 4
C   BUFR MESSAGES).  ANY DICTIONARY MESSAGES ENCOUNTERED (BUFR TYPE 11)
C   ARE COPIED AS IS (WITHOUT ANY MODIFICATION), UNLESS OPTION 2 BELOW
C   IS INVOKED.
C           OPTION 1: THE TOP-LEVEL SEQUENCE DESCRIPTOR CORRESPONDING
C   TO THE UPDATED MESSAGE TYPE AND SUBTYPE ABOVE ("NCtttsss") IS READ
C   FROM THE EXTERNAL BUFR TABLE ASCII FILE "bufrtab.ttt". {THE BUFR
C   MNEMONIC TABLE FILES ARE ASSUMED TO EXIST IN ONE OF THREE
C   DIRECTORIES: IN "TANK_DIR", WHERE WHERE TANK_DIR IS THE TANK
C   (DATABASE PARENT) DIRECTORY (READ FROM SCRIPT ENVIRONMENT VARIABLE
C   "tank_dir") (FIRST CHOICE FOR ALL RUN TYPES); IN
C   "SATINGEST_FIX_DIR", WHERE SATINGEST_FIX_DIR IS THE
C   OBSPROC_SATINGEST FIX DIRECTORY (READ FROM SCRIPT ENVIRONMENT
C   VARIABLE "FIXobsproc_satingest") (SECOND CHOICE FOR SATINGEST RUN
C   TYPE AND THIRD CHOICE FOR DECODER RUN TYPE); OR IN "BUFR_FIX_DIR",
C   WHERE BUFR_FIX_DIR IS THE BUFR FIX DIRECTORY (READ FROM SCRIPT
C   ENVIRONMENT VARIABLE "FIXbufr") (SECOND CHOICE FOR DECODER RUN TYPE
C   AND THIRD CHOICE FOR SATINGEST RUN TYPE).} THIS SEQUENCE DESCRIPTOR
C   IS THEN WRITTEN OVER THE FIRST SEQUENCE DESCRIPTOR (ASSUMED TO BE
C   THE ORIGINAL TOP-LEVEL DESCRIPTOR) ENCOUNTERED IN SECTION 3 OF EACH
C   NON-DICTIONARY BUFR MESSAGE.  THIS IS DONE ONLY IF THIS IS EITHER
C   THE FIRST DESCRIPTOR IN SECTION 3 (BYTES 8-9) {MEANING THE FILE IS
C   MOST LIKELY NCEP-FLAVORED BUFR (I.E., IT CONTAINS A TOP-LEVEL
C   STANDARD SEQUENCE DESCRIPTOR IN SECTION 3) BUT IS OTHERWISE
C   STANDARDIZED}, OR THE SECOND DESCRIPTOR IN SECTION 3 (BYTES 10-11)
C   {MEANING THE FILE IS MOST LIKELY NCEP-FLAVORED BUFR (I.E., IT
C   CONTAINS A TOP-LEVEL STANDARD OR LOCAL SEQUENCE DESCRIPTOR IN
C   SECTION 3) BUT IS NOT STANDARDIZED IN THAT THE FIRST DESCRIPTOR IN
C   SECTION 3 IS A LOCAL TABLE B DESCRIPTOR HOLDING "BYTE COUNT" (F-X-Y
C   VALUE 0-63-000)}.  IF NEITHER THE FIRST NOR SECOND DESCRIPTOR IN
C   SECTION 3 IS A SEQUENCE DESCRIPTOR, THE PROGRAM ABORTS.  NORMALLY
C   OPTION 1 WOULD ONLY BE INVOKED FOR NCEP- FLAVORED BUFR FILES WHERE
C   THE ORIGINAL TOP-LEVEL SEQUENCE DESCRIPTOR IN SECTION 3 IS NOT
C   DEFINED IN THE UPDATED MESSAGE TYPE "NCtttsss" READ FROM THE
C   EXTERNAL BUFR TABLE ASCII FILE "bufrtab.ttt".  THIS OPTION IS
C   CONTROLLED BY THE VALUE OF SCRIPT ENVIRONMENT VARIABLE
C   "mod_sec3_desc" (SEE REMARKS).
C           OPTION 2: THE EXTERNAL BUFR TABLE ASCII FILE FOUND IN
C   "bufrtab.ttt" (CONTAINING "NCtttsss" AS DEFINED ABOVE) CAN BE
C   ENCODED INTO THE TOP MESSAGES IN THE OUTPUT BUFR FILE.  IF THIS IS
C   DONE, THEN ANY DICTIONARY MESSAGES INITIALLY PRESENT IN THE INPUT
C   FILE ARE SKIPPED OVER (I.E., NOT COPIED TO THE OUTPUT FILE).  THIS
C   OPTION IS CONTROLLED BY THE VALUE OF SCRIPT ENVIRONMENT VARIABLE
C   "encode_bufrtable" (SEE REMARKS).
C
C   OPTIONS 1 AND 2 ARE INDEPENDENT OF EACH OTHER. THIS CODE THUS
C   ALLOWS NCEP TO PROCESS BUFR FILES GENERATED BY OUTSIDE PROVIDERS
C   (E.G., NESDIS) (USUALLY NCEP-FLAVORED) WITH A BUFR TYPE AND SUBTYPE
C   THAT MAY EITHER BE IN CONFLICT WITH AN EXISTING BUFR TYPE AND
C   SUBTYPE IN THE NCEP BUFR DATABASE, OR MAY SIMPLY BE INCONSISTENT
C   WITH NCEP DATABASE STANDARDS.  THESE FILES MAY THEN BE READY FOR
C   DIRECT DATABASING VIA BUFR_TRANJB.
C
C PROGRAM HISTORY LOG:
C 2005-10-06  J. WOOLLEN - ORIGINAL AUTHOR OF NOW OBSOLETE PROGRAM
C     BUFR_TRANAIRSWRM FROM WHICH THIS PROGRAM WAS DERIVED
C 2005-11-29  D. KEYSER  - ORIGINAL AUTHOR OF THIS PROGRAM
C 2006-02-24  D. KEYSER  - REPLACED CALL TO BUFRLIB ROUTINE OVRBS1 WITH
C     CALL TO BUFRLIB ROUTINE PKBS1 (OVRBS1 OBSOLETE WITH 1/31/2006
C     VERSION OF BUFRLIB) - CURRENTLY MUST USE IN-LINE VERSIONS OF
C     BUFRLIB ROUTINES PKBS1, IUPBS01, GETS1LOC AND PKVS01, ALL
C     MODIFIED TO UPDATE MESSAGE TYPE AND SUBTYPE IN SECTION 1 OF BUFR
C     MESSAGES (THESE CHANGES WILL BE INCORPORATED INTO THE NEXT UPDATE
C     OF BUFRLIB, AFTERWHICH THESE CAN BE REMOVED FROM THIS PROGRAM)
C 2007-02-07  D. KEYSER  - REMOVED IN-LINE ROUTINES PKBS1, IUPBS01,
C     GETS1LOC AND PKVS01 NOW THAT THEY HAVE ADDED TO THE NEWLY-
C     IMPLEMENTED BUFRLIB UPDATE WHICH IS LINKED IN BY THIS CODE AT
C     COMPILE TIME; WORKS FOR EDITION 4 BUFR MESSAGES (AS WELL AS
C     EDITION 3, AS BEFORE); ADDED OPTION TO ENCODE EXTERNAL BUFR
C     MNEMONIC TABLE (IN UNIT 20) INTO BEGINNING OF OUTPUT BUFR FILE
C     (VIA USE OF INPUT SCRIPT ENVIRONMENTAL VARIABLE
C     "encode_bufrtable")
C 2008-10-23  D. KEYSER  - MODIFICATION OF THE TOP-LEVEL SEQUENCE
C     DESCRIPTOR IN SEC. 3 IS MADE OPTIONAL RATHER THAN MANDATORY (VIA
C     USE OF INPUT SCRIPT ENVIRONMENTAL VARIABLE "mod_sec3_desc"); IN
C     THE EVENT THAT THE TOP-LEVEL DESCRIPTOR IN SEC. 3 IS SET TO BE
C     CHANGED, NOW CHECKS FIRST SEC. 3 DESCRIPTOR (BYTES 8-9) TO ENSURE
C     THAT IT IS INDEED A SEQUENCE DESCRIPTOR BEFORE OVERWRITING TAKES
C     PLACE (USUALLY THE CASE FOR NCEP-FLAVORED FILES THAT ARE
C     OTHERWISE STANDARDIZED), IF IT IS NOT A SEQUENCE DESCRIPTOR {MOST
C     LIKELY MEANING IT IS 0-63-000 ("BYTE COUNT", FOUND IN NCEP-
C     FLAVORED BUFR FILES THAT ARE NOT STANDARDIZED), THEN THE SECOND
C     SEC. 3 DESCRIPTOR (BYTES 10-11) IS CHECKED TO SEE IF IT IS A
C     SEQUENCE DESCRIPTOR AND IF IT IS IT IS ASSUMED TO BE THE TOP-
C     LEVEL DESCRIPTOR AND IS THUS OVERWRITTEN (USUALLY THE CASE FOR
C     NCEP-FLAVORED FILES THAT ARE NOT STANDARDIZED) - IF NEITHER THE
C     FIRST NOT THE SECOND DESCRIPTOR IN SEC. 3 ARE SEQUENCE
C     DESCRIPTORS THE PROGRAM ABORTS (PREVIOUSLY THIS PROGRAM ALWAYS
C     OVERWROTE THE FIRST DESCRIPTOR IN SEC. 3 - THIS WOULD CAUSE A
C     LATER TRANJB ERROR FOR NON-STANDARD NCEP-FLAVORED BUFR FILES
C     WHICH UNTIL NOW WERE NEVER PROCESSED BY THIS PROGRAM); WILL NOW
C     ABORT IF BUFR MESSAGE CONTAINS OPTIONAL SEC. 2 (HAS NEVER
C     HAPPENED TO DATE) SINCE IT CANNOT LOCATE BEGINNING OF SEC. 3 (AT
C     LEAST WITHOUT SOME ADDITIONAL LOGIC (PREVIOUSLY NEVER TESTED FOR
C     SEC. 2, ALWAYS ASSUMED IT WAS MISSING - ITS PRESENCE WOULD CAUSE
C     LATER TRANJB ERROR)
C 2010-01-29  D. KEYSER  - REPLACED CALL TO BUFRLIB ROUTINE WRDLEN
C     WITH CALL TO W3LIB ROUTINE W3FI01 TO GET MACHINE WORD LENGTH,
C     THIS ALLOWS FOR REMOVAL OF BUFRLIB COMMON BLOCK HRDWRD WHICH IS
C     CHANGING IN THE NEXT UPDATE OF BUFRLIB (AND REMOVES A BAD
C     PROGRAMMING PRACTICE!)
C 2012-10-03  D. KEYSER -- CHANGES TO RUN ON WCOSS {USES BUFRLIB C I/O
C     INTERFACE TO READ/WRITE BUFR MESSAGES, NO NEED TO CONSIDER
C     BLOCKING STRUCTURE OF INPUT BUFR FILE SINCE WILL READ EITHER
C     BLOCKED (BIG- OR LITTLE-ENDIAN) OR UNBLOCKED FILES - OUTPUT BUFR
C     FILE ON WCOSS IS UNBLOCKED SINCE THIS IS THE BUFRLIB DEFAULT}
C 2014-01-24 D. KEYSER  -- NOW LOOKS FOR SCRIPT ENVIRONMENT VARIABLE
C     "RUN_TYPE" VIA CALL TO GET_ENVIRONMENT_VARIABLE.  IT IS EITHER
C     'decoder' (DEFAULT) or 'satingest'.  NOW THREE CHOICES FOR
C     LOCATING EXTERNAL BUFR MNEMONIC TABLE FILE bufrtab.XXX WHICH
C     DEPEND UPON VALUE OF "RUN_TYPE", RATHER THAN THE TWO FIXED
C     CHOICES BEFORE.  FIRST CHOICE REMAINS TANK DIRECTORY (tank_dir)
C     REGARDLESS OF RUN TYPE.  SECOND CHOICE IS OBSPROC_SATINGEST FIX
C     DIRECTORY (FIXobsproc_satingest) FOR SATINGEST RUN TYPE AND BUFR
C     FIX DIRECTORY (FIXbufr) FOR DECODER RUN TYPE.  THIRD CHOICE IS
C     FIXbufr FOR SATINGEST RUN TYPE AND FIXobsproc_satingest FOR
C     DECODER RUN TYPE. THIS CHANGE WILL ALLOW DEOCDER RUNS TO USE THIS
C     VERSION OF BUFR_TRANMTYPSBT SINCE THE DECODER RUNS WILL USE THE
C     BUFR MNEMONIC TABLES IN ITS PREFERRED LOCATION of FIXbufr.  THE
C     SATINGEST RUNS WILL, FOR NOW, USE THE BUFR MNEMONIC TABLES IN
C     THEIR THIRD CHOICE LOCATION (FIXbufr), BUT WILL AUTOMATICALLY
C     TRANSITION TO THE SECOND CHOICE FIXobsproc_satingest ONCE THE
C     BUFR MNEMONIC TABLES ARE AVAILABLE THERE.
C
C USAGE:
C   INPUT FILES:
C     UNIT 11  - BUFR FILE (MOST LIKELY NCEP-FLAVORED, EITHER STANDARD
C                OR NON-STANDARD, GENERATED BY NESDIS OR SOME OTHER
C                ORGANIZATION OUTSIDE OF NCEP) - MAY BE EITHER FORTRAN
C                BLOCKED (BIG- OR LITTLE-ENDIAN) OR UNBLOCKED
C     UNIT 20  - EXTERNAL BUFR MNEMONIC TABLE {EITHER
C                TANK_DIR/bufrtab.XXX (FIRST CHOICE FOR ALL RUN TYPES),
C                SATINGEST_FIX_DIR/bufrtab.XXX (SECOND CHOICE FOR
C                SATINGEST RUN TYPE AND THIRD CHOICE FOR DECODER RUN
C                TYPE), OR BUFR_FIX_DIR/bufrtab.XXX (SECOND CHOICE FOR
C                DECODER RUN TYPE AND THIRD CHOICE FOR SATINGEST RUN
C                TYPE); WHERE TANK_DIR IS THE ROOT OF THE DIRECTORY
C                PATH TO THE BUFR DATABASE TANK FILE (E.G.,
C                /dcom/us007003) (READ FROM SCRIPT ENVIRONMENT VARIABLE
C                "tank_dir"), SATINGEST_FIX_DIR IS THE
C                OBSPROC_SATINGEST FIX DIRECTORY (READ FROM SCRIPT
C                ENVIRONMENT VARIABLE "FIXobsproc_satingest"),
C                BUFR_FIX_DIR IS THE BUFR FIX DIRECTORY (READ FROM
C                SCRIPT ENVIRONMENT VARIABLE "FIXbufr"), XXX IS UPDATED
C                MESSAGE TYPE}
C                      WHEN OPTION 1 IN ABSTRACT IS INVOKED, USED TO
C                   OBTAIN TOP-LEVEL SEQUENCE DESCRIPTOR (CORRESPONDING
C                   TO THE UPDATED MESSAGE TYPE) WHICH IS THEN WRITTEN
C                   OVER CURRENT TOP-LEVEL SEQUENCE DESCRIPTOR IN SEC.
C                   3 OF EACH BUFR MESSAGE
C                      WHEN OPTION 2 IN ABSTRACT IS INVOKED, THIS TABLE
C                   IS ENCODED INTO THE TOP MESSAGES IN THE OUTPUT BUFR
C                   FILE (IN PLACE OF ANY EXISTING DICTIONARY MESSAGES,
C                   IF PRESENT IN INPUT FILE)
C                THIS IS NOT NEEDED IF NEIHER OPTION 1 NOR OPTION 2 IN
C                ABSTRACT ARE INVOKED.
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 51  - BUFR FILE (MOST LIKELY NCEP-FLAVORED, EITHER STANDARD
C                OR NON-STANDARD) IDENTICAL TO INPUT FILE IN UNIT 11
C                EXCEPT THE BUFR TYPE AND SUBTYPE IN SECTION 1 OF EACH
C                MESSAGE HAVE BEEN MODIFIED AND, OPTIONALLY, THE
C                TOP-LEVEL SEQUENCE DESCRIPTOR IN SEC. 3 OF EACH
C                MESSAGE HAS BEEN MODIFIED, AND, OPTIONALLY, THE
C                EXTERNAL ASCII BUFR TABLE IN UNIT 20 HAS BEEN ENCODED
C                INTO THE TOP MESSAGES IN THE FILE; ALSO, ON WCOSS,
C                THIS FILE CONTAINS RECORDS (BUFR MESSAGES) THAT ARE
C                NOT FORTRAN BLOCKED BASED ON THE BUFRLIB DEFAULT
C     UNIT 99  - SCRATCH FILE
C
C   SUBPROGRAMS CALLED:
C     SYSTEM:    - GET_ENVIRONMENT_VARIABLE   INQUIRE
C     LIBRARY:
C       W3NCO    - W3TAGB   W3TAGE   ERREXIT
C       BUFRLIB  - IUPBS01  PKB      PKBS1    IFXY
C                  DATELEN  OPENBF   IREADMG  COPYMG
C                  COPYBF   UPB      ADN30    RDMSGW
C                  MSGWRT   CLOSBF
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          = 253 - NO REPORTS WRITTEN OUT
C
C REMARKS: FIVE SCRIPT ENVIRONMENT VARIABLES ARE READ IN:
C         RUN_TYPE         - If = 'satingest' then this program is
C                              being executed by a satellite ingest
C                              job developed by NCEP/EMC
C                            If = 'decoder' then this program is being
C                              executed by a decoder job developed by
C                              NCEP/NCO/SIB (default)
C                            Note: Right now, this switch is only used
C                                  to determine the order of choices 2
C                                  and 3 for locating the external BUFR
C                                  mnemonic table)
C         TANKFILE         - In the form "bttt/xxsss" - this provides
C                            the BUFR type (ttt) and subtype (sss) to
C                            change to in Sec. 1 of each message (e.g.,
C                            "b021/xx255")
C         MOD_SEC3_DESC    - Switch determining if the top-level
C                            sequence descriptor in Sec. 3 of every
C                            BUFR message should be overwritten with
C                            the value from the external BUFR
C                            mnemonic table (!= "NO", default); if =
C                            "NO" this is not performed
C         ENCODE_BUFRTABLE - Switch determining if the external BUFR
C                            mnemonic table should be encoded into
C                            the top messages in output BUFR file (in
C                            place of any existing dictionary messages
C                            in file, if present in input file)
C                            (= "YES"); if != "YES" (default) this is
C                            not performed (and any existing dictionary
C                            messages in file will be copied as is, if
C                            present in input file)
C         TANK_DIR         - The root of the directory path to the BUFR
C                            database tank file {up to, but not
C                            including, the date sub-directory (e.g.,
C                            "/dcom/us007003")}
C                          - This directory path is the first choice
C                            for locating the external BUFR mnemonic
C                            tables for all run types (see
C                            SATINGEST_FIX_DIR for the second choice
C                            directory path for satingest run type and
C                            third choice directory path for decoder
C                            run type, and see BUFR_FIX_DIR for the
C                            second choice directory path for decoder
C                            run type and third choice directory path
C                            for satingest run type)
C                          - Applicable only if "mod_sec3_desc" != "NO"
C                            or "encode_bufrtable" = "YES"
C         SATINGEST_FIX_DIR
C                          - The path to the OBSPROC_SATINGSET fix
C                            directory (e.g.,
C                            /nwprod/obsproc_satingest.v2.0.0/fix)
C                          - This directory path is the second choice
C                            for locating the external BUFR mnemonic
C                            tables for satingest run type and the
C                            third choice for locating the external
C                            BUFR mnemonic tables for decoder run type
C                            (see TANK_DIR for the first choice
C                            directory path for all run types, and see
C                            BUFR_FIX_DIR for the second choice
C                            directory path for decoder run type and
C                            the third choice directory path for
C                            satingest run type)
C                          - Applicable only if "mod_sec3_desc" != "NO"
C                            or "encode_bufrtable" = "YES"
C         BUFR_FIX_DIR     - The path to the BUFR fix directory (e.g.,
C                            /nwprod/fix)
C                          - This directory path is the second choice
C                            for locating the external BUFR mnemonic
C                            tables for decoder run type and the third
C                            choice for locating the external BUFR
C                            mnemonic tables for satingest run type
C                            (see TANK_DIR for the first choice
C                            directory path for all run types, and see
C                            SATINGEST_FIX_DIR for the second choice
C                            directory path for satingest run type and
C                            the third choice directory path for
C                            decoder run type)
C                          - Applicable only if "mod_sec3_desc" != "NO"
C                            or "encode_bufrtable" = "YES"
C 
C     IMPORTANT:  THE RECORDS (BUFR MESSAGES) IN THE FILE IN UNIT 11
C                 MAY BE EITHER FORTRAN BLOCKED (BIG- OR LITTLE-ENDIAN)
C                 OR UNBLOCKED SINCE THIS PROGRAM USES BUFRLIB'S C I/O
C                 INTERFACE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      PROGRAM BUFR_TRANMTYPSBT

      PARAMETER (MXMSGL=50000) ! maximum BUFR message length (in bytes)

      CHARACTER*500 TANK_DIR,BUFR_FIX_DIR,SATINGEST_FIX_DIR,BTFILE,
     $              FILE_CHOICE2,FILE_CHOICE3
      CHARACTER*80  CARD
      CHARACTER*18  RUN_TYPE_MNEMONIC_CHOICE2,RUN_TYPE_MNEMONIC_CHOICE3
      CHARACTER*10  TANKFILE
      CHARACTER*11  BUFRTAB
      CHARACTER*9   RUN_TYPE
      CHARACTER*8   SUBSET
      CHARACTER*6   DESCR,ADN30,ADN30_1,ADN30_2
      CHARACTER*3   ENCODE_BUFRTABLE,MOD_SEC3_DESC
      DIMENSION     MBAY(MXMSGL)
      LOGICAL       EXIST
 
      DATA LUNIN  /11/
      DATA LUNDX  /20/
      DATA LUNOT  /51/
      DATA LSCRT  /99/
      DATA IRD    / 0/
      DATA IWT    / 0/
      DATA ICP    / 0/
      DATA ISK    / 0/
      DATA IFIRST / 0/

      DATA MTYPI_last /-99/,  MSBTI_last /-99/,  IEDTN_last /-99/
C-----------------------------------------------------------------------
      CALL W3TAGB('BUFR_TRANMTYPSBT',2014,0024,0081,'NP22')

C  OBTAIN RUN TYPE ('decoder' or 'satingest') FROM SCRIPT ENVIRONMENTAL
C  VARIABLE "RUN_TYPE"
C  -------------------------------------------------------------

      CALL GET_ENVIRONMENT_VARIABLE('RUN_TYPE',RUN_TYPE)
      IF(RUN_TYPE(1:1).EQ.' ')  RUN_TYPE = 'decoder'  ! default

      PRINT *, ' '
      PRINT *, ' ==> Welcome to BUFR_TRANMTYPSBT -- Version 01/24/2014',
     $ ' -- RUN TYPE IS ',trim(RUN_TYPE)
      PRINT *, ' '

C  OBTAIN NEW MESSAGE TYPE AND SUBTYPE FROM SCRIPT ENVIRONMENTAL
C  VARIABLE "TANKFILE"
C  -------------------------------------------------------------

      CALL GET_ENVIRONMENT_VARIABLE('TANKFILE',TANKFILE)
      READ(TANKFILE,'(1X,I3.3,3X,I3.3)',ERR=900,END=900) MTYPO,MSBTO

C  CHECK VALUE OF SCRIPT ENVIRONMENTAL VARIABLE "mod_sec3_desc", IF
C  "NO" DO NOT OVERWRITE EXISTING TOP-LEVEL SEQUENCE DESCRIPTOR IN
C  SECTION 3 WITH VALUE FROM EXTERNAL BUFR TABLE (OTHERWISE, YES
C  OVERWRITE)
C  ----------------------------------------------------------------

      CALL GET_ENVIRONMENT_VARIABLE('mod_sec3_desc',MOD_SEC3_DESC)

C  CHECK VALUE OF SCRIPT ENVIRONMENTAL VARIABLE "encode_bufrtable", IF
C  "YES" WILL LATER ENCODE THE EXTERNAL BUFR MNEMONIC TABLE INTO THE
C  TOP MESSAGES IN OUTPUT FILE (IN PLACE OF ANY EXISTING DICTIONARY
C  MESSAGES, IF PRESENT IN INPUT FILE) (OTHERWISE, DON'T ENCODE
C  EXTERNAL BUFR MNEMONIC TABLE, BUT EXISTING DICTIONARY MESSAGES WILL
C  BE COPIED AS IS IF PRESENT IN INPUT FILE)
C  -------------------------------------------------------------------

      CALL GET_ENVIRONMENT_VARIABLE('encode_bufrtable',ENCODE_BUFRTABLE)

      IF(ENCODE_BUFRTABLE.EQ.'YES' .OR. MOD_SEC3_DESC.NE.'NO') THEN

C  OBTAIN DATABASE AND FIXED FIELD DIRECTORIES FROM SCRIPT
C  ENVIRONMENTAL VARIABLES "tank_dir", "FIXobsproc_satingest" AND
C  "FIXbufr", RESPECTIVELY
C  --------------------------------------------------------------

         CALL GET_ENVIRONMENT_VARIABLE('tank_dir',TANK_DIR)
         CALL GET_ENVIRONMENT_VARIABLE('FIXbufr',BUFR_FIX_DIR)
         CALL GET_ENVIRONMENT_VARIABLE('FIXobsproc_satingest',
     $    SATINGEST_FIX_DIR)

         WRITE(BUFRTAB,'("bufrtab.",i3.3)') MTYPO

C----------------------------------------------------------------------
C LOOK FOR THE APPROPRIATE EXTERNAL BUFR MNEMONIC TABLE DIRECTORY PATH
C----------------------------------------------------------------------

         IFOUND = 0

         IF(trim(RUN_TYPE).EQ.'satingest') THEN
            FILE_CHOICE2 = trim(SATINGEST_FIX_DIR)
            FILE_CHOICE3 = trim(BUFR_FIX_DIR)
            RUN_TYPE_MNEMONIC_CHOICE2 = 'SATINGEST_FIX_DIR'
            RUN_TYPE_MNEMONIC_CHOICE3 = 'BUFR_FIX_DIR'
         ELSE
            FILE_CHOICE2 = trim(BUFR_FIX_DIR)
            FILE_CHOICE3 = trim(SATINGEST_FIX_DIR)
            RUN_TYPE_MNEMONIC_CHOICE2 = 'BUFR_FIX_DIR'
            RUN_TYPE_MNEMONIC_CHOICE3 = 'SATINGEST_FIX_DIR'
         ENDIF

C  First choice is the TANK directory "TANK_DIR" regardless of RUN_TYPE
C  --------------------------------------------------------------------

         BTFILE = trim(TANK_DIR)//'/'//BUFRTAB
         INQUIRE(FILE=BTFILE,EXIST=EXIST)
         IF(EXIST) THEN
            CLOSE(LUNDX)
            OPEN (LUNDX,FILE=BTFILE)
            PRINT'(101("=")/"OPENING BUFR MNEMONIC TABLE: ",A,"/",A,
     $       " IN UNIT",I3)', trim(TANK_DIR),BUFRTAB,LUNDX
            IFOUND = 1
cppppp
ccc   print *, 'Go with 1st choice - TANK_DIR'
cppppp
         ELSE

C  For RUN_TYPE = satingest, second choice is the OBSPROC_SATINGEST
C   FIX directory "SATINGEST_FIX_DIR"
C  For RUN_TYPE = decoder, second choice is the BUFR FIX directory
C   "BUFR_FIX_DIR"
C  ----------------------------------------------------------------

            BTFILE = trim(FILE_CHOICE2)//'/'//BUFRTAB
            INQUIRE(FILE=BTFILE,EXIST=EXIST)
            IF(EXIST) THEN
               CLOSE(LUNDX)
               OPEN (LUNDX,FILE=BTFILE)
               PRINT'(101("=")/"OPENING BUFR MNEMONIC TABLE: ",A,"/",A,
     $          " IN UNIT",I3)', trim(FILE_CHOICE2),BUFRTAB,LUNDX
               IFOUND = 1
cppppp
ccc   print *, 'For RUN_TYPE ',trim(RUN_TYPE),', go with 2nd choice - ',
ccc  $         trim(RUN_TYPE_MNEMONIC_CHOICE2)
cppppp
            ELSE

C  For RUN_TYPE = satingest, third choice is the BUFR FIX directory
C   "BUFR_FIX_DIR"
C  For RUN_TYPE = decoder, third choice is the OBSPROC_SATINGEST FIX
C   directory "SATINGEST_FIX_DIR"
C  -------------------------------------------------------------------

               BTFILE = trim(FILE_CHOICE3)//'/'//BUFRTAB
               INQUIRE(FILE=BTFILE,EXIST=EXIST)
               IF(EXIST) THEN
                  CLOSE(LUNDX)
                  OPEN (LUNDX,FILE=BTFILE)
                  PRINT'(101("=")/"OPENING BUFR MNEMONIC TABLE: ",A,"/",
     $             A," IN UNIT",I3)', trim(FILE_CHOICE3),BUFRTAB,LUNDX
                  IFOUND = 1
cppppp
ccc   print *, 'For RUN_TYPE ',trim(RUN_TYPE),', go with 3rd choice - ',
ccc  $         trim(RUN_TYPE_MNEMONIC_CHOICE3)
cppppp
               ENDIF
            ENDIF
         ENDIF
         IF(IFOUND.EQ.0) GO TO 901  ! BUFR table Not found, ERROR

         REWIND(LUNDX)

         IF(MOD_SEC3_DESC.NE.'NO') THEN

C  READ THROUGH THE EXTERNAL ASCII BUFR TABLE LOOKING FOR THE TOP-LEVEL
C  SEQUENCE DESCRIPTOR CORRESPONDING TO THE UPDATED MESSAGE TYPE
C  "NCtttsss"
C  --------------------------------------------------------------------

            DO
               READ(LUNDX,'(A80)',ERR=902,END=902) CARD
               IF(CARD(3:10).EQ.'NC'//TANKFILE(2:4)//TANKFILE(8:10))THEN
                  DESCR = '3'//CARD(15:19)  ! Found it, exit the loop
                  EXIT
               ENDIF
            ENDDO

            WRITE(6,130) TANKFILE(2:4),TANKFILE(8:10),DESCR
  130       FORMAT(/1X,'UPDATED MESSAGE TYPE "NC',A,A,'" IS LOCATED ',
     $       'IN THE EXTERNAL BUFR TABLE, ITS CORRESPONDING TOP-LEVEL ',
     $       'SEQUENCE DESCRIPTOR IS "',A,'"'/)
         ENDIF
      ENDIF

      CALL DATELEN(10)

C  CONNECT THE INPUT AND OUTPUT FILES FOR READING/WRITING TO THE
C   BUFRLIB C I/O INTERFACE
C  -------------------------------------------------------------

      CALL OPENBF(LUNIN,'INX',LUNIN)
      CALL OPENBF(LUNOT,'OUX',LUNIN)

C  READ IN EACH MESSAGE AND UPDATE IT WITH SPECIFIED VALUES
C  (UNLESS DICTIONARY MESSAGE, THEN CHECK VALUE OF encode_bufrtable, IF
C  YES, SKIP DICTIONARY MESSAGE, OTHERWISE JUST COPY IT AS IS)
C  --------------------------------------------------------------------

      DO
         CALL RDMSGW(LUNIN,MBAY,IER)
         IF(IER.EQ.-1)  GOTO 100  ! hit end-of-file
         IF(IER.EQ.-2)  GOTO 907  ! read error

         IRD = IRD + 1  ! count the messages read in

C  SEE WHAT THE INITIAL MESSAGE TYPE AND SUBTYPE ARE
C  -------------------------------------------------

         ISEC2 = IUPBS01(MBAY,'ISC2') ! See if Sec. 2 exists(0-no,1-yes)
ccccc       print *, 'ISEC2 = ',ISEC2

C  If this BUFR message contains optional Section 2 - cannot locate the
C   beginning of Section 3, considered an ERROR (future coding could
C   get around this, but almost never any BUFR files with Sec. 2)
C  --------------------------------------------------------------------

         IF(ISEC2.NE.0) GO TO 905

         IEDTN = IUPBS01(MBAY,'BEN') ! BUFR edition number from Sect. 0
         MTYPI = IUPBS01(MBAY,'MTYP')! message type from Section 1
         MSBTI = IUPBS01(MBAY,'MSBT')! message subtype from Section 1
ccccc       print *, 'IEDTN, MTYPI, MSBTI: ',IEDTN, MTYPI, MSBTI
         IF(MTYPI.NE.011) THEN ! modify non-dictionary messages
            CALL PKBS1(MTYPO,MBAY,'MTYP') ! modify Section 1 type
            CALL PKBS1(MSBTO,MBAY,'MSBT') ! modify Section 1 subtype

            IF(MOD_SEC3_DESC.NE.'NO') THEN

C  Look for top-level sequence descriptor in Sec. 3 that needs to be
C   replaced (initially check first descriptor in bytes 8-9 of Sec. 3)
C  -------------------------------------------------------------------

               IBIT = (IUPBS01(MBAY,'LEN0')+IUPBS01(MBAY,'LEN1')+7)*8
               JBIT = IBIT
               CALL UPB(NVAL,16,MBAY,JBIT)
               ADN30_1 = ADN30(NVAL,6)
               ADN30_2 = 'xxxxx'
               IF(ADN30_1(1:1).NE.'3') THEN

C  ... First descriptor in Sec. 3 (bytes 8-9) does not begin with '3'
C      {most likely is 0-63-000 (byte count)}, not what we want - try
C      second descriptor in Sec. 3 (bytes 10-11)
C      --------------------------------------------------------------

                  IBIT = IBIT + 16
                  JBIT = IBIT
                  CALL UPB(NVAL,16,MBAY,JBIT)
                  ADN30_2 = ADN30(NVAL,6)

C  ...... If second descriptor in Sec. 3 (bytes 10-11) also does not
C         begin with '3', not what we want - time to ABORT
C         ----------------------------------------------------------

                  IF(ADN30_2(1:1).NE.'3') GO TO 906
               ENDIF

C  HAVE LOCATED THE TOP-LEVEL SEQUENCE DESCRIPTOR IN SECTION 3 THAT
C   NEEDS TO BE REPLACED - GO AHEAD AND REPLACE IT
C  ----------------------------------------------------------------

               CALL PKB(IFXY(DESCR),16,MBAY,IBIT)
            ENDIF

            IWT = IWT + 1  ! count the modified messages written out

            IF(IFIRST.EQ.0) THEN
               PRINT *
               PRINT *, 'FIRST NON-DICTIONARY MESSAGE:'
               PRINT *, '--> BUFR EDITION NUMBER = ',IEDTN
               PRINT *, '--> INPUT BUFR MESSAGE TYPE/SUBTYPE = ',MTYPI,
     $          '/', MSBTI
               PRINT *, '----> SEC. 1 MESSAGE  TYPE   CHANGED TO ',MTYPO
               PRINT *, '----> SEC. 1 MESSAGE SUBTYPE CHANGED TO ',MSBTO
               IF(MOD_SEC3_DESC.NE.'NO') THEN
                  PRINT *
                  PRINT *, 'FIRST DESCRIPTOR IN SEC. 3 IS ',ADN30_1
                  IF(ADN30_2.NE.'xxxxx') THEN
                     PRINT *, '   - not what we want, try next one'
                     PRINT *, 'SECOND DESCRIPTOR IN SEC. 3 IS ',ADN30_2
                  ENDIF
                  PRINT *,'   - this is the top-level sequence ',
     $             'descriptor we want to replace'
                  WRITE(6,110) ADN30(NVAL,6),DESCR
  110 FORMAT(/1X,'===> CHANGE TOP-LEVEL SEQUENCE DESCRIPTOR IN SEC. 3 ',
     $ 'FROM "',A,'" TO "',A,'"')
               ELSE
                  WRITE(6,120)
  120 FORMAT(/1X,'===> TOP-LEVEL SEQUENCE DESCRIPTOR IN SEC. 3 **NOT**',
     $ ' CHANGED SINCE SCRIPT VARIABLE "mod_sec3_desc" = "NO"')
               ENDIF
               PRINT *
               PRINT *, 'ALL SUBSEQUENT MESSAGES WILL BE SIMILARLY ',
     $          'MODIFIED'
               PRINT *
               IFIRST = 1
            ELSE IF(MTYPI.NE.MTYPI_last.OR.MSBTI.NE.MSBTI_last) THEN
               PRINT *
               PRINT *, 'INPUT BUFR MESSAGE TYPE/SUBTYPE HAS CHANGED ',
     $          'TO ',MTYPI,MSBTI
               PRINT *, 'BUFR EDITION NUMBER = ',IEDTN
               PRINT *, '--> SEC. 1 MESSAGE  TYPE   STILL CHANGED TO ',
     $          MTYPO
               PRINT *, '--> SEC. 1 MESSAGE SUBTYPE STILL CHANGED TO ',
     $          MSBTO
               IF(MOD_SEC3_DESC.NE.'NO') THEN
                  PRINT *, '--> TOP-LEVEL SEQUENCE DESCRIPTOR IN SEC. ',
     $             '3 STILL CHANGED TO ',DESCR
               ELSE
                  PRINT *, '--> TOP-LEVEL SEQUENCE DESCRIPTOR IN SEC. ',
     $             '3 STILL **NOT** CHANGED SINCE VARIABLE ',
     $             '"mod_sec3_desc" = "NO"'
               ENDIF
               PRINT *
               PRINT *, 'ALL SUBSEQUENT MESSAGES WILL BE SIMILARLY ',
     $          'MODIFIED'
               PRINT *
            ELSE IF(IEDTN.NE.IEDTN_last) THEN
               PRINT *
               PRINT *, 'BUFR EDITION NUMBER HAS CHANGED TO ',IEDTN
               PRINT *, 'INPUT BUFR MESSAGE TYPE/SUBTYPE = ',MTYPI,'/',
     $          MSBTI
               PRINT *, '--> SEC. 1 MESSAGE  TYPE   STILL CHANGED TO ',
     $          MTYPO
               PRINT *, '--> SEC. 1 MESSAGE SUBTYPE STILL CHANGED TO ',
     $          MSBTO
               IF(MOD_SEC3_DESC.NE.'NO') THEN
                  PRINT *, '--> TOP-LEVEL SEQUENCE DESCRIPTOR IN SEC. ',
     $             '3 STILL CHANGED TO ',DESCR
               ELSE
                  PRINT *, '--> TOP-LEVEL SEQUENCE DESCRIPTOR IN SEC. ',
     $             '3 STILL **NOT** CHANGED SINCE VARIABLE ',
     $             '"mod_sec3_desc" = "NO"'
               ENDIF
               PRINT *
               PRINT *, 'ALL SUBSEQUENT MESSAGES WILL BE SIMILARLY ',
     $          'MODIFIED'
               PRINT *
            ENDIF
            IEDTN_last = IEDTN
            MTYPI_last = MTYPI
            MSBTI_last = MSBTI
         ELSE  !  do not modify dictionary msgs, check encode_bufrtable
            IF(ENCODE_BUFRTABLE.EQ.'YES') THEN
               ISK = ISK + 1  ! count the dictionary messages skipped
               PRINT *
               PRINT *, 'DICTIONARY MESSAGE READ IN - SKIP'
               PRINT *
               CYCLE ! skip dictionary messages
            ENDIF
            ICP = ICP + 1  ! count the dictionary messages copied as is
            PRINT *
            PRINT *, 'DICTIONARY MESSAGE READ IN - COPY OUT AS IS (DO ',
     $       'NOT MODIFY)'
            PRINT *
         ENDIF

C  WRITE OUT EACH UPDATED MESSAGE (ALSO UNCHANGED DICTIONARY MESSAGE IF
C  encode_bufrtable IS NOT YES)
C  --------------------------------------------------------------------

         CALL MSGWRT(LUNOT,MBAY,IUPBS01(MBAY,'LENM'))

      ENDDO

  100 CONTINUE

C  ALL INPUT BUFR MESSAGES HAVE BEEN READ, SEE IF WE SHOULD ENCODE
C   EXTERNAL BUFR MNEMONIC TABLE INTO TOP MESSAGES IN OUTPUT BUFR FILE
C  -------------------------------------------------------------------

      PRINT *
      PRINT *, 'ALL INPUT BUFR MESSAGES READ AND MODIFIED'
      PRINT *

      IF(ENCODE_BUFRTABLE.EQ.'YES') THEN
         PRINT *
         PRINT *, 'ENCODE EXTERNAL BUFR MNEMONIC TABLE INTO TOP ',
     $    'MESSAGES IN OUTPUT BUFR FILE'
         PRINT *
         REWIND(LUNDX)
         CALL CLOSBF(LUNOT)
         CALL OPENBF(LUNOT,'IN',LUNDX)
         PRINT *
         PRINT *, 'OPEN UPDATED BUFR FILE IN UNIT ',LUNOT,' NOW AS ',
     $    'INPUT'
         PRINT *
         CALL CLOSBF(LSCRT)
         OPEN(LSCRT,FILE='scratch.wrk',FORM='UNFORMATTED')
         PRINT'(101("=")/"OPENING SCRATCH FILE: scratch.wrk IN UNIT",
     $    I3)',LSCRT
         CALL OPENBF(LSCRT,'OUT',LUNOT)
         PRINT *
         PRINT *, 'OPEN NEW OUTPUT BUFR FILE IN UNIT ',LSCRT,' WITH ',
     $    'ENCODED EXTERNAL BUFR MNEMONIC TABLE FROM UNIT ',LUNDX
         PRINT *
         KMSG = 0
         DO WHILE(IREADMG(LUNOT,SUBSET,IDATE).EQ.0)
            KMSG = KMSG + 1
            PRINT'("READ INPUT MESSAGE",I7,", SUBSET = ",A,", DATE = ",
     $       I10," -- COPY TO OUTPUT FILE")', KMSG,SUBSET,IDATE
            CALL COPYMG(LUNOT,LSCRT)
         ENDDO
         CALL CLOSBF(LUNOT)
         CALL CLOSBF(LSCRT)
         OPEN(LSCRT,FILE='scratch.wrk',FORM='UNFORMATTED')
         CALL COPYBF(LSCRT,LUNOT)
         PRINT'(/"NEW OUTPUT BUFR FILE IN UNIT ",I3," SUCCESSFULLY ",
     $    "COPIED TO ORIGINAL OUTPUT UNIT NUMBER ",I3/)', LSCRT,LUNOT
      ENDIF

C  END OF PROGRAM
C  --------------

      PRINT*,'** PROCESSING ENDED NORMALLY ***'
      PRINT*,'** NUMBER OF BUFR MESSAGES READ :                    ',IRD
      PRINT*,'** NUMBER OF BUFR UPDATED MESSAGES WRITTEN:          ',IWT
      PRINT*,'** NO. OF INPUT (DICTIONARY) BUFR MSGS COPIED AS IS: ',ICP
      PRINT*,'** NO. OF INPUT (DICTIONARY) BUFR MSGS SKIPPED:      ',ISK
      PRINT*,'** PROCESSING ENDED NORMALLY ***'
      IF(IWT.EQ.0) GO TO 904

      CALL W3TAGE('BUFR_TRANMTYPSBT')
      STOP

C  ERROR EXITS
C  -----------

  900 CONTINUE

      WRITE(6,200)
  200 FORMAT(/' #####BUFR_TRANMTYPSBT - EOF/ERR READING ENVIRONMENT ',
     $ 'VARIABLE "TANKFILE" -- DISABLING ALL SUBSEQUENT PROCESSING'/)
      CALL W3TAGE('BUFR_TRANMTYPSBT')
      CALL ERREXIT(253)

  901 CONTINUE

      WRITE(6,201) BUFRTAB,trim(TANK_DIR),trim(FILE_CHOICE2),
     $ trim(FILE_CHOICE3)
  201 FORMAT(/' #####BUFR_TRANMTYPSBT - BUFR MNEMONIC TABLE ',A,' DOES',
     $ ' NOT EXIST IN:'/1X,A,'(First choice)'/17X,'-- or --'/1X,A,
     $ '(Second choice)'/17X,'-- or --'/1X,A,'(Third choice)'/
     $ ' -- DISABLING ALL SUBSEQUENT PROCESSING'/)
      CALL W3TAGE('BUFR_TRANMTYPSBT')
      CALL ERREXIT(253)

  902 CONTINUE

      WRITE(6,202) MTYPO,MSBTO
  202 FORMAT(/' #####BUFR_TRANMTYPSBT - EOF/ERR READING DICTIONARY ',
     $ 'FILE LOOKING FOR "NC',2(I3.3),'" -- DISABLING ALL SUBSEQUENT ',
     $ 'PROCESSING'/)
      CALL W3TAGE('BUFR_TRANMTYPSBT')
      CALL ERREXIT(253)

  903 CONTINUE

      WRITE(6,203)
  203 FORMAT(/' #####BUFR_TRANMTYPSBT - FILE READ IN IS NOT BUFR -- ',
     $ 'DISABLING ALL SUBSEQUENT PROCESSING'/)
      CALL W3TAGE('BUFR_TRANMTYPSBT')
      CALL ERREXIT(253)

  904 CONTINUE

      WRITE(6,204)
  204 FORMAT(/' #####BUFR_TRANMTYPSBT - NO REPORTS PROCESSED -- ',
     $ 'DISABLING ALL SUBSEQUENT PROCESSING'/)
      CALL W3TAGE('BUFR_TRANMTYPSBT')
      CALL ERREXIT(253)

  905 CONTINUE

      WRITE(6,205)
  205 FORMAT(/' #####BUFR_TRANMTYPSBT - OPTIONAL SEC. 2 IS PRESENT, ',
     $ 'CANNOT LOCATE BEGINNING OF SEC. 3 - DISABLING ALL SUBSEQUENT ',
     $ 'PROCESSING'/)
      CALL W3TAGE('BUFR_TRANMTYPSBT')
      CALL ERREXIT(253)

  906 CONTINUE

      WRITE(6,206)
  206 FORMAT(/' #####BUFR_TRANMTYPSBT - CANNOT LOCATE TOP-LEVEL ',
     $ 'SEQUENCE DESCRIPTOR IN SEC. 3 THAT MUST BE REPLACED - ',
     $ 'DISABLING ALL SUBSEQUENT PROCESSING'/)
      CALL W3TAGE('BUFR_TRANMTYPSBT')
      CALL ERREXIT(253)

  907 CONTINUE

      WRITE(6,207) LUBFI
  207 FORMAT(/' #####BUFR_TRANMTYPSBT - ERROR READING IN BUFR MESSAGE ',
     $ 'FROM UNIT',I4,' - DISABLING ALL SUBSEQUENT PROCESSING'/)
      CALL W3TAGE('BUFR_TRANMTYPSBT')
      CALL ERREXIT(253)

      END
