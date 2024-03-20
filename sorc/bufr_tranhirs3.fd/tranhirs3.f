      PROGRAM BUFR_TRANHIRS3
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_TRANHIRS3
C   PRGMMR: DONG             ORG: NP22        DATE: 2019-10-09
C
C ABSTRACT: Read raw HIRS-3 or HIRS-4 1B format file, decode, write
C   selected Tb observations to output BUFR file.
C
C PROGRAM HISTORY LOG:
C 1998-06-15  Treadon  -- Original author
C 2000-09-06  Woollen  -- Added second output in BUFR
C 2000-11-13  Keyser   -- Added error handling when no output is
C       created so that subsequent TRANJB's are skipped
C 2000-11-20  Treadon  -- Changed to properly relabel NOAA-16 satellite
C       id from 2 to 16 to be consistent with the convention followed
C       in the global and regional analysis systems
C 2002-02-11  Woollen  -- Modifications and corrections to output BUFR
C       dataset: "SAID" (0-01-007) corrected to proper WMO Code Table
C       value (was 14 for NOAA-14, etc.), "SIID" (0-02-019) repl.
C       "SIDU" (0-02-021) which didn't seem to be correct, "HMSL"
C       (0-07-002) corrected to proper units of meters (was being
C       stored in km), "LSQL" (0-08-012) corrected to proper WMO Code
C       Table value (0-land/1-sea) (was backwards), "TMBR" (0-12-163)
C       corrected to proper units of K (was being stored as K +
C       273.15), channel 20 "TMBR" set to missing for HIRS-2 and HIRS-3
C       types
C 2002-07-08  Keyser   -- Accounts for NOAA-17 (converts NESDIS sat.
C       no. from 6 to 17)
C 2002-10-23  Treadon  -- Use lbyte instead of mbyte for unpacking
C       counts
C 2004-01-23  Keyser   -- Based on new namelist switch "compress", now
C       has option to write compressed BUFR messages using WRITCP
C       instead of WRITSB (removes the need for the downstream program
C       BUFR_COMPRESS)
C 2005-04-26  Sager    -- Modified to use new atovs1b hirx format:     
C       (1) Tests version type to invoke new format decoding
C       (2) Looks at octet 35 bit 6 to see if any quality flags set
C       (3) If so, tests channel quality flags octet 37-76 to locate
C           bad channels
C 2005-04-29  Keyser   -- Improved Docblocks and comments in code
C 2005-06-21  Keyser   -- Modified to handle processing of NOAA-18
C       HIRS-4 data, processing it into BUFR message type NC021028 (in
C       addition to existing processing of NOAA-15, -16, -17 HIRS-3
C       data into both BUFR message type NC021025 and IEEE), NOAA-18
C       HIRS-4 does not write out into IEEE
C 2005-09-06  Keyser   -- Corrected error in 2005-04-26 change, did
C       not extract octet 35 bit 6 properly - this resulted in
C       unilateral skipping of channel calibration quality flag tests
C       (even if octet 35 bit 6 was set); for NOAA-18, no longer
C       includes calibration quality bit in determining if scan
C       line failed overall Q.C. - this flag is always set because N-18
C       channel 1 HIRS is always bad - instead, falls through and lets
C       code check calibration quality flags for individual channels
C 2006-04-21  Derber   -- Modified to estimate solar and satellite
C       azimuth (via new subroutine SATAZIMUTH), and to update time
C       within a scan line
C 2007-02-09  Keyser   -- Modified to encode the following new
C      information into output BUFR file: estimated solar azimuth
C      (SOLAZI) and estimated satellite azimuth (BEARAZ) for each
C      subset (retrieval) (note: this is not added to output IEEE file
C      for HIRS-3); the "report" time in the BUFR and IEEE files now
C      varies across a scan line as a result of 2006-04-21 change;
C      modified subr. DATTIM to input real double precision second-of-
C      day and output real double precision second-of-minute (both had
C      been integer); array passed into subr. BUFR1B now contains spot
C      hour-of-day, minute-of-hour and second-of-minute rather than
C      only second-of-day (since the hour, minute and second are stored
C      in BUFR, second now rounded to nearest whole second rather than
C      truncated) (IEEE files still store second-of-day for HIRS-3);
C      increased limit for i/o filename length from 80 characters to
C      500 characters; modified to write BUFR code table value
C      0-01-007, the BUFR value for satellite id, into word 1 of output
C      "bdata" array, rather than the actual satellite number as before
C      (this simplifies subroutine BUFR1B which then encodes this value
C      directly into BUFR) (note: the actual satellite number is still
C      written to the output IEEE file); now accounts for new METOP-1
C      satellite (HIRS-4)
C 2007-04-12  Keyser   -- Modified to correct METOP satellite number
C      to METOP-2 with BUFR satellite id = 4 (was incorrectly set to
C      METOP-1 with BUFR satellite id = 3)
C 2009-09-03  Krasowski -- Modified to handle processing of NOAA-19
C      data
C 2009-09-03  Keyser   -- Modified to no longer unilaterally toss a
C      scan line if a calibration anomaly is detected in the
C      calibration quality information (bit 28 in octets 29-32 set,
C      this only means a potential for noise in the data), instead the
C      results from this test are ignored and the "Scan Line Quality
C      Flags" are tested to see if a scan contains marginal calibration
C      in some of its IR channels (bit 6 in octet 35 set), if not then
C      all channels pass, otherwise, code tests the calibration quality
C      bits for each individual channel and if set for a particular
C      channel, that channel is not processed {Note: This overall logic
C      had previously been in place but only for NOAA-18, now it is in
C      place for all satellites (for HIRS-3 or -4) assuming the format
C      is the post-April 2005 version}
C 2012-10-23  Keyser   -- Changes to run on WCOSS. Modified to handle
C      processing of METOP-1 data. Removed IEEE output processing.
C 2014-01-20  Keyser   -- Minor changes
C 2019-09-18  Dong -- Modified to handle processing of METOP-3 data.
C      Modified to shift the Y2K windowing technique that converts
C      2-digit years to 4-digit.
C
C USAGE:
C
C   INPUT FILES:
C     UNIT 05     - Standard input (namelist "input")
C     UNIT 11     - Binary file containing raw 1B HIRS-3 or HIRS-4 data
C     UNIT 12     - BUFR mnemonic table
C     UNIT 41     - Binary file containing topography information
C                   used by function LANSEA
C
C           ***NOTE***
C                   Function LANSEA assumes this information is in
C                   a file named 'lowtopog.dat' which is local to
C                   the working directory.
C
C   OUTPUT FILES:
C     UNIT 06      - Printout
C     UNIT 52      - BUFR file containing HIRS-3 or HIRS-4 Tb data
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - HIRS    CHARS   DATTIM  ICHARS     LANSEA  LBIT
C                  MBYTE   LBYTE   XFLOAT  SATAZIMUTH BUFR1B
C     SYSTEM:    - SYSTEM  GET_ENVIRONMENT_VARIABLE
C     LIBRARY:
C       W3NCO    - W3TAGB  W3TAGE  ERREXIT W3FS26  W3MOVDAT W3DOXDAT
C       BUFRLIB  - OPENBF  CLOSBF  OPENMB  WRITSB  WRITCP   UFBSEQ
C                  MESGBC
C
C
C   EXIT STATES
C     0 = No errors detected
C     1 = Data type id decoded from header is not for HIRS-3 or HIRS-4
C     3 = Problem reading header record of 1b HIRS-3 or HIRS-4 file
C     6 = Unknown satellite id
C     7 = Unknown satellite instrument
C
C REMARKS:
C   Switches read in Namelist INPUT:
C     INFILE     - Path to input 1B data file
C     COMPRESS   - BUFR compression switch (YES or NO)
C     COEFILE    - Path to input coefficient file
C     PROCESS_Tb - Process brightness temps into BUFR files?
C                    (hardwired to YES - can only process Tb)
C     PROCESS_Ta - Process antenna temps into BUFR files?
C                    (hardwired to NO - can only process Tb)
C
C####################################################################
C  NOTE: This program can only process Tb into a BUFR file.  There
C        is no Ta data for HIRS-3 or HIRS-4.  Switches pertaining to
C        the processing of Ta data are included because the parent
C        script also executes BUFR_TRANAMSUA which can process BOTH
C        Tb and Ta data into BUFR files.
C####################################################################
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

C  Declare namelist variables and namelist
C  ---------------------------------------

      integer stdout
      character*500 infile,coefile
      character*8  compress,process_Tb,process_Ta
      namelist /input/ infile,coefile,compress,process_Tb,process_Ta

      common/switches/compress,process_Tb,process_Ta

C  Set I/O unit numbers
C  --------------------

      data lunam, stdout / 5,  6  /
      data lunin         / 11 /

      call w3tagb('BUFR_TRANHIRS3',2019,0282,0068,'NP22')

      print *
      print *, 'WELCOME TO BUFR_TRANHIRS3 - Version 10/09/2019'
      print *

C  Get Namelist input
C  ------------------

      read(lunam,input)

      process_Tb = 'YES'  ! process_Tb is hardwired to YES
      process_Ta = 'NO'   ! process_Ta is hardwired to NO

      write(stdout,*)'namelist input below'
      write(stdout,input)

C  Read/decode/output data records scan by scan
C  --------------------------------------------

      call hirs(lunin,infile)

      call w3tage('BUFR_TRANHIRS3')

      stop
      end

      SUBROUTINE HIRS(LUNIN,RAWHIRS)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    HIRS
C   PRGMMR: DONG             ORG: NP22        DATE: 2019-10-09
C
C ABSTRACT: Read raw HIRS-3 or HIRS-4 1B format file, decode, write
C   selected Tb observations to output BUFR file.
C
C PROGRAM HISTORY LOG:
C 1998-06-15  Treadon  -- Original author
C 2002-07-08  Keyser   -- Accounts for NOAA-17 (converts NESDIS sat.
C       no. from 6 to 17)
C 2005-06-21  Keyser   -- Modified to handle processing of NOAA-18
C       HIRS-4 data, processing it into BUFR message type NC021028 (in
C       addition to existing processing of NOAA-15, -16, -17 HIRS-3
C       data into both BUFR message type NC021025 and IEEE), NOAA-18
C       HIRS-4 does not write out into IEEE
C 2006-04-21  Derber   -- Modified to estimate solar and satellite
C       azimuth (via new subroutine SATAZIMUTH), and to update time
C       within a scan line
C 2006-07-20  Keyser   -- Modified to encode the following new
C      information into output BUFR file: estimated solar azimuth
C      (SOLAZI) and estimated satellite azimuth (BEARAZ) for each
C      subset (retrieval) (note: this is not added to output IEEE file
C      for HIRS-3); the "report" time in the BUFR and IEEE files now
C      varies across a scan line as a result of 2006-04-21 change;
C      increased limit for i/o filename length from 80 characters to
C      500 characters
C 2009-09-03  Krasowski -- Modified to handle processing of NOAA-19
C      data
C 2009-09-03  Keyser   -- Modified to no longer unilaterally toss a
C      scan line if a calibration anomaly is detected in the
C      calibration quality information (bit 28 in octets 29-32 set,
C      this only means a potential for noise in the data), instead the
C      results from this test are ignored and the "Scan Line Quality
C      Flags" are tested to see if a scan contains marginal calibration
C      in some of its IR channels (bit 6 in octet 35 set), if not then
C      all channels pass, otherwise, code tests the calibration quality
C      bits for each individual channel and if set for a particular
C      channel, that channel is not processed {Note: This overall logic
C      had previously been in place but only for NOAA-18, now it is in
C      place for all satellites (for HIRS-3 or -4) assuming the format
C      is the post-April 2005 version}
C 2012-10-23  Keyser   -- Removed IEEE output processing
C 2019-09-18  Dong -- Modified to handle processing of METOP-3 data.
C
C USAGE:    CALL HIRS(LUNIN,RAWHIRS)
C   INPUT ARGUMENT LIST:
C     LUNIN    - Unit connected to raw 1B HIRS-3 or HIRS-4 data file
C     RAWHIRS  - Name of raw 1B HIRS-3 or HIRS-4 data file
C
C   INPUT FILES:
C     UNIT 12     - BUFR mnemonic table
C     UNIT 41     - Binary file containing topography information
C                   used by function LANSEA
C     UNIT LUNIN  - Binary file containing raw 1B HIRS-3 or HIRS-4 data
C
C           ***NOTE***
C                   Function LANSEA assumes this information is in
C                   a file named 'lowtopog.dat' which is local to
C                   the working directory.
C
C   OUTPUT FILES:
C     UNIT 06      - Printout
C     UNIT 52      - BUFR file containing HIRS-3 or HIRS-4 Tb data
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

C  Include machine dependent parameters
C  ------------------------------------

      include 'rfac.inc'

C  Declare/set parameters:
C      NBYTE1  = Total number of bytes (4608) in HIRS-3 or HIRS-4 data
C                record
C      NBYTE4  = Number of 4-byte words in NBYTE1 bytes (4608/4=1152)
C      NSET    = Number of topography datasets for function LANSEA3
C                (not currently used)
C      EPS     = A "small" number
C      MCH     = Number of channels
C      MPOS    = Number of spots (positions) on a scan line

      integer,parameter::real_32=selected_real_kind(6,37)
      integer,parameter::real_64=selected_real_kind(15,307)
      real(real_64) eps
      parameter (nbyte1=4608,nbyte4=nbyte1/4)
      parameter (nset=3)
      parameter (eps=1.d-12)
      parameter (mch=20)
      parameter (mpos=56)

C  Set parameters for structure of output data file
C  ------------------------------------------------

      parameter (nreal=24,nperchan=1,ntot=nreal+nperchan*mch)

C  Declare variables
C  -----------------

      character*1 kbuf(nbyte1),kold
      character*4 indat(nbyte4),jbuf(nbyte4)
      character*40 mapfile(nset)
      character*80 tankfile
      character*500 rawhirs

      integer stdout
      integer(8) itime
      integer newpos(mch),idt(2),ndt(5),idat(8),jdat(8)
      integer ichan(mch),lndsea(mpos),ikeepb(mpos)
      integer imx(nset),jmx(nset),ibadc(mch)

      real(real_64) p1,p2,term1,term2,term3,ta0,b,c
      real(real_64) scale,scale5,scale6,scale9,scale12
      real(real_64) sctime,xsec,counts,rads,rad0
      real(real_64) soza(mpos),saza(mpos),rlocaz(mpos),sazimuth(mpos)
      real(real_64) slat(mpos),slon(mpos),aazimuth(mpos)
      real(real_64) cwave(mch),cnst1(mch),cnst2(mch)
      real(real_64) rad(mch,mpos),tb(mch,mpos),sfchgt(mpos)
      real(real_64) c0(mch),c1(mch),c2(mch)
      real(real_32) bdata(ntot),rinc(5),sctime1
      real(real_64) badr(mch),badtb(mch),badc(mch)

      REAL*8        RDAT(8)
      INTEGER       RDAT1(8)

      logical lnew

C  Declare equivalences
C  --------------------

      equivalence (kbuf(1),jbuf(1))

C  Set information for different resolution map datasets
C  -----------------------------------------------------

      data imx / 360, 720, 1440 /
      data jmx / 181, 361, 721  /
      data mapfile / 'mapdat_100.iee', 'mapdat_050.iee',
     x     'mapdat_025.iee' /

C  Lower/upper limits for gross temperature check on Tb
C  ----------------------------------------------------

      data tlo,thi / 100., 400. /

C  Constants for Planck equation
C  -----------------------------

      data p1,p2 / 1.1910659d-5, 1.438833d0 /

C  Order channel numbers as stored in data file
C  --------------------------------------------

      data newpos/1,17,2,3,13,4,18,11,19,7,8,20,10,14,6,5,15,12,16,9/

C  Missing data flag
C  -----------------

      data rmiss / -999. /

C  Channel numbers
C  ---------------

      data ichan / 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
     x     12, 13, 14, 15, 16, 17, 18, 19, 20 /

C  Set I/O unit numbers (including standard output)
C  ------------------------------------------------

      data stdout / 6/
      data lundx  /12/
      data lubfrb /52/

      data kold /Z'01'/

      call get_environment_variable('TANKFILE',tankfile)

      write(stdout,*)' '
      if(tankfile.ne.'b021/xx028') then
         write(stdout,*)' BEGIN HIRS-3 1B DECODE'
      else
         write(stdout,*)' BEGIN HIRS-4 1B DECODE'
      end if

C  Initialize arrays
C  -----------------

      badr  = 0.
      badtb = 0.
      badc  = 0.
      nprint = 500  ! skip between data record diagnostic prints

C  Write header record to standard output
C  --------------------------------------

      write(stdout,*)' '
      write(stdout,*)'header information below'
      write(stdout,*)'nreal,mch = ',nreal,mch
      write(stdout,*)'ntot      = ',ntot
      write(stdout,*)'channel numbers below'
      write(stdout,*) (ichan(i),i=1,mch)
      write(stdout,*)' '
ccccc if(tankfile.ne.'b021/xx028') then
ccccc    write(99,*) nreal-4,mch,(ichan(i),i=1,mch)
ccccc else
ccccc    write(99,*) nreal,mch,(ichan(i),i=1,mch)
ccccc end if

C  Open output BUFR file
C  ---------------------

ccccc call openbf(lubfrb,'OUT',lundx)
      call openbf(lubfrb,'NODX',lundx)

C  Open unit to raw 1B HIRS-3 or HIRS-4 data file - read header record,
C   see if valid data type - if not, exit routine
C  --------------------------------------------------------------------

      open(lunin,file=rawhirs,recl=nbyte1/rfac,
     &      access='direct',status='old')
      nri = 1
      read (lunin,rec=nri,err=1900) (kbuf(i),i=1,nbyte1)

C  Load header record into work array
C  ----------------------------------

      do i = 1,nbyte4
         indat(i) = jbuf(i)
      end do

C  Test for OLD vs. NEW NESDIS 1B format (NEW after 4/28/2005)
C  -----------------------------------------------------------

      if (kbuf(6) .eq. kold ) then
         lnew = .false.
         print *, 'OLD NESDIS 1B Format'
      else
         lnew = .true.
         print *, 'NEW NESDIS 1B Format'
      end if

C  Extract NOAA spacecraft identification code (72*8+1=577)
C   and convert it into BUFR value (CODE TABLE 0-01-007)
C  --------------------------------------------------------

      jsat  = lbyte(577,16,indat)
      if (jsat.eq.4) then  ! NOAA-15
         jsat0 = jsat
         jsat  = 206
         write(stdout,*) '***WARNING:   reset NOAA-15 satellite id ',
     x        'from ',jsat0,' to ',jsat
      elseif (jsat.eq.2) then  ! NOAA-16
         jsat0 = jsat
         jsat  = 207
         write(stdout,*) '***WARNING:   reset NOAA-16 satellite id ',
     x        'from ',jsat0,' to ',jsat
      elseif (jsat.eq.6) then  ! NOAA-17
         jsat0 = jsat
         jsat  = 208
         write(stdout,*) '***WARNING:   reset NOAA-17 satellite id ',
     x        'from ',jsat0,' to ',jsat
      elseif (jsat.eq.7) then  ! NOAA-18
         jsat0 = jsat
         jsat  = 209
         write(stdout,*) '***WARNING:   reset NOAA-18 satellite id ',
     x        'from ',jsat0,' to ',jsat
      elseif (jsat.eq.8) then  ! NOAA-19
         jsat0 = jsat
         jsat  = 223
         write(stdout,*) '***WARNING:   reset NOAA-19 satellite id ',
     x        'from ',jsat0,' to ',jsat
      else if(jsat.eq.13) then ! METOP-3
         jsat0 = jsat
         jsat  = 5
         write(stdout,*) '***WARNING:   reset METOP-3 satellite id ',
     x        'from ',jsat0,' to ',jsat
      else if(jsat.eq.12) then ! METOP-2
         jsat0 = jsat
         jsat  = 4
         write(stdout,*) '***WARNING:   reset METOP-2 satellite id ',
     x        'from ',jsat0,' to ',jsat
      else if(jsat.eq.11) then ! METOP-1
         jsat0 = jsat
         jsat  = 3
         write(stdout,*) '***WARNING:   reset METOP-1 satellite id ',
     x        'from ',jsat0,' to ',jsat
      else
         write(stdout,*) '***ERROR***   unknown satellite id ',jsat
         call w3tage('BUFR_TRANHIRS3')
         call errexit(6)
      endif

C  Extract data type code (76*8+1=609)
C  -----------------------------------

      jtype = lbyte(609,16,indat)

      if (jtype.ne.5) then
         write(stdout,*)'***ERROR***  Input data file does not contain',
     x    ' HIRS-3 or HIRS-4 data (type=5).  data type = ',jtype
         call w3tage('BUFR_TRANHIRS3')
         call errexit(1)
      endif
      write(stdout,*) 'Data type and BUFR satellite id = ',jtype,jsat

C  Extract number of data records in data set (128*1+1=1025)
C   and number of scans (130*8+1=1041)
C  ---------------------------------------------------------

      nrecs = lbyte(1025,16,indat)
      nscan = lbyte(1041,16,indat)
      write(stdout,*)'nrecs,nscan=',nrecs,nscan

C  Extract coefficients for radiance to temperature conversion
C  -----------------------------------------------------------

      scale5 = 1.d-5
      scale6 = 1.d-6
      do j = 1,mch
         scale = scale6
         if (j.ge.13) scale = scale5
         if (j.le.mch-1) then
            jb  = 521 + (j-1)*12
            jb0 = jb
            jb1 = jb0 + 4
            jb2 = jb1 + 4
            cwave(j) = xfloat(1,kbuf(jb0))*scale
            cnst1(j) = xfloat(1,kbuf(jb1))*scale6
            cnst2(j) = xfloat(1,kbuf(jb2))*scale6
ccccc         write(stdout,*)'cwave = ',j,jb0,xfloat(1,kbuf(jb0))
ccccc         write(stdout,*)'cnst1 = ',j,jb1,xfloat(1,kbuf(jb1))
ccccc         write(stdout,*)'cnst2 = ',j,jb2,xfloat(1,kbuf(jb2))
         else
            jb  = 749
            jb0 = jb
            jb1 = jb0 + 2
            cwave(j) = 0.0
            cnst1(j) = xfloat(1,kbuf(jb0))*scale6
            cnst2(j) = xfloat(1,kbuf(jb1))*scale6
ccccc         write(stdout,*)'cwave = ',j,0.0
ccccc         write(stdout,*)'cnst1 = ',j,jb0,xfloat(1,kbuf(jb0))
ccccc         write(stdout,*)'cnst2 = ',j,jb1,xfloat(1,kbuf(jb1))
         endif
      end do
      write(stdout,*)'chn 1 cwave,cnst1,cnst2=',
     x     cwave(1),cnst1(1),cnst2(1)
      write(stdout,*)' '

C  Prepatory initializations prior to reading in satellite data
C  ------------------------------------------------------------

      nopos   = 0
      nqcbad  = 0
      nqctim  = 0
      nqccal  = 0
      nqcloc  = 0
      nbadc   = 0
      nbadl   = 0
      nbadtb  = 0
      nbadr   = 0
      nrecb   = 0
      nrepb   = 0
      nskipc  = 0
      nskipq  = 0
      nskipm  = 0
      nskiptb = 0
      nlandb  = 0
      nseab   = 0
      nlo = 0

 1200 continue

C**********************************************************************
C                    MAIN LOOP OVER NUMBER OF SCANS
C**********************************************************************

         nri = nri + 1    ! Increment record counter

C  Read data record and load into local work array
C  -----------------------------------------------

         read(lunin,rec=nri,err=1600) (kbuf(i),i=1,nbyte1)

         do i = 1,nbyte4
            indat(i) = jbuf(i)
         end do

         nlo  = nlo + 1   ! Increment line counter
         line = nlo

C  Extract scan type (18*8+1=145) - if scan type is not 0 (earth view),
C   skip this scan line
C    Possible scan types are:
C             0 = earth view
C             1 = space view
C             2 = cold black body (BB) view
C             3 = main BB view
C  --------------------------------------------------------------------

         iscantyp = lbyte(145,16,indat)
         if (iscantyp.ne.0) then
            nskipm = nskipm + 1
            nskipq = nskipq + 1
            write(stdout,2000) nlo,iline,iscantyp,nskipq
 2000              format('***WRONG MODE-SCAN :  nlo=',i6,' iline=',i6,
     x           ' type=',i2,' nskipq=',i6)
            goto 1200
         endif

C  Extract scan line number, start date/time, position, and type
C  -------------------------------------------------------------

         iline  = lbyte(1,16,indat)
         iyear  = lbyte(17,16,indat)  ! (2*8+1=17)
         iddd   = lbyte(33,16,indat)  ! (4*8+1=33)
         itime  = lbyte(65,32,indat)  ! (8*8+1=65)
         idt(1) = iddd          ! day of the year
         idt(2) = iyear         ! 4-digit year
         sctime = 1.d-3*itime   ! second of the day

ccccc    write(stdout,*) 'iline,date=',iline,iyear,iddd,itime,sctime,idt

C  Convert scan start time from year, day-of-year and second-of-day
C   to YYYY,MM,DD,HH,mm,ss
C  ----------------------------------------------------------------

         call dattim(idt,sctime,ndt,xsec)
         rinc   = 0
         idat   = 0
         idat(1:3) = ndt(1:3)
         idat(5:6) = ndt(4:5)
         idat(7)   = xsec
         idat(8)   = mod(xsec*1000._8,1000._8)

C  Extract quality bits - if all good (=0) continue, else skip this scan
C  ---------------------------------------------------------------------

         isum   = 0
         iqcbad = lbyte(225,1,indat)  ! (8*28+1=225)
         iqctim = lbyte(226,1,indat)  ! (8*28+1+1=226)
         iqccal = lbyte(228,1,indat)  ! (8*28+1+3=228)
         iqcloc = lbyte(229,1,indat)  ! (8*28+1+4=229)
         isum   = iqcbad + iqctim + iqccal + iqcloc

C  For NOAA-18, or all satellites if new NESDIS HIRS 1B format, do not
C   include calibration quality bit in determining if scan line failed
C   overall Q.C. (in NOAA-18 this flag is always set because channel 1
C   HIRS is always bad, for any satellite in new format this flag being
C   set only indicates a potential for noise in the data) - instead,
C   fall through and let code check calibration quality flags for
C   individual channels
C  --------------------------------------------------------------------

         if (jsat.eq.209 .or. lnew)  isum   = iqcbad + iqctim + iqcloc

         if (iqcbad.ne.0) nqcbad = nqcbad + 1
         if (iqctim.ne.0) nqctim = nqctim + 1
         if (iqccal.ne.0) nqccal = nqccal + 1
         if (iqcloc.ne.0) nqcloc = nqcloc + 1
         if (isum.ne.0) then
            nskipq = nskipq + 1
            write(stdout,1000) nlo,iline,iqcbad,iqctim,
     x           iqccal,iqcloc,nskipq
 1000              format('***FAIL QC-SCAN :  nlo=',i6,' iline=',i6,
     x           ' bad=',i2,' time=',i2,' cali=',i2,' loc=',i2,
     x           ' nskipq=',i6)
            goto 1200
         endif

         isum = 0

         if (lnew) then

C  If this is new format, check to see if octet 35, bit 6 (scan
c   contains marginal calibration in some of the IR channels) is set
c  -----------------------------------------------------------------

            io35b6 = lbyte(274,1,indat) ! (8*34+1+1=274)
ccc         print *, 'io35b6 = ',io35b6
cppppp
            if(io35b6.ne.0) print *, '~~~Octet 35, bit 6 is ON'
cppppp
            if (io35b6 .ne. 0) then           
C
C   .. if this flag is set, must extract calibration quality bits (and
C       test) for each channel (otherwise skip this and accept all
C       channels) - each channel is in a 2-octet word beginning at
C       octet 37, there are 20 channels
C   ------------------------------------------------------------------

               do jj = 1,mch
                  j = newpos(jj)
                  ibadc(j) = 0
                  jb       = (36 + (j-1)*2)*8+1
                  iqccal   = lbyte(jb,16,indat)
                  if (iqccal .lt. 7) then

C    .. if either space views (bit 1) or blackbody views (bit 2) failed
C       NEDC test (or both) {bit(s) set}, but no other quality bits are
C       set for a particular channel, then this channel passes
C       calibration q.c. test (also true if all bits are not set)
C    -----------------------------------------------------------------

                     iqccal = 0
                  else

C    .. this channel fails calibration q.c. test
C    -------------------------------------------

                     iqc5 = lbyte(jb+10,1,indat)
                     iqc4 = lbyte(jb+11,1,indat)
                     iqc3 = lbyte(jb+12,1,indat)
                     iqc2 = lbyte(jb+13,1,indat)
                     iqc1 = lbyte(jb+14,1,indat)
                     iqc0 = lbyte(jb+15,1,indat)
                     write(stdout,3005) nlo,iline,j,iqccal,iqc0,iqc1,
     x                iqc2,iqc3,iqc4,iqc5
 3005                format('+++FAIL CAL-CHN:  nlo=',i6,' iline=',i6,
     x                    ' chn=',i3,' badcal=',i2,' bits 0-5=',6i2)
                  end if
                  ibadc(j) = iqccal
                  isum     = isum + ibadc(j)
               end do   
               if (isum.ne.0) then
                  nskipc = nskipc + 1
ccc               write(stdout,1005) nlo,iline,(ibadc(j),j=1,mch)
 1005             format('***FAIL CAL :  nlo=',i6,' iline=',i6,
     x                 ' badcal=',20(i2,1x))
               endif
            endif
         else

C  If this is old format, always extract calibration quality bits for
C   each channel (if any quality bit is set, this channel fails
C   calibration q.c. test)
C  ------------------------------------------------------------------

            do jj = 1,mch
               j = newpos(jj)
               ibadc(j) = 0
               jb       = (36 + (j-1)*2)*8+1
               iqccal   = lbyte(jb,16,indat)
               if (iqccal .gt. 0) then
                  iqc5 = lbyte(jb+10,1,indat)
                  iqc4 = lbyte(jb+11,1,indat)
                  iqc3 = lbyte(jb+12,1,indat)
                  iqc2 = lbyte(jb+13,1,indat)
                  iqc1 = lbyte(jb+14,1,indat)
                  iqc0 = lbyte(jb+15,1,indat)
                  write(stdout,3005) nlo,iline,j,iqccal,iqc0,iqc1,
     x                iqc2,iqc3,iqc4,iqc5
               end if
               ibadc(j) = iqccal
               isum     = isum + ibadc(j)
            end do
            if (isum.ne.0) then
               nskipc = nskipc + 1
ccc            write(stdout,1005) nlo,iline,(ibadc(j),j=1,mch)
            endif
         endif

C  Extract calibration coefficients
C  --------------------------------

         scale12 = 1.d-12
         scale9  = 1.d-9
         scale6  = 1.d-6
         do jj = 1,mch
            j = newpos(jj)
            jb2   = 157 + (jj-1)*12
            c2(j) = xfloat(1,kbuf(jb2))*scale12
            jb1   = jb2 + 4
            c1(j) = xfloat(1,kbuf(jb1))*scale9
            jb0   = jb1 + 4
            c0(j) = xfloat(1,kbuf(jb0))*scale6
ccccc     write(stdout,*)'cali c0 = ',jj,j,jb0,xfloat(1,kbuf(jb0)),c0(j)
ccccc     write(stdout,*)'cali c1 = ',jj,j,jb1,xfloat(1,kbuf(jb1)),c1(j)
ccccc     write(stdout,*)'cali c2 = ',jj,j,jb2,xfloat(1,kbuf(jb2)),c2(j)
         end do

C  EXTRACT NAVIGATION DATA
C  -----------------------
C  -----------------------

C  Extract spacecraft altitude (km)
C  --------------------------------

         scale  = 1.d-1
         sathgt = lbyte(5297,16,indat)*scale  ! (662*8+1=5297)

C  Extract angular relationships
C  -----------------------------

         scale = 1.d-2
         do i = 1,mpos
            jb0 = 664*8+1 + (i-1)*48
            jb1 = jb0 + 16
            jb2 = jb1 + 16
            soza(i)  = mbyte(jb0,16,indat)*scale
            saza(i)  = mbyte(jb1,16,indat)*scale
            rlocaz(i)  = mbyte(jb2,16,indat)*scale
ccccc       write(stdout,*)'location = ',i,jb0,jb1,jb2,soza(i),
ccccc  x                   saza(i),rlocaz(i)
         end do

C  Extract earth location data
C  ---------------------------

         scale = 1.d-4
         do i = 1,mpos
            jb0     = 1001 + (i-1)*8
            slat(i) = xfloat(1,kbuf(jb0))*scale
            jb1     = jb0 + 4
            slon(i) = xfloat(1,kbuf(jb1))*scale
            lndsea(i) = rmiss
            sfchgt(i) = rmiss
            ikeepb(i)  = 0
ccccc       write(stdout,*)'latlon = ',i,jb0,jb1,xfloat(1,kbuf(jb0)),
ccccc  x                    xfloat(1,kbuf(jb1)),slat(i),slon(i)
            if ( (abs(slat(i)).gt.90.) .or.
     x           (abs(slon(i)).gt.180.) ) then
               ikeepb(i) = 0
               nbadl    = nbadl + 1
               write(stdout,*)'bad (lat,lon) ',i,slat(i),slon(i)
            elseif ( (abs(slat(i)).le.eps) .and.
     x               (abs(slon(i)).le.eps) ) then
               ikeepb(i) = 0
               nopos    = nopos + 1
            else
               ikeepb(i) = 1

C  Set surface type information based on resolution option
C    If iresol = 1, use 1.0 degree dataset
C       iresol = 2, use 0.5 degree dataset
C       iresol = 3, use 0.25 degree dataset
C  -------------------------------------------------------

ccccc            call lansea3(xlat,xlon,imx(iresol),jmx(iresol),
ccccc  x              mapfile(iresol),rmask,water,elev,stdev)
ccccc            lndsea(i) = rmask + 1.d-3
ccccc            sfchgt(i) = elev

               ils = lansea(slat(i),slon(i),ll)
               if (ils.eq.2) then
                  lndsea(i) = 0
                  sfchgt(i) = 0.0
               elseif (ils.eq.1) then
                  lndsea(i) = 1
                  sfchgt(i) = 1.*ll
               else
                  lndsea(i) = rmiss
                  sfchgt(i) = rmiss
               endif
            endif
         end do

C  Extract HIRS-3 or HIRS-4 counts, then convert counts to radiances
C  -----------------------------------------------------------------

         do i = 1,mpos
            jb = (1460 + (i-1)*48)*8+1
            do jj = 1,mch
               j = newpos(jj)
               jb0 = jb + (jj-1)*16
               counts = lbyte(jb0,16,indat)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cfix
c     98071500:  Tom K. notes that counts-4096 "fixes" problem with
c                hirs-3 radiances.  This problem began 98062800.
c
c     98092300:  Tom K. says offset should be 4095, not 4096
               counts = counts - 4095
cfix
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

               rads   = c0(j) + (c1(j)+c2(j)*counts)*counts
               rad0   = rads
               if (rads.lt.0.) then
                  nbadr   = nbadr + 1
                  badr(j) = badr(j) + 1
                  rads    = rmiss
               endif
               rad(j,i) = rads
ccccc          write(stdout,*)'counts = ',i,jb,jj,jb0,counts,rad0,rads
            end do
         end do

C-----------------------------------------------------------------------
C  Convert radiances to apparent temperature (Ta0), then convert
C   apparent temperature (Ta0) to brightness temperature (Tb)
C   QC all channels - if all channels are bad for a given spot, set
C   flag to omit Tb data in final write
C   Note:  The HIRS-3 and HIRS-4 do not produce an intermediate antenna
C          temperature (Ta)
C-----------------------------------------------------------------------

         do i = 1,mpos
            ibadtb = 0
            do jj = 1,mch
               j     = newpos(jj)
               rads  = rad(j,i)
               term1 = p2*cwave(j)
               tb(j,i) = rmiss
               if ( (rads.gt.eps) .and. (j.ne.mch) ) then
                  term2 = 1 + p1*cwave(j)**3/rads
                  if (term2.le.0) term2 = eps
                  term3   = dlog(term2)
                  ta0     = term1/term3
                  b       = cnst1(j)
                  c       = cnst2(j)
                  tb(j,i) = (ta0-b)/c
               else
                  ta0 = 0.
                  tb(j,i) = rmiss
               endif

C  Apply gross check to Tb using limits TLO and THI set in data stmt
C  -----------------------------------------------------------------

               if ( ((tb(j,i).lt.tlo) .or.
     x               (tb(j,i).gt.thi)) .and.
     x              (j.ne.mch) ) then
                  nbadtb   = nbadtb + 1
                  badtb(j) = badtb(j) + 1
                  tb(j,i) = rmiss
               endif

C  If calibration quality flag for this channel is nonzero, we do not
C   want to use this channel for Tb
C  ------------------------------------------------------------------

               if (ibadc(j).ne.0) then
CC bug, wrong     nbadc   = nbadc + nbadc
                  nbadc   = nbadc + 1
                  badc(j) = badc(j) + 1
                  tb(j,i) = rmiss
               endif

C  Count number of bad channels for current scan position
C  ------------------------------------------------------

               if (tb(j,i).lt.0.) ibadtb = ibadtb + 1

            end do

C  If all Tb channels are bad for current scan position, set keep flag
C   to zero (this tells the code below to not write Tb for this spot
C   to the output files)
C  -------------------------------------------------------------------

            if (ibadtb.eq.mch) then
               nskiptb = nskiptb + 1
               ikeepb(i) = 0
            endif

         end do


C  -------------------------------------------------------------------
C  WRITE HIRS-3 OR HIRS-4 DATA FOR EACH SPOT POS. ON CURRENT SCAN LINE
C  -------------------------------------------------------------------

               CALL W3UTCDAT(RDAT1)
               RDAT(1) = dble(RDAT1(4))
               RDAT(2) = dble(RDAT1(1))
               RDAT(3) = dble(RDAT1(2))
               RDAT(4) = dble(RDAT1(3))
               RDAT(5) = dble(RDAT1(5))
               RDAT(6) = dble(RDAT1(6))

         do i = 1,mpos
            if (ikeepb(i).eq.1) then

C  First, update scan start date time to reflect date time for spot
C  ----------------------------------------------------------------

               rinc(4) = 0.023+(i-1)*.10  ! # of seconds to add to scan
                                          ! start time for this spot
               call w3movdat(rinc,idat,jdat)  ! update date time

C  Calculate updated day-of-year & second-of-day for use by satazimuth
C  -------------------------------------------------------------------

               call w3doxdat(jdat,jdow,jdoy,jday) ! calc. updated DOY
               sctime = jdat(5)*3600 +jdat(6)*60 +jdat(7) +jdat(8)/1000.
               sctime1 = sctime

C  Estimate satellite azimuth angle and solar azimuth angle
C  --------------------------------------------------------

               call satazimuth(jdat(1),jdoy,sctime,slat(i),slon(i),
     x                         rlocaz(i),aazimuth(i),sazimuth(i))
ccccc          write(stdout,*)i,sctime,slat(i),slon(i),aazimuth(i),
cccccx                     sazimuth(i),soza(i)

C  Store output information for this spot
C  --------------------------------------

               bdata(1) = jsat                    ! satellite id
               bdata(2) = jtype                   ! data type indicator
               bdata(3) = jdat(1)                 ! 4-digit year
               bdata(4) = jdat(2)                 ! month
               bdata(5) = jdat(3)                 ! day
               bdata(6) = jdat(5)                 ! hour
               bdata(7) = jdat(6)                 ! minute
               bdata(8) = jdat(7) + jdat(8)/1000. ! second
               bdata(9) = RDAT(1)                  ! RECEIPT TIME SIGNIFICANCE
               bdata(10) = RDAT(2)                 ! YEAR   - TIME OF RECEIPT
               bdata(11) = RDAT(3)                 ! MONTH  - TIME OF RECEIPT
               bdata(12) = RDAT(4)                 ! DAY    - TIME OF RECEIPT
               bdata(13) = RDAT(5)                 ! HOUR   - TIME OF RECEIPT
               bdata(14) = RDAT(6)                 ! MINUTE - TIME OF RECEIPT

               bdata(15) = lndsea(i)               ! land/sea tag
               bdata(16)= i                       ! F-O-V (spot) number
               bdata(17)= slat(i)                 ! latitude
               bdata(18)= slon(i)                 ! longitude
ccccc          bdata(19)= rlocaz(i)
               bdata(19)= saza(i)                 ! sat.  zenith angle
               bdata(20)= soza(i)                 ! solar zenith angle
               bdata(21)= sfchgt(i)               ! surface height
               bdata(22)= sathgt                  ! satellite height
               bdata(23)= sazimuth(i)             ! solar azimuth angle
               bdata(24)= aazimuth(i)             ! sat.  azimuth angle

               if (lndsea(i).lt.0.5) nseab  = nseab + 1
               if (lndsea(i).gt.0.5) nlandb = nlandb + 1
               do j = 1,mch
                  bdata(24+j) = tb(j,i) ! brightness temp
               end do
               nrecb = nrecb + 1
               if(tankfile.ne.'b021/xx028') then 
                  call bufr1b(lubfrb,'NC021025',nreal,mch,bdata,nrepb)
ccccc             write(99,*) (bdata(j),j=1,5),sctime1,
ccccc+             (bdata(j),j=9,nreal-2),
ccccc+             (bdata(j),j=nreal+1,ntot,nperchan)
               else
                  call bufr1b(lubfrb,'NC021028',nreal,mch,bdata,nrepb)
               endif
            endif
         end do

C  Every NPRINT scan lines, print mpos-th record
C  ---------------------------------------------

         if (mod(nlo,nprint).eq.0 .or. nlo.eq.1) then
            write(stdout,*)' '
            write(stdout,*)' Tb data for line,rec=',nlo,nrecb
            write(stdout,*) (bdata(i),i=1,ntot)
            write(stdout,*)' '
         endif

C**********************************************************************
C           DONE WITH THIS SCAN LINE, READ NEXT SCAN LINE
C**********************************************************************

      goto 1200
ctest
ccccc if (nlo.le.30) goto 1200
ctest

 1600 continue

C  All scan lines have been read and processed, summarize
C  ------------------------------------------------------

      write(stdout,*)' '
      write(stdout,*)'Done reading raw 1b file'
      write(stdout,*)' '
      if(tankfile.ne.'b021/xx028') then
         write(stdout,*)'HIRS-3 INGEST STATS:'
      else
         write(stdout,*)'HIRS-4 INGEST STATS:'
      end if
      write(stdout,*)' # of records actually read                  = ',
     $ nlo
      write(stdout,*)' # of records in file as read in header      = ',
     $ nrecs
      write(stdout,*)' # of "good" scans in file as read in header = ',
     $ nscan
      write(stdout,*)' # of scans flagged for non-use (skip)       = ',
     $ nqcbad
      write(stdout,*)' # of scans failing time qc (skip)           = ',
     $ nqctim
      write(stdout,*)' # of scans failing location qc (skip)       = ',
     $ nqcloc
      write(stdout,*)' # of scans with bad scan type mode (skip)   = ',
     $ nskipm
      if (jsat.eq.209 .or. lnew) then
      write(stdout,*)' # of scans with calibration qc noise        = ',
     $ nqccal
      else
      write(stdout,*)' # of scans with calibration qc noise (skip) = ',
     $ nqccal
      end if
      write(stdout,*)' TOTAL # of scans skipped                    = ',
     $ nskipq
      write(stdout,*)' TOTAL # of channels failing calibration qc  = ',
     $ nskipc
      write(stdout,*)' TOTAL # of scan positions with bad lat,lon  = ',
     $ nbadl
      write(stdout,*)' TOTAL # of scan positions with zero lat,lon = ',
     $ nopos
      write(stdout,*)' TOTAL # of channels with bad radiances      = ',
     $ nbadr
      write(stdout,*)' TOTAL # of channels with bad calibration    = ',
     $ nbadc
      write(stdout,*)' TOTAL # of scan positions with bad Tb value = ',
     $ nskiptb
      write(stdout,*)' TOTAL # of channels with bad Tb value       = ',
     $ nbadtb
      write(stdout,*)' # of Tb reports passed into BUFR encoder    = ',
     $ nrecb
      write(stdout,*)' -- # of land Tb reports                     = ',
     $ nlandb
      write(stdout,*)' -- # of sea  Tb reports                     = ',
     $ nseab
      write(stdout,*)' # of Tb BUFR reports (subsets) written      = ',
     $ nrepb

      write(stdout,*)' '
      write(stdout,*)'bad radiance,temperature,calibration counts per ',
     $ 'channel'
      write(stdout,1020)
 1020 format(t1,'channel',t10,'  bad rad',t20,'  bad Tb',t30,
     $ '  bad calib.')
      sumr  = 0.
      sumtb = 0.
      sumc  = 0.
      do jj = 1,mch
         j = newpos(jj)
         write(stdout,1030) j,nint(badr(j)),nint(badtb(j)),nint(badc(j))
 1030    format(t1,i2,t10,I6,t20,I6,t30,I6)
         sumr  = sumr  + badr(j)
         sumtb = sumtb + badtb(j)
         sumc  = sumc  + badc(j)
      end do
      write(stdout,*)'nbadr,nbadtb,nbadc=',sumr,sumtb,sumc

      write(stdout,*)' '
      if(tankfile.ne.'b021/xx028') then
         write(stdout,*)' HIRS-3 1B DECODE COMPLETED'
      else
         write(stdout,*)' HIRS-4 1B DECODE COMPLETED'
      end if
      write(stdout,*)' '

C  Close UNITs
C  -----------

      close(lunin)
      call closbf(lubfrb)

      call system('echo YES > Tb')
      if(nrecb.eq.0) then
         write(stdout,1003)
 1003    format(/' NO Tb RECORDS WRITTEN -- DISABLING ALL SUBSEQUENT ',
     .    'Tb PROCESSING.'/)
         call system('echo NO > Tb')
      else
         call mesgbc(lubfrb,msgt,icomp)
         if(icomp.eq.1) then
            print'(/"OUTPUT Tb BUFR FILE MESSAGES   '//
     .       'C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND IS",I5/)',
     .       msgt
         elseif(icomp.eq.0) then
            print'(/"OUTPUT Tb BUFR FILE MESSAGES   '//
     .       'U N C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND IS",'//
     .       'I5/)',  msgt
         elseif(icomp.eq.-1) then
            print'(//"ERROR READING OUTPUT Tb BUFR FILE - MESSAGE '//
     .       'COMPRESSION UNKNOWN"/)'
         elseif(icomp.eq.-3) then
            print'(/"OUTPUT Tb BUFR FILE DOES NOT EXIST"/)'
         elseif(icomp.eq.-2) then
            print'(/"OUTPUT Tb BUFR FILE HAS NO DATA MESSAGES"/'//
     .       '"FIRST MESSAGE TYPE FOUND IS",I5/)', msgt
         endif
      endif

      close(lubfrb)

      return

C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C                                 ERRORS
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C  Error reading 1B file header record
C  -----------------------------------

 1900 continue
      write(stdout,*)' *** error reading hdr record of file ',rawhirs
      close(lunin)
      call closbf(lubfrb)
      call w3tage('BUFR_TRANHIRS3')
      call errexit(3)

      end

      SUBROUTINE CHARS(IWORD,LEN,CWORD)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    CHARS
C   PRGMMR: BERT KATZ        ORG: NP20       DATE: 1997-11-06
C
C ABSTRACT: Turns integer into character string of specified length,
C   starting at low-order byte of integer.
C
C PROGRAM HISTORY LOG:
C 1997-11-06  Katz -- Original author
C
C USAGE:    CALL CHARS(IWORD,LEN,CWORD)
C   INPUT ARGUMENT LIST:
C     IWORD    - INTEGER argument
C     LEN      - INTEGER argument holding number of low-order bytes
C                of first argument to convert into character
C
C   OUTPUT ARGUMENT LIST:
C     CWORD    - CHARACTER argument
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      character*1 cword(len)
      integer iword
      do ic = len , 1 , -1
        ibeg = (len - ic) * 8
        ichr = ibits(iword,ibeg,8)
        cword(ic) = char(ichr)
      enddo
      return
      end

      SUBROUTINE DATTIM(IDT,SCTIME,NDT,XSEC)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    DATTIM
C   PRGMMR: KEYSER           ORG: NP22       DATE: 2006-07-20
C
C ABSTRACT: Converts year, day-of-year, and second-of-day into year,
C   month-of-year, day-of-month, hour-of-day, minute-of-hour, and
C   second-of-minute.
C
C PROGRAM HISTORY LOG:
C 1997-11-06  Katz   -- Original author
C 2006-07-20  Keyser -- Modified to input real double precision
C      second-of-day and output real double precision second-of-minute
C      (both had been integer)
C
C USAGE:    CALL DATTIM(IDT,SCTIME,NDT,XSEC)
C   INPUT ARGUMENT LIST:
C     IDT      - INTEGER array argument containing two members:
C                day-of-year and year.
C     SCTIME   - REAL*8 argument containing second-of-day.
C
C   OUTPUT ARGUMENT LIST:
C     NDT      - INTEGER array argument containing five members:
C                year, month-of-year, day-of-month, hour-of-day,
C                and minute-of-hour.
C     XSEC     - REAL*8 argument containing second-of-minute.
C
C REMARKS:
C   NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      integer,parameter::real_64=selected_real_kind(15,307)

      integer idt(2),ndt(5)
      integer iday,ihr,imin,imon,isec,iyr,jday,jsec
      real(real_64) sctime,xsec

      external w3fs26

      intrinsic mod

       JULIAN(IYR,IDYR) = -31739 + 1461 * (IYR + 4799) / 4
     &                    -3 * ((IYR + 4899) / 100) / 4 + IDYR

      jday = idt(1)
      iyr  = idt(2)

C  If year is two digits, convert to 4 digits
c  iyr=00-40; then kyr=2000+iyr=2000-2040
c  iyr=41-99; then kyr=1900+iyr=1941-1999
C  ------------------------------------------

      if (iyr.ge.0.and.iyr.le.99) then
         if (iyr.lt.41) then
            kyr = iyr + 2000
         else
            kyr = iyr + 1900
         endif
      else
         kyr = iyr
      endif

C  Compute julian day number as number days after 4713 bc
C  ------------------------------------------------------

      idy = jday
      jdn = julian(kyr,idy)

      call w3fs26(jdn,iyear,jmo,jda,idaywk,idayyr)

      imon = jmo
      iday = jda
      jsec = sctime
      ihr = sctime/3600
      imin = mod(sctime,3600._8)/60
      xsec = sctime - 3600*ihr - 60*imin
      ndt(1) = iyr
      ndt(2) = imon
      ndt(3) = iday
      ndt(4) = ihr
      ndt(5) = imin
      return
      end

      INTEGER FUNCTION ICHARS(CWORD,LEN)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    ICHARS
C   PRGMMR: NADIGA           ORG: NP20       DATE: 2019-10-09
C
C ABSTRACT: Turns character string of specified length into integer.
C
C PROGRAM HISTORY LOG:
C 1997-11-05  Katz -- Original author
C 2019-10-09  Nadiga -- Modified to shift the Y2K windowing technique
C that converts 2-digit years to 4-digit.
C
C USAGE:    ICHARS(CWORD,LEN)
C   INPUT ARGUMENT LIST:
C     CWORD    - CHARACTER*1 array argument
C     LEN      - INTEGER argument holding length of cword
C
C REMARKS:
C   NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      character*1 cword(len)
      lchars = 0
      do ic = len , 1 , -1
        ibeg = (len - ic) * 8
        icmove = mova2i(cword(ic))
        call mvbits(icmove,0,8,lchars,ibeg)
      enddo
      ichars = lchars
      return
      end
cfpp$ expand(ichars,lbit)

      INTEGER FUNCTION LANSEA(RLAT,RLON,LEVEL)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    LANSEA
C   PRGMMR: BERT KATZ        ORG: NP20       DATE: 1997-11-05
C
C ABSTRACT: Calculates topography, land/sea status from latitude and
C   longitude.
C
C PROGRAM HISTORY LOG:
C 1997-11-05  Katz -- Original author
C
C USAGE:    LANSEA(RLAT,RLON,LEVEL)
C   INPUT ARGUMENT LIST:
C     RLAT     - INTEGER argument containing scaled latitude
C     RLON     - INTEGER argument containing scaled longitude
C
C   OUTPUT ARGUMENT LIST:
C     LEVEL    - INTEGER argument containing scaled topography
C
C   INPUT FILES:
C     UNIT 41  - Binary low-resolution topography file
C
C REMARKS:
C   NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      include 'rfac.inc'

      integer,parameter::real_64=selected_real_kind(15,307)
      integer ilat,ilon,level
      real(real_64) rlat,rlon
      real slat,slon,xlon
      integer iopn,iu,lan,last,lat,lenr,lon
      character*12 name
      character*4 iflag(12),kelev(192)
      character*2 ielev(360)

      integer lbit,ichars

      external lbit,ichars

      intrinsic float,max0

      equivalence (iflag(1),kelev(1)), (ielev(1),kelev(13))

      data name/'lowtopog.dat'/,iu/41/,lenr/768/,last/0/,iopn/0/

      save iopn,kelev,last

      if (iopn.eq.0) then
        open (iu,recl=lenr/rfac,
     &        file=name,access='direct',status='old')
        iopn = 1
      endif
      lan = 0
      level = 0
      slat = rlat
      slon = rlon
      lat = slat + 1.
      if (slat.lt.0.) lat = slat
      lat = max0(lat,-87)
      lat = 91 - lat
      if (lat.eq.last) go to 100
      read (iu,rec=lat) kelev
      last = lat
  100 xlon = slon
      if (xlon.lt.0.) xlon = xlon + 360.
      lon = xlon
      if (lon.eq.360) lon = 0
      lon = lon + 1
      lan = lbit(lon,iflag)
      if (lan.ne.0) then
        ltemp = ichars(ielev(lon),2)
        if (btest(ltemp,15)) then
          ltemp = ior(ltemp,-65536)
        endif
        level = ltemp
      endif
      lansea = 2 - lan
      return
      end
cfpp$ expand(ichars)

      INTEGER FUNCTION LBIT(J,ARRAY)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    LBIT
C   PRGMMR: BERT KATZ        ORG: NP20       DATE: 1997-11-05
C
C ABSTRACT: Extracts j'th bit from array of CHARACTER*4.
C
C PROGRAM HISTORY LOG:
C 1997-11-05  Katz -- Original author
C
C USAGE:    LBIT(J,ARRAY)
C   INPUT ARGUMENT LIST:
C     J        - INTEGER argument
C     ARRAY    - CHARACTER*4 array argument
C
C REMARKS:
C   NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      integer j
      character*4 array(*)
      integer ibit,jout,jw,jword,nbit

      integer ichars
      external ichars

      intrinsic btest

      jw = (j-1)/32
      nbit = j - jw*32
      jword = ichars(array(jw+1),4)
      ibit = 32 - nbit
      jout = 0
      if (btest(jword,ibit)) jout = 1
      lbit = jout
      return
      end
cfpp$ expand(ichars)

      INTEGER FUNCTION MBYTE(J,LENGTH,JARRAY)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    MBYTE
C   PRGMMR: BERT KATZ        ORG: NP20       DATE: 1997-11-05
C
C ABSTRACT: Extracts bit string from array of CHARACTER*4 and
C   converts it to INTEGER.  Entry point MBYTE propagates sign bit
C   in result; entry point LBYTE does not.
C
C PROGRAM HISTORY LOG:
C 1997-11-05  Katz -- Original author
C
C USAGE:    MBYTE(J,LENGTH,JARRAY)
C   INPUT ARGUMENT LIST:
C     J        - INTEGER argument containing starting bit
C     LENGTH   - integer argument containing number of bits
C                (maximum value 32)
C     JARRAY   - CHARACTER*4 array argument
C
C   OUTPUT FILES:
C     UNIT 06  - printout
C
C REMARKS:
C   NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      integer j,length
      character*4 jarray(*)
      integer inleft,jbit,kompl,mflag,n,nlj,nrj,
     +        nsh,nword
      integer(8) item,jleft,jrite,mask
      integer(8)  kounts(33)

      integer ichars
      external ichars

      intrinsic iand,ior,mod

      integer lbyte

      data kounts/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,
     +     16384,32768,65536,131072,262144,524288,1048576,2097152,
     +     4194304,8388608,16777216,33554432,67108864,134217728,
     +     268435456,536870912,1073741824,2147483648_8,0/

C  ENTRY MBYTE
C  -----------

      mflag = 1
  110 nword = (j-1)/32 + 1
      if (length.lt.1 .or. length.gt.32) write (*,fmt=120) length
  120 format (' improper byte length in mbyte or lbyte',i10)
      nlj = mod(j-1,32)
      nrj = 32 - length - nlj
      if (nrj.lt.0) go to 150
      item = ichars(jarray(nword),4)
      kompl = 33 - length - nlj
      mask = -kounts(kompl)
      item = iand(item,mask)
      item = item/kounts(nrj+1)
      mask = kounts(length+1) - 1
      item = iand(item,mask)
  130 if (mflag.eq.0) go to 140

c ... means logical byte

      mbyte = item
      jbit = iand(kounts(length),item)
      if (jbit.eq.0) return

c ... need sign extension

      mask = -mask - 1
      item = ior(item,mask)
      mbyte = item
      return

C  ENTRY LBYTE
C  -----------

      entry lbyte(j,length,jarray)

      mflag = 0
      go to 110

  140 lbyte = item
      return

c ... byte spans two words

  150 inleft = length + nrj
      mask = kounts(inleft+1) - 1
      jleft = ichars(jarray(nword),4)
      jleft = iand(jleft,mask)
      nsh = 1 - nrj
      jleft = jleft*kounts(nsh)
      n = 1 - nrj
      jrite = ichars(jarray(nword+1),4)
      kompl = 33 + nrj
      mask = -kounts(kompl)
      jrite = iand(jrite,mask)
      jrite = jrite/kounts(nrj+33)
      mask = kounts(n) - 1
      jrite = iand(jrite,mask)
      item = ior(jrite,jleft)
      mask = kounts(length+1) - 1
      go to 130
      end
cfpp$ expand(ichars)

      REAL FUNCTION XFLOAT(JB,IARRAY)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    XFLOAT
C   PRGMMR: BERT KATZ        ORG: NP20       DATE: 1997-11-05
C
C ABSTRACT: Takes two consecutive elements of CHARACTER*2 array and
C   forms a floating point number from them.
C
C PROGRAM HISTORY LOG:
C 1997-11-05  Katz -- Original author
C
C USAGE:    XFLOAT(JB,IARRAY)
C   INPUT ARGUMENT LIST:
C     JB       - INTEGER argument containing array location
C     IARRAY   - CHARACTER*2 array argument
C
C REMARKS:
C   NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      integer jb
      integer*8 mask
      data mask/x'ffffffff00000000'/
ccccc character*2 iarray(2)
      character*2 iarray(2000)
      character*4 conv
      real xf
      integer j
      integer*8 jj
      integer in(2)

      integer ichars
      external ichars

      intrinsic btest,ior

      j = jb
      conv(1:2) = iarray(j)
      conv(3:4) = iarray(j+1)
      jj = ichars(conv,4)
      if (btest(jj,31_8)) then
        jj = ior(jj,mask)
      endif
      xf = jj
      xfloat = xf
      return
      end

      SUBROUTINE SATAZIMUTH(IYEAR,IDAY,STIME,SLAT,SLON,RAZIMUTH,
     +           AAZIMUTH,SAZIMUTH)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SATAZIMUTH
C   PRGMMR: DERBER           ORG: NP2        DATE: 2006-04-06
C
C ABSTRACT: Calculates satellite azimuth angle and solar azimuth angle
C   given the relative azimuth angle and the date/time.
C
C PROGRAM HISTORY LOG:
C 2006-04-06  Derber - Original author
C
C USAGE:    CALL SATAZIMUTH(IYEAR,IDAY,STIME,RAZIMUTH,SLAT,SLON,
C                           AAZIMUTH,SAZIMUTH)
C   INPUT ARGUMENT LIST:
C     IYEAR    - Year
C     IDAY     - Day of year
C     STIME    - Time of day (seconds)
C     SLAT     - Latitude (degrees N)
C     SLON     - Longitude (degrees, + E, - W)
C     RAZIMUTH - Relative azimuth angle (degrees true)
C
C   OUTPUT ARGUMENT LIST:
C     AAZIMUTH - Satellite azimuth angle (degrees true, 0-360)
C     SAZIMUTH - Solar azimuth angle (degrees true, 0-360)
C
C REMARKS:
C   NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      integer,parameter::real_64=selected_real_kind(15,307)
      real(real_64) slon,slat,tcen,omega,gmeananomsun,sec,y
      real(real_64) razimuth,aazimuth,deg2rad,rad2deg
      real(real_64) sinm,sin2m,sin3m,suneqofcenter,geommeanlongsun
      real(real_64) suntruelon,sunapparentlong,meanobliquity
      real(real_64) obliquitycorrection,decl,ayr
      real(real_64) byr,sin210,sin410,cos210,eccearthorbit
      real(real_64) eqtime,tst,sazimuth,sozanew,stime
      integer iday

      external w3fs26

      ajday(byr,jday) = floor(365.25*(byr+4716.)) -
     +    floor(byr/100.)+
     +    floor(float(floor(byr/100.))/4.)-2452639.5+float(jday)
      deg2rad = acos(-1.)/180.
      rad2deg = 1./deg2rad

      ayr  = float(iyear-1)

C  Calculate relative time to 2000.0 (in centuries)
C  ------------------------------------------------

      tcen = (ajday(ayr,iday)+stime/86400.)/36525.0
      omega=(125.04-1934.136*tcen)*deg2rad
      gmeananomsun=(357.52911+tcen*(35999.05029-
     +           0.0001537*tcen))*deg2rad
      sinm=sin(gmeananomsun)
      sin2m=sin(2.0*gmeananomsun)
      sin3m=sin(3.0*gmeananomsun)
      suneqofcenter=(sinm*(1.914602-
     + tcen*(0.004817+0.000014*tcen))+sin2m*(0.019993-
     + 0.000101*tcen)+sin3m*0.000289)*deg2rad
      geommeanlongsun=280.46646+tcen*(36000.76983+0.0003032*tcen)
      if(geommeanlongsun > 360.0) geommeanlongsun=geommeanlongsun-360.
      if(geommeanlongsun < 0.0) geommeanlongsun=geommeanlongsun+360.
      geommeanlongsun=deg2rad*geommeanlongsun
      suntruelon=geommeanlongsun+suneqofcenter
      sunapparentlong=suntruelon-(0.00569-0.00478*sin(omega))*deg2rad
      sec=21.448-tcen*(46.8150+tcen*(0.00059-tcen*0.001813))
      meanobliquity=23.0+(26.0+(sec/60.))/60.
      obliquitycorrection= (meanobliquity + 0.00256*cos(omega))*deg2rad
      decl = asin(sin(obliquitycorrection)*sin(sunapparentlong))
ccccc solarrightascension = atan2(cos(obliquitycorrection)*  
ccccc+       sin(sunapparentlong),cos(sunapparentlong))
      y = tan(obliquitycorrection/2.0)
      y = y*y
      sin210=sin(2.0*geommeanlongsun)
      sin410=sin(4.0*geommeanlongsun)
      cos210=cos(2.0*geommeanlongsun)
      eccearthorbit=0.016708634-tcen*(0.000042037+
     +    0.0000001287*tcen)
      eqtime=(y*sin210
     +    -2.0*eccearthorbit*sinm 
     +    +4.0*eccearthorbit*y*sinm*cos210
     +    -0.5*y*y*sin410 
     +    -1.25*eccearthorbit*eccearthorbit*sin2m)*4.0*rad2deg

      tst=eqtime+4.*slon+stime/60.
      ha=(tst/4.0)-180.
      if(ha > 180.)ha=ha-360.
      if(ha < -180.)ha=360.+ha
      sozanew=acos(sin(deg2rad*slat)*sin(decl)+cos(deg2rad*slat)*
     +    cos(decl)*cos(deg2rad*ha))
      sazimuth = acos((sin(decl)-sin(deg2rad*slat)*cos(sozanew))/
     +    (cos(deg2rad*slat)*sin(sozanew)))*rad2deg
      if(ha > 0.)sazimuth=-sazimuth
ccccc if(sazimuth >  180.)sazimuth=sazimuth-360.
ccccc if(sazimuth < -180.)sazimuth=360.+sazimuth

      aazimuth= razimuth+sazimuth
      if(aazimuth >  180.)aazimuth=aazimuth-360.
      if(aazimuth < -180.)aazimuth=360.+aazimuth

C  Correct aziumuth angles to be 0-360 degrees true (for BUFR)
C  -----------------------------------------------------------

      if(aazimuth <  0.)aazimuth=360.+aazimuth
      if(sazimuth <  0.)sazimuth=360.+sazimuth

      return
      end
