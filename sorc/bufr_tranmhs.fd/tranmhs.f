      PROGRAM BUFR_TRANMHS
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_TRANMHS
C   PRGMMR: NADIGA         ORG: NP22        DATE: 2019-10-09
C
C ABSTRACT: Read raw MHS 1B format file, decode, write selected Tb
C   observations to output BUFR file.
C
C PROGRAM HISTORY LOG:
C 2005-06-21  Keyser/Derber   -- Original authors
C 2006-04-21  Derber   -- Modified to estimate solar and satellite
C       azimuth (via new subroutine SATAZIMUTH), and to update time
C       within a scan line
C 2007-02-09  Keyser   -- Modified to encode the following new
C      information into output BUFR file: estimated solar azimuth
C      (SOLAZI) and estimated satellite azimuth (BEARAZ) for each
C      subset (retrieval), and cold space temperature correction (CSTC)
C      for each channel in subset; the "report" time in the BUFR file
C      now varies across a scan line as a result of 2006-04-21 change;
C      modified subr. DATTIM to input real double precision second-of-
C      day and output real double precision second-of-minute (both had
C      been integer); array passed into subr. BUFR1B now contains spot
C      hour-of-day, minute-of-hour and second-of-minute rather than
C      only second-of-day (since the hour, minute and second are stored
C      in BUFR, second now rounded to nearest whole second rather than
C      truncated); increased limit for i/o filename length from 80
C      characters to 500 characters; modified to write BUFR code table
C      value 0-01-007, the BUFR value for satellite id, into word 1 of
C      output "bdata" array, rather than the actual satellite number as
C      before (this simplifies subroutine BUFR1B which then encodes
C      this value directly into BUFR); now accounts for new METOP-1
C      satellite
C 2007-04-12  Keyser   -- Modified to correct METOP satellite number
C      to METOP-2 with BUFR satellite id = 4 (was incorrectly set to
C      METOP-1 with BUFR satellite id = 3)
C 2009-05-08  Krasowski -- Modified to handle processing of NOAA-19
C      data
C 2012-10-23  Keyser   -- Changes to run on WCOSS. Modified to handle
C      processing of METOP-1 data
C 2014-01-20  Keyser   -- Minor changes
C 2018-12-08  Ling -- Modified to handle processing of METOP-3 data
C 2019-10-09  Nadiga -- Modified to shift the Y2K windowing technique
C      that converts 2-digit years to 4-digit.
C
C USAGE:
C
C   INPUT FILES:
C     UNIT 05     - Standard input (namelist "input")
C     UNIT 11     - Binary file containing raw 1B MHS data
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
C     UNIT 52      - BUFR file containing MHS Tb data
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - MHS     CHARS   DATTIM  ICHARS     LANSEA  LBIT
C                  MBYTE   LBYTE   XFLOAT  SATAZIMUTH BUFR1B
C     SYSTEM:    - SYSTEM
C     LIBRARY:
C       W3NCO    - W3TAGB  W3TAGE  ERREXIT W3FS26  W3MOVDAT W3DOXDAT
C       BUFRLIB  - OPENBF  CLOSBF  OPENMB  WRITSB  WRITCP   UFBSEQ
C                  MESGBC
C
C
C   EXIT STATES
C     0 = No errors detected
C     1 = Data type id decoded from header is not for MHS
C     3 = Problem reading header record of 1b MHS file
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
C#####################################################################
C  NOTE: This program can only process Tb into a BUFR file.  There
C        is no Ta data for MHS.  Switches pertaining to the processing
C        of Ta data are included because the parent script also
C        executes BUFR_TRANAMSUA which can process BOTH Tb and Ta
C        data into BUFR files.
C#####################################################################
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

      call w3tagb('BUFR_TRANMHS',2019,0282,0068,'NP22')

      print *
      print *, 'WELCOME TO BUFR_TRANMHS - Version 10/09/2019'
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

      call mhs(lunin,infile)

      call w3tage('BUFR_TRANMHS')

      stop
      end

      SUBROUTINE MHS(LUNIN,RAWMHS)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    MHS
C   PRGMMR: KRASOWSKI      ORG: NP22       DATE: 2009-05-08
C
C ABSTRACT: Read raw MHS 1B format file, decode, write selected Tb
C   observations to output BUFR file.
C
C PROGRAM HISTORY LOG:
C 2005-06-21  Keyser   -- Original author
C 2006-04-21  Derber   -- Modified to estimate solar and satellite
C       azimuth (via new subroutine SATAZIMUTH), and to update time
C       within a scan line
C 2006-07-20  Keyser   -- Modified to encode the following new
C      information into output BUFR file: estimated solar azimuth
C      (SOLAZI) and estimated satellite azimuth (BEARAZ) for each
C      subset (retrieval), and cold space temperature correction (CSTC)
C      for each channel in subset; the "report" time in the BUFR file
C      now varies across a scan line as a result of 2006-04-21 change;
C      increased limit for i/o filename length from 80 characters to
C      500 characters
C 2009-05-08  Krasowski -- Modified to handle processing of NOAA-19
C      data
C
C USAGE:    CALL MHS(LUNIN,RAWMHS)
C   INPUT ARGUMENT LIST:
C     LUNIN    - Unit connected to raw 1B MHS data file
C     RAWMHS   - Name of raw 1B MHS data file
C
C   INPUT FILES:
C     UNIT 12     - BUFR mnemonic table
C     UNIT 41     - Binary file containing topography information
C                   used by function LANSEA
C     UNIT LUNIN  - Binary file containing raw 1B MHS data
C
C           ***NOTE***
C                   Function LANSEA assumes this information is in
C                   a file named 'lowtopog.dat' which is local to
C                   the working directory.
C
C   OUTPUT FILES:
C     UNIT 06      - Printout
C     UNIT 52      - BUFR file containing MHS Tb data
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
C      NBYTE1  = Total number of bytes (3072) in MHS data record
C      NBYTE4  = Number of 4-byte words in NBYTE1 bytes (3072/4=768)
C      NSET    = Number of topography datasets for function LANSEA3
C                (not currently used)
C      EPS     = A "small" number
C      MCH     = Number of channels
C      MPOS    = Number of spots (positions) on a scan line
C      NTX     = Transmitter number
C      NFOV    = Bias correction number from data

      integer,parameter::real_32=selected_real_kind(6,37)
      integer,parameter::real_64=selected_real_kind(15,307)
      real(real_64) eps
      parameter (nbyte1=3072,nbyte4=nbyte1/4)
      parameter (nset=3)
      parameter (eps=1.d-12)
      parameter (mch=5)
      parameter (mpos=90)
      parameter (ntx=4)
      parameter (nfov=19)

C  Set parameters for structure of output data file
C  ------------------------------------------------

      parameter (nreal=18,nperchan=2,ntot=nreal+nperchan*mch)

C  Declare variables
C  -----------------

      character*1 kbuf(nbyte1)
      character*3 chan(mch)
      character*4 indat(nbyte4),jbuf(nbyte4)
      character*40 mapfile(nset)
      character*500 rawmhs

      integer stdout
      integer(8) itime
      integer idt(2),ndt(5),idat(8),jdat(8)
      integer lndsea(mpos),ikeepb(mpos)
      integer imx(nset),jmx(nset),ibadc(mch)
      integer ibiascorr(mch,nfov,ntx)
      integer icorrtab(mch,mpos,ntx)
      integer itranrefpow(ntx)
      integer itranpow(ntx)

      real(real_64) p1,p2,term1,term2,term3,ta0,b,c
      real(real_64) sctime,xsec,counts,rads,sathgt
      real(real_64) scale,scale6,scale10,scale16
      real(real_64) slat(mpos),slon(mpos),tshelfb,tshelfp
      real(real_64) cwave(mch),cnst1(mch),cnst2(mch)
      real(real_64) cfrq0(mch),tref(mch),dt0(mch),dt1(mch),dt2(mch)
      real(real_64) rad(mch,mpos),tb(mch,mpos),sfchgt(mpos)
      real(real_64) c0(mch),c1(mch),c2(mch),aazimuth(mpos)
      real(real_64) saza(mpos),soza(mpos),rlocaz(mpos),sazimuth(mpos)
      real(real_32) bdata(ntot),rinc(5),sctime1
      real(real_64) badr(mch),badtb(mch)
      real(real_64) grad(5,19,4)

      double precision two22,two30,two44

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

C  Missing data flag
C  -----------------

      data rmiss / -999. /

C  Channel numbers
C  ---------------

      data chan / ' H1', ' H2', ' H3', ' H4', ' H5' /

C  Set I/O unit numbers (including standard output)
C  ------------------------------------------------

      data stdout / 6/
      data lundx  /12/
      data lubfrb /52/

      write(stdout,*)' '
      write(stdout,*)' BEGIN MHS 1B DECODE'

C  Initialize arrays
C  -----------------

      badr  = 0.
      badtb = 0.
      nprint = 1000 ! skip between data record diagnostic prints

C  Write header record to standard output
C  --------------------------------------

      write(stdout,*)' '
      write(stdout,*)'header information below'
      write(stdout,*)'nreal,mch = ',nreal,mch
      write(stdout,*)'ntot      = ',ntot
      write(stdout,*)'channel numbers below'
      write(stdout,*) (chan(i),i=1,mch)
      write(stdout,*)' '
ccccc write(99,*) nreal,mch,(chan(i),i=1,mch)

C  Open output BUFR file
C  ---------------------

ccccc call openbf(lubfrb,'OUT',lundx)
      call openbf(lubfrb,'NODX',lundx)

C  Open unit to raw 1B MHS data file - read header record, see if valid
C   data type - if not, exit routine
C  --------------------------------------------------------------------

      open(lunin,file=rawmhs,recl=nbyte1/rfac,
     &      access='direct',status='old')
      nri = 1
      read (lunin,rec=nri,err=1900) (kbuf(i),i=1,nbyte1)

C  Load header record into work array
C  ----------------------------------

      do i = 1,nbyte4
         indat(i) = jbuf(i)
      end do

C  Extract NOAA spacecraft identification code (72*8+1=577)
C   and convert it into BUFR value (CODE TABLE 0-01-007)
C  --------------------------------------------------------

      jsat  = lbyte(577,16,indat)
      if (jsat.eq.7) then  ! NOAA-18
         jsat0 = jsat
         jsat  = 209
         write(stdout,*) '***WARNING:   reset NOAA-18 satellite id ',
     x        'from ',jsat0,' to ',jsat
      else if(jsat.eq.8) then ! NOAA-19
         jsat0 = jsat
         jsat  = 223
         write(stdout,*) '***WARNING:   reset NOAA-19 satellite id ',
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
      else if(jsat.eq.13) then ! METOP-3
         jsat0 = jsat
         jsat  = 5
         write(stdout,*) '***WARNING:   reset METOP-3 satellite id ',
     x        'from ',jsat0,' to ',jsat
      else
         write(stdout,*) '***ERROR***   unknown satellite id ',jsat
         call w3tage('BUFR_TRANMHS')
         call errexit(6)
      endif

C  Extract data type code (76*8+1=609)
C  -----------------------------------

      jtype = lbyte(609,16,indat)

      if (jtype.ne.12) then
         write(stdout,*)'***ERROR***  Input data file does not contain',
     x    ' MHS data (type=12).  data type = ',jtype
         call w3tage('BUFR_TRANMHS')
         call errexit(1)
      endif
      write(stdout,*) 'Data type and BUFR satellite id = ',jtype,jsat

C  Extract number of data records in data set (132*8+1=1057)
C   and number of scans (134*8+1=1073)
C  ---------------------------------------------------------

      nrecs = lbyte(1057,16,indat)
      nscan = lbyte(1073,16,indat)
      write(stdout,*)'nrecs,nscan=',nrecs,nscan

C  Extract instrument temperature and load into reference array
C  ------------------------------------------------------------

      scale = 1.d-2
      tshelfp = mbyte(1601,16,indat)*scale  ! (200*8+1=1585)
      tshelfb = mbyte(1649,16,indat)*scale  ! (206*8+1=1633)
      tref(1) = tshelfb
      tref(2) = tshelfb
      tref(3) = tshelfp
      tref(4) = tshelfp
      tref(5) = tshelfp

C  Extract cold space fixed bias correction
C  ----------------------------------------

      scale = 1.d-3
      do i = 1,mch
         jb = (216 + (i-1)*14)*8  + 1
         dt0(i) = mbyte(jb,16,indat)*scale
         dt1(i) = mbyte(jb+16,16,indat)*scale
         dt2(i) = mbyte(jb+32,16,indat)*scale
      end do

      write(stdout,*)'instrument T=',(tref(i),i=1,mch)
      write(stdout,*)'cold space dt0=',(dt0(i),i=1,mch)
      write(stdout,*)'cold space dt1=',(dt1(i),i=1,mch)
      write(stdout,*)'cold space dt2=',(dt2(i),i=1,mch)
      write(stdout,*)' '

C  Extract coefficients for radiance to temperature conversion
C  -----------------------------------------------------------

      scale6 = 1.d-6
      do j = 1,mch
! tranamsub        jb  = 325 + (j-1)*12
         jb  = 417 + (j-1)*12
         jb0 = jb
         jb1 = jb0 + 4
         jb2 = jb1 + 4
         cwave(j) = xfloat(1,kbuf(jb0))*scale6
         cnst1(j) = xfloat(1,kbuf(jb1))*scale6
         cnst2(j) = xfloat(1,kbuf(jb2))*scale6
      end do
      write(stdout,*)'chn 1 cwave,cnst1,cnst2=',
     x     cwave(1),cnst1(1),cnst2(1)
      write(stdout,*)' '

C  Extract the bias correction
C     ntx:  the trasmitter number: STX-1, STX-2, STX-3 SARR
C     nfov: the number of bias correction provided in the header
C            record nfov=90/5+1
C  -------------------------------------------------------------

      do itx=1,ntx
      do ifov=1,nfov
      do ich=1,mch
! tranamsub        jj=1000*8+(ifov-1)*5*16+(ich-1)*16+(itx-1)*105*16+1
          jj=886*8+(ifov-1)*5*16+(ich-1)*16+(itx-1)*105*16+1
          ibiascorr(ich,ifov,itx)=mbyte(jj,16,indat)
      enddo
      enddo
      enddo

      write(stdout,*) 'ibiascorr(1,1,1),ibiascorr(1,2,1)',
     :                 ibiascorr(1,1,1),ibiascorr(1,2,1)
      write(stdout,*) 'ibiascorr(4,1,4),ibiascorr(4,2,4)',
     :                 ibiascorr(4,1,4),ibiascorr(4,2,4)

C  Interpolation: quadratic interpolation recommended in the appendix 4
C  --------------------------------------------------------------------

C  Calculate gradients
C  -------------------

      do ich=1,mch
      do itx=1,ntx
      do ifov=2,nfov-1
         grad(ich,ifov,itx)=0.1*(ibiascorr(ich,ifov+1,itx)-
     :                           ibiascorr(ich,ifov-1,itx))
      end do
        grad(ich,1,itx)=2.0*grad(ich,2,itx)-grad(ich,3,itx)
        grad(ich,nfov,itx)=2.0*grad(ich,nfov-1,itx)-grad(ich,nfov-2,itx)
      enddo
      enddo

C  Interpolate
C  -----------

      do ipx=1,mpos
         ip1=ipx/5+1           !  find nearest 2 points in table
         ip2=ipx/5+2           !  2 to 20
         ipx1=(ip1-1)*5        !  0 to 90
         ipx2=(ip2-1)*5        !  5 to 95
         if(ipx1 .eq.0) ipx1=1 !  first point is pixel 1
         if(ip2 .gt.19) ip2=19
         f=(ipx2-ipx)/(1.0*(ipx2-ipx1))  ! linear term
         ff=0.5*f*(ipx-ipx1)             ! quadratic term
         do ich=1,mch
         do itx=1,ntx
            icorrtab(ich,ipx,itx)=nint(ibiascorr(ich,ip1,itx)*f
     :               +ibiascorr(ich,ip2,itx)*(1.0-f)
     :               +(grad(ich,ip1,itx)-grad(ich,ip2,itx))*ff)
         enddo
         enddo
         enddo

       write(stdout,*) 'icorrtab(1,1,1), icorrtab(1,2,1)',
     :                 icorrtab(1,1,1), icorrtab(1,2,1)
       write(stdout,*) 'icorrtab(1,3,1), icorrtab(1,4,1)',
     :                 icorrtab(1,3,1), icorrtab(1,4,1)

       write(stdout,*) 'icorrtab(4,1,4), icorrtab(4,85,4)',
     :                 icorrtab(4,1,4), icorrtab(4,85,4)
       write(stdout,*) 'icorrtab(4,15,4), icorrtab(4,90,4)',
     :                 icorrtab(4,15,4), icorrtab(4,90,4)

C  Extract transmitter reference power
C  -----------------------------------

      do itx=1,ntx
       scale=1.0d-1
       jj=1734*8+1+(itx-1)*16
       itranrefpow(itx)=nint(mbyte(jj,16,indat)*scale)
      enddo

      write(stdout,*) 'itranrefpow',(itranrefpow(i),i=1,ntx)

C  Prepatory initializations prior to reading in satellite data
C  ------------------------------------------------------------

      nopos   = 0
      nqcbad  = 0
      nqctim  = 0
      nqccal  = 0
      nqcloc  = 0
      nermin  = 0
      nermaj  = 0
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

C  Extract scan line number, start date/time, position, and type
C  -------------------------------------------------------------

         iline  = lbyte(1,16,indat)
         iyear  = lbyte(17,16,indat)  ! (2*8+1=17)
         iddd   = lbyte(33,16,indat)  ! (4*8+1=33)
         itime  = lbyte(65,32,indat)  ! (8*8+1=65)
         idt(1) = iddd                   ! day of the year
         idt(2) = iyear                  ! 4-digit year
         sctime = 1.d-3*float(itime)     ! second of the day

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

C  Is scan line for full scan mode - if so, keep, else skip this scan
C  ------------------------------------------------------------------

         imode  = lbyte(177,8,indat)  ! (22*8+1=177)
         if ( imode.ne.3 ) then
            nskipm = nskipm + 1
            write(stdout,1010) nlo,iline,imode,nskipm
 1010              format('***FAIL MODE:  nlo=',i6,' iline=',i6,
     x           ' imode=',i2,' nskipm=',i6)
            goto 1200
         endif

C  Extract quality bits - if all good (=0) continue, else skip this scan
C  ---------------------------------------------------------------------

         isum   = 0
         iqcbad = lbyte(193,1,indat)  ! (8*24+1+0=193)
         iqctim = lbyte(194,1,indat)  ! (8*24+1+1=194)
         iqccal = lbyte(196,1,indat)  ! (8*24+1+3=196)
         iqcloc = lbyte(197,1,indat)  ! (8*24+1+4=197)
         iermin = lbyte(222,1,indat)  ! (8*24+1+29=222)
         iermaj = lbyte(223,1,indat)  ! (8*24+1+30=223)
         isum   = iqcbad + iqctim + iqccal + iqcloc
     x        + iermin + iermaj
         if (iqcbad.ne.0) nqcbad = nqcbad + 1
         if (iqctim.ne.0) nqctim = nqctim + 1
         if (iqccal.ne.0) nqccal = nqccal + 1
         if (iqcloc.ne.0) nqcloc = nqcloc + 1
         if (iermin.ne.0) nermin = nermin + 1
         if (iermaj.ne.0) nermaj = nermaj + 1
         if (isum.ne.0) then
            nskipq = nskipq + 1
            write(stdout,1000) nlo,iline,iqcbad,iqctim,
     x           iqccal,iqcloc,iermin,iermaj,nskipq
 1000              format('***FAIL QC  :  nlo=',i6,' iline=',i6,
     x           ' bad=',i2,' time=',i2,' cali=',i2,' loc=',i2,
     x           ' min=',i2,' maj=',i2,' nskipq=',i6)
            goto 1200
         endif

C  Extract calibration quality bits for each channel
C  -------------------------------------------------

         isum = 0
         do j = 1,mch
            ibadc(j) = 0
            jb       = (32 + (j-1)*2)*8+1
            iqccal   = lbyte(jb,16,indat)
            ibadc(j) = iqccal
            isum     = isum + ibadc(j)
         end do
         if (isum.ne.0) then
            nskipc = nskipc + 1
            write(stdout,1005) nlo,iline,(ibadc(j),j=1,mch)
 1005       format('***FAIL CAL :  nlo=',i6,' iline=',i6,
     x           ' badcal=',20(i2,1x))
         endif

C  Extract calibration coefficients
C  --------------------------------

         scale16 = 1.d-16
         scale10 = 1.d-10
         scale6  = 1.d-6
         do j = 1,mch
            jb2   = 61 + (j-1)*12
            c2(j) = xfloat(1,kbuf(jb2))*scale16
            jb1   = jb2 + 4
            c1(j) = xfloat(1,kbuf(jb1))*scale10
            jb0   = jb1 + 4
            c0(j) = xfloat(1,kbuf(jb0))*scale6

C  Code to pull out secondary calibration coefficients
C  ---------------------------------------------------

ccccc         jb2   = 121 + (j-1)*12
ccccc         c2j = xfloat(1,kbuf(jb2))*scale16
ccccc         jb1   = jb2 + 4
ccccc         c1j = xfloat(1,kbuf(jb1))*scale10
ccccc         jb0   = jb1 + 4
ccccc         c0j = xfloat(1,kbuf(jb0))*scale6
         end do

C  EXTRACT NAVIGATION DATA
C  -----------------------
C  -----------------------

C  Extract spacecraft altitude (km)
C  --------------------------------

         scale  = 1.d-1
         jb     = 210*8+1
         sathgt = lbyte(jb,16,indat)*scale

C  Extract angular relationships
C  -----------------------------

         scale = 1.d-2
         do i = 1,mpos
            jb0 = 212*8+1 + (i-1)*48
            jb1 = jb0 + 16
            jb2 = jb1 + 16
            soza(i) = mbyte(jb0,16,indat)*scale
            saza(i) = mbyte(jb1,16,indat)*scale
            rlocaz(i) = mbyte(jb2,16,indat)*scale
         end do

C  Extract earth location data
C  ---------------------------

         scale = 1.d-4
         do i = 1,mpos
            jb0     = 753 + (i-1)*8
            slat(i) = xfloat(1,kbuf(jb0))*scale
            jb1     = jb0 + 4
            slon(i) = xfloat(1,kbuf(jb1))*scale
            lndsea(i) = rmiss
            sfchgt(i) = rmiss
            ikeepb(i)  = 0
            if ( (abs(slat(i)).gt.90.) .or.
     x           (abs(slon(i)).gt.180.) ) then
               ikeepb(i) = 0
               nbadl    = nbadl + 1
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
ccccc            lndsea(i) = rmask + eps
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

C  Extract the transmitter power
C  -----------------------------

         do itx=1,ntx
          jj=2854*8+1+(itx-1)*16  ! (2854*8+1=22833)
          itranpow(itx)=mbyte(jj,16,indat)
         enddo
         itranpow(ntx)=itranpow(ntx)+mbyte(22897,16,indat)  ! (2862*8+1)

         if(nlo.eq.10) then
          write(stdout,*)'itranpow',(itranpow(i),i=1,4)
         end if

C  Extract MHS counts for channels H1-H5, then convert counts to
C   radiances
C  -------------------------------------------------------------

         do i = 1,mpos
            jb = (1480 + (i-1)*12 + 1*2)*8+1
            do j = 2,6
               jj = j-1
               jb0 = jb + (j-2)*16
               counts1 = lbyte(jb0,16,indat)

C  Bias correction on counts
C  -------------------------

               iecorr=0
               do itx=1,ntx
                 if(itranrefpow(itx).gt.0) then
                    f=itranpow(itx)/(1.0*itranrefpow(itx))
                    if(f.gt.0.01) then
                      iecorr=iecorr+nint(icorrtab(jj,i,itx)*f)
                    end if
                 end if
               enddo

               counts=counts1+iecorr

ccccc   write(stdout,*) 'iecorr,counts1,counts jj,c0(jj),c1(jj),c2(jj)',
ccccc :  iecorr,counts1,counts,jj,c0(jj),c1(jj),c2(jj)

               rads   = c0(jj) + (c1(jj)+c2(jj)*counts)*counts
               if (rads.lt.0.) then
                  write(stdout,*) rads,jj,i,iecorr,counts1,counts
                  nbadr   = nbadr + 1
                  badr(j) = badr(j) + 1
                  rads    = rmiss
               endif
               rad(jj,i) = rads
            end do
         end do

C-----------------------------------------------------------------------
C  Convert radiances to apparent temperature (Ta0), then convert
C   apparent temperature (Ta0) to brightness temperature (Tb)
C   QC all channels - if all channels are bad for a given spot, set
C   flag to omit Tb data in final write
C   Note:  The MHS does not produce an intermediate antenna temperature
C          (Ta)
C----------------------------------------------------------------------

         do i = 1,mpos
            ibadtb = 0
            do j = 1,mch
               rads  = rad(j,i)
               term1 = p2*cwave(j)
               if (rads.gt.eps) then
                  term2 = 1. + p1*cwave(j)**3/rads
                  if (term2.le.0) term2 = eps
                  term3   = log(term2)
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

               if ( (tb(j,i).lt.tlo) .or.
     x              (tb(j,i).gt.thi) ) then
                  nbadtb   = nbadtb + 1
                  badtb(j) = badtb(j) + 1
                  tb(j,i) = rmiss
               endif

C  If calibration quality flag for this channel is nonzero, we do not
C   want to use this channel for Tb
C  ------------------------------------------------------------------

               if (ibadc(j).ne.0) then
                  nbadc   = nbadc + nbadc
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

C  ----------------------------------------------------------
C  WRITE MHS DATA FOR EACH SPOT POSITION ON CURRENT SCAN LINE
C  ----------------------------------------------------------

         do i = 1,mpos
            if (ikeepb(i).eq.1) then

C  First, update scan start date time to reflect date time for spot
C  ----------------------------------------------------------------

               rinc(4) = -0.0285+(i-1)*.019 !# of seconds to add to scan
                                            !start time for this spot
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
               bdata(9) = lndsea(i)               ! land/sea tag
               bdata(10)= i                       ! F-O-V (spot) number
               bdata(11)= slat(i)                 ! latitude
               bdata(12)= slon(i)                 ! longitude
ccccc          bdata(13)= rlocaz(i)
               bdata(13)= saza(i)                 ! sat.  zenith angle
               bdata(14)= soza(i)                 ! solar zenith angle
               bdata(15)= sfchgt(i)               ! surface height
               bdata(16)= sathgt                  ! satellite height
               bdata(17)= sazimuth(i)             ! solar azimuth angle
               bdata(18)= aazimuth(i)             ! sat.  azimuth angle

               if (lndsea(i).lt.0.5) nseab  = nseab + 1
               if (lndsea(i).gt.0.5) nlandb = nlandb + 1
               do j = 1,mch
                  bdata(17+j*nperchan) = tb(j,i) ! brightness temp
                  bdata(18+j*nperchan) = dt1(j)  ! cold space temp corr
               end do
               nrecb = nrecb + 1
               call bufr1b(lubfrb,'NC021027',nreal,mch,bdata,nrepb)
ccccc          write(99,*) (bdata(j),j=1,ntot)
            endif
         end do

C  Every NPRINT scan lines, print mpos-th record
C  ---------------------------------------------

         if (mod(nlo,nprint).eq.0) then
            write(stdout,*)' '
            write(stdout,*)' Tb data for line,rec=',nlo,nrecb
            write(stdout,*) (bdata(i),i=1,ntot)
            write(stdout,*)' '
         endif

C**********************************************************************
C           DONE WITH THIS SCAN LINE, READ NEXT SCAN LINE
C**********************************************************************

      goto 1200

 1600 continue

C  All scan lines have been read and processed, summarize
C  ------------------------------------------------------

      write(stdout,*)' '
      write(stdout,*)'Done reading raw 1b file'
      write(stdout,*)' '
      write(stdout,*)'MHS INGEST STATS:'
      write(stdout,*)' no. scan lines           =   ',nlo,nrecs,nscan
      write(stdout,*)' no. fail good qc         =   ',nqcbad
      write(stdout,*)' no. fail time qc         =   ',nqctim
      write(stdout,*)' no. fail cali qc         =   ',nqccal
      write(stdout,*)' no. fail loc qc          =   ',nqcloc
      write(stdout,*)' no. frame errors         =   ',nermin,nermaj
      write(stdout,*)' no. scans bad qc         =   ',nskipq
      write(stdout,*)' no. scans bad calibr.    =   ',nskipc
      write(stdout,*)' no. scans bad mode       =   ',nskipm
      write(stdout,*)' no. bad (lat,lon)        =   ',nbadl
      write(stdout,*)' no. zero (lat,lon)       =   ',nopos
      write(stdout,*)' no. bad radiances        =   ',nbadr
      write(stdout,*)' no. bad calibration      =   ',nbadc
      write(stdout,*)' no. scans with bad Tb    =   ',nskiptb
      write(stdout,*)' no. bad Tb values        =   ',nbadtb
      write(stdout,*)' no. land/sea Tb obs      =   ',nlandb,nseab
      write(stdout,*)' no. Tb recs written      =   ',nrecb
      write(stdout,*)' no. Tb BUFR rpts written =   ',nrepb

      write(stdout,*)' '
      write(stdout,*)'bad radiance,temperature counts per channel'
      write(stdout,1020)
 1020 format(t1,'channel',t10,'bad rad',t20,'bad Tb')
      sumr  = 0.
      sumtb = 0.
      do j = 1,mch
         write(stdout,1030) j,badr(j),badtb(j)
 1030    format(t1,i2,t10,f8.1,t20,f8.1)
         sumr  = sumr  + badr(j)
         sumtb = sumtb + badtb(j)
      end do
      write(stdout,*)'nbadr,nbadtb=',sumr,sumtb

      write(stdout,*)' '
      write(stdout,*)' MHS 1B DECODE COMPLETED'
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
      write(stdout,*)' *** error reading hdr record of file ',rawmhs
      close(lunin)
      call closbf(lubfrb)
      call w3tage('BUFR_TRANMHS')
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
C   PRGMMR: NADIGA           ORG: NP22       DATE: 2019-10-09
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
C 2019-10-09  Nadiga -- Modified to shift the Y2K windowing technique
C      that converts 2-digit years to 4-digit.
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
C   PRGMMR: BERT KATZ        ORG: NP20       DATE: 1997-11-05
C
C ABSTRACT: Turns character string of specified length into integer.
C
C PROGRAM HISTORY LOG:
C 1997-11-05  Katz -- Original author
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
      integer(8) mask
      data mask/z'ffffffff00000000'/
ccccc character*2 iarray(2)
      character*2 iarray(2000)
      character*4 conv
      real xf
      integer j
      integer(8) jj
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
