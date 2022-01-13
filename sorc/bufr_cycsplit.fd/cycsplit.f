      PROGRAM BUFR_CYCSPLIT

C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_CYCSPLIT
C   PRGMMR: STOKES           ORG: NP2         DATE: 2015-11-02
C
C ABSTRACT: READS IN BUFR FILE AND WRITES OUT MESSAGES TO OUTPUT FILES
C   AS DETERMINED BY CORRESPONDING SIX-HOUR CYCLE. 
C   (CENTERED ON 0000, 0600, 1200 AND 1800 UTC).
C   This is intended for short term use for a specific situation.
C
C PROGRAM HISTORY LOG:
C 2015-11-02  D. STOKES - ORIGINAL AUTHOR
C
C USAGE:
C
C   INPUT FILES:
C     UNIT 11  - BUFR INPUT FILE
C
C   OUTPUT FILES:
C     UNIT 06     - STANDARD OUTPUT PRINT
C     UNIT 50-79  - BUFR FILES CONTAINING TIME WINDOWED DATA
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       W3NCO  - W3TAGB W3TAGE IW3JDN W3FS26 err_exit
C     BUFRLIB  - DATELEN  OPENBF  IREADNS  UFBINT  IBFMS OPENMG OPENMB
C                UFBCPY  WRITSB CLOSMG CLOSBF  
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C              7 - NEED MORE CYCLE OUTPUT FILES THAN EXPECTED
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM iDataPlex
C
C$$$

      implicit none
      real(8) vec(4)
      integer,parameter :: mxcyc=29
      integer icdates(mxcyc),nsubseto(mxcyc)
      integer lunbfi,mdate,idate,idateold,icdate
      integer lunbfo,lunbfo_start
      integer ireadns,nsubseti,ibfms,nret
      integer iyr,imo,ida,ihr,icychr,kcyc,ii,ncycs_found
      integer iw3jdn,ijdncyc,iyrcyc,imcyc,idcyc,idwkcyc,idyrcyc
      character*8   subset
      character*24  cbufr_out
      logical exist

      data lunbfi/20/,lunbfo_start/51/

      call w3tagb('BUFR_CYCSPLIT',2015,0306,0050,'NP2') 

      print *
      print *, ' ==> Welcome to BUFR_CYCSPLIT -- Version 11/02/2015'
      print *

      nsubseti=0
      nsubseto(:)=0
      idateold=0
      icdates(:)=0
      ncycs_found=0

      call datelen(10)
      call openbf(lunbfi,'IN',lunbfi)
! read thru each subset in file
      sbloop: do while(ireadns(lunbfi,subset,mdate).EQ.0)
        nsubseti=nsubseti+1
        call ufbint(lunbfi,vec,4,1,nret,'YEAR MNTH DAYS HOUR')
        do ii=1,4
          if(ibfms(vec(ii)))then
            print*,'skip ob with missing date/time field:',vec(:)
            cycle sbloop
          endif
        enddo
        iyr=nint(vec(1))
        imo=nint(vec(2))
        ida=nint(vec(3))
        ihr=nint(vec(4))
        idate=iyr*1000000+imo*10000+ida*100+ihr

        if(idate.ne.idateold)then
!  determine appropriate cycle (including date)
          if(ihr.ge.21)then   ! 00Z cycle next day
            ijdncyc=iw3jdn(iyr,imo,ida)+1
            call w3fs26(ijdncyc,iyrcyc,imcyc,idcyc,idwkcyc,idyrcyc)  
            icdate=iyrcyc*1000000+imcyc*10000+idcyc*100
          else
            icychr=((ihr+3)/6)*6
            icdate=(idate/100)*100+icychr
          endif

!  check if we have already found this cycle/date
          do kcyc=1,ncycs_found
            if(icdate.eq.icdates(kcyc))exit
          enddo
          if(kcyc.gt.ncycs_found)then
            ncycs_found=ncycs_found+1
            if(ncycs_found.gt.mxcyc)then
              print*,'ERROR: More cycles found than expected.  Exit 7'
              call err_exit(7)
            endif
          endif
          lunbfo=lunbfo_start+kcyc-1

          if(nsubseto(kcyc).eq.0)then
            print*,'new cyc/date:',idate, icdate, lunbfo
            write(cbufr_out,'(a8,a1,i10,a5)')subset,'.',icdate,'.bufr'
! Output bufr filename will be along the lines of:
!    NC012012.2015110100.bufr
! (for above example: subset NC012012, date 20151101, 00Z cycle)
! See if file exists.  If so, open for append.  Otherwise, open as new.
            inquire(FILE=cbufr_out,EXIST=EXIST)
            if(exist)then
              print*,' append to existing file ',cbufr_out
              open(lunbfo,file=cbufr_out,form='unformatted')
              call openbf(lunbfo,'APX',lunbfi)
            else
              print*,' new file ',cbufr_out
              open(lunbfo,file=cbufr_out,form='unformatted')
              call openbf(lunbfo,'OUT',lunbfi)
! Add an empty msg to top of each output file.  This is done as a 
! precautionary measure for the sake of the current version of
! downstream job poes_phyret_sst which cannot process a file that
! contains only one message.
              call openmg(lunbfo,subset,icdate)
              call closmg(lunbfo)
            endif
            icdates(kcyc)=icdate
          endif
          idateold=idate
        endif

! copy subset to appropriate output bufr file.
        call openmb(lunbfo,subset,icdate)
        call ufbcpy(lunbfi,lunbfo)
        call writsb(lunbfo)
        nsubseto(kcyc)=nsubseto(kcyc)+1

      enddo   sbloop 

      call closbf(lunbfi)
      print*,'subsets read:',nsubseti
      print*,'subsets written for each cyc:'
      do kcyc=1,ncycs_found
        print*,'   ',icdates(kcyc),':  ',nsubseto(kcyc)
        lunbfo=lunbfo_start+kcyc-1
        call closbf(lunbfo)
      enddo

      call w3tage('BUFR_CYCSPLIT') 

      stop
      end

