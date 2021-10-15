      PROGRAM SNO8GRB
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: SNO8GRB      GRIBS THE USAF SNOW/ICE DATA
C   PRGMMR: KATZ             ORG: NP2         DATE: 1999-01-04
C
C ABSTRACT: This code reads in the USAF 1/8th bedient snow depth/sea 
C           ice coverage data and converts them to a grib file.  
C
C PROGRAM HISTORY LOG:
C   98-01-22  YING LIN
C   98-01-27  BERT KATZ - MINOR CHANGES FOR IMPLEMENTATION
C   98-10-09  YING LIN  1) Y2K compliance
C                       2) Use the input AFGWC Julian hour to determine 
C                          yyyy/mm/dd/hh
C   98-11-07  BERT KATZ - Changes for implementation
C   99-01-04  BERT KATZ - Removed date check after NESDIS glitch
C 1999-05-19  Gilbert   - Changed bitmap from logical to logical*1.
C                       - added call to BAOPEN to open output GRIB file.
C                       - now reads ebcdic string from unit 11 and
C                         converts to ascii.
C 2012-12-06  DC Stokes - Modifications to run on WCOSS
C                       - now reads unlbocked data.
C 2014-01-20  D. KEYSER - Minor changes.
C
C USAGE:
C   INPUT FILES:
C     fort.11  - USAF snow depth/ice coverage
C
C   OUTPUT FILES:
C     fort.51  - gribbed USAF fields
C
C   SUBPROGRAMS CALLED: 
C     LIBRARY:  PUTGB  W3MOVDAT  W3TAGB  W3TAGE
C 
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =   6 - ENVIRONMENT VARIABLE FOR OUTPUT FILE DOES NOT EXIST
C          =   7 - OUTPUT FILENAME TOO LONG FOR VARIABLE LENGTH
C          =   8 - NON-SPECIFIC ERROR GETTING OUTPUT FILENAME
C          =   9 - UNEXPECTED STATUS GETTING OUTPUT FILENAME
C          = 102 - READ ERROR ON UNIT 11                
C
C ATTRIBUTES:
C   LANGUAGE:  FORTRAN 90
C   MACHINE:   NCEP WCOSS
C
C$$$

      parameter(n=512,xpnmcaf=257,ypnmcaf=256.,
     &                xmeshl=47.625,orient=80.)
c  xpnmcaf, ypnmcaf - location of pole.
c  xmeshl - grid mesh length at 60N in km
c  orient - the orientation west longitude of the grid.
c
      dimension snodep(n,n), cice(n,n), KPDS(200),KGDS(200)
      integer*2   idata(n)
      integer     julhr
      real      timinc(5)
      integer   itimin(8),itmout(8)
      logical*1   bitmap(n,n)
      character mon(12)*2, header(2)*8, fileo*80, envvar*6
      data lun/51/
c
c  2-letter abbreviation of month names used by USAF:
      data mon/'JA','FE','MR','AP','MY','JN','JL','AU','SE','OC','NO',
     &         'DE'/
      CALL W3TAGB('SNO8GRB ',2014,0020,0000,'NP2    ')                  
C--------------------------------------------------------------------
c
c   Open output GRIB file
c
      envvar='FORT  '
      write(envvar(5:6),fmt='(I2)') lun
      call get_environment_variable(envvar, fileo, length, iestatus)
      select case(iestatus)
        case(0)
          continue
        case(1)
          print*,'environment variable ',trim(envvar),' does not exist'
          call errexit(6)
        case(-1)
          print*,'env variable ',trim(envvar),' is set to string of',
     1     length,' characters which does not fit in fileo.'
          call errexit(7)
        case(3)
          print*,'non-specific error(s) from GET_ENVIRONMENT_VARIABLE'
          call errexit(8)
        case default
          print*,'unexpected status from GET_ENVIRONMENT_VARIABLE'
          call errexit(9)
      end select

      call baopen(lun,fileo,iret)
C
      open(11,access='direct',recl=1024)
! first read not needed but kept in place (commented) for reference
!      read(11,err=980,rec=1) header, julhr
      read(11,err=980,rec=2) header, julhr
c
      write(6,*) 'header=', header, ' julhr=', julhr
c
      read(header(2),'(x,i2,2x,i2)') ida, iyr
c
      itimin(1) = 1967
      itimin(2) = 12
      itimin(3) = 31
      itimin(4:8) = 0
      timinc(1) = julhr / 24
      timinc(2) = mod(julhr,24)
      timinc(3:5) = 0.0
      call w3movdat(timinc,itimin,itmout)
      iyr = itmout(1)
      imo = itmout(2)
      ida = itmout(3)
      ihr = itmout(5)
c
      ibitm = 0
c
      do 40 j = 1,n
         xj = float(j) - ypnmcaf
         read(11,err=980,rec=n-j+3) idata
         do 30 i = 1, n
            xi = float(i) - xpnmcaf
            bitmap(i,j) = .TRUE.
c
c  Check to see if the point is south of the equator:
            call w3fb05(xi,xj,xmeshl,orient,alat,alon)
            if (alat.lt.0.) then
               ibitm = 1
               bitmap(i,j) = .FALSE.
               go to 30
            endif
c
c  the data are from 0 to 4090. '4090' is the sea ice flag.  Otherwise
c  it is inches of snow*10.  (e.g. '250' means 25.0 inches of snow).
c  For the grib file, the ice field is either 0 or 1, and the snow
c  field is in meters.
c
            if (idata(i).eq.4090) then
               cice(i,j) = 1.
               snodep(i,j) = 0.
            elseif (idata(i).lt. 4000.) then
               cice(i,j) = 0.
               snodep(i,j) = float(idata(i))/10.*0.0254
            else
               ibitm = 1
               bitmap(i,j) = .FALSE.
            endif
 30      continue
 40   continue
c
c
c  Compute the (lat, lon) for the lower-left corner of the domain 
c  (that is, the (1,1) point for the NCEP grid, or, in the 1/16th 
c  mesh USAF grid, the (1,1024) point)
c
      xj1 = float(1) - ypnmcaf
      xi1 = float(1) - xpnmcaf
      call w3fb05(xi1,xj1,xmeshl,orient,alat1,alon1)
c
      KPDS(1) =7
      KPDS(2) =25
      KPDS(3) =255
      KPDS(4) =128+ibitm*64
c
      KPDS(6) =1
      KPDS(7) =0
      KPDS(8) =mod(iyr-1,100)+1
      KPDS(9) =imo
      KPDS(10)=ida
      KPDS(11)=ihr
      KPDS(12)=0
      KPDS(13)=0
      KPDS(14)=0
      KPDS(15)=0
      KPDS(16)=0
      KPDS(17)=0
      KPDS(18)=1
      KPDS(19)=2
      KPDS(20)=0
      KPDS(21)=(iyr-1)/100 + 1
c kpds(22) - scaling factor, set separately.  
      KPDS(23)=4
      KPDS(24)=0
      KPDS(25)=0
c
      KGDS(1)= 5
      KGDS(2)= n
      KGDS(3)= n
      KGDS(4)= 1000.*alat1
      KGDS(5)= -1000.*alon1   
c  Longitude given by w3fb05 is the west longitude.
      KGDS(6)= 8
      KGDS(7)= 280000
      KGDS(8)= 1000.*xmeshl
      KGDS(9)= 1000.*xmeshl
      KGDS(10)= 0
      KGDS(11)= 64
      KGDS(12)= 0
      KGDS(13)= 0
      KGDS(14)= 0
      KGDS(15)= 0
      KGDS(16)= 0
      KGDS(17)= 0
      KGDS(18)= 0
      KGDS(19)= 0
      KGDS(20)= 255
      KGDS(21)= 0
      KGDS(22)= 0
c
c  Output sea ice:
c
      KPDS(5) = 91
      KPDS(22)=0
      call putgb(lun,n*n,kpds,kgds,bitmap,cice,iret)
      write(6,*) 'After PUTGB for sea ice, iret=', iret
c
c  Output snow, scaling factor set to 2 (i.e. 10**2)
c
      KPDS(5) = 66
      KPDS(22)=2
      call putgb(lun,n*n,kpds,kgds,bitmap,snodep,iret)
      write(6,*) 'After PUTGB for snow, iret=', iret
      CALL W3TAGE('SNO8GRB ')                                           
      stop
 980  continue
      write(6,*) 'Read Error on USAF snow data!'
      CALL W3TAGE('SNO8GRB ')                                           
      call errexit(102)
      end
