      PROGRAM SNO16GRB
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: SNO16GRB     GRIBS THE NESDIS/SAB SNOW/ICE DATA
C   PRGMMR: KATZ             ORG: NP2         DATE: 1998-11-09
C
C ABSTRACT: This code reads in the IMS 1/16th bedient snow/sea ice 
C           coverage data and convert them to a grib file.  
C
C PROGRAM HISTORY LOG:
C   98-01-22  YING LIN
C   98-01-27  BERT KATZ - MINOR CHANGES FOR IMPLEMENTATION
C   98-10-09  YING LIN Y2K compliance.  Also read in the data from the 
C             new IMS ascii file, in which 
C               1) each grid point is represented by a single-
C                  digit value  
C                     0 - south of equator (so we no longer need 
C                         to calculate the latitude for each 
C                         grid point
C                     1 - sea (no ice)
C                     2 - land (no snow)
C                     3 - sea ice
C                     4 - snow
C               2) File name and the date stamp inside have 4-
C                  digit year instead of 2-digit year
C   98-11-06  Bert Katz   Minimal changes for implementation
C 1999-05-19  Gilbert   - Changed bitmap from logical to logical*1.
C                       - added call to BAOPEN to open output GRIB file.
C 2012-12-06  DC Stokes - Minor modifications to run on WCOSS.
C 2014-01-20  D. KEYSER - Minor changes.
C
C USAGE:
C   INPUT FILES:
C     stdin   - YYYYDDD (year and julian date) of the snow/ice file.
C     fort.11  - NESDIS/SAB snow/ice coverage data in ASCII
C
C   OUTPUT FILES:
C     fort.51  - gribbed snow/ice fields
C
C   SUBPROGRAMS CALLED: 
C     LIBRARY:  PUTGB  W3FB05  W3MOVDAT  W3TAGB  W3TAGE
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =   6 - ENVIRONMENT VARIABLE FOR OUTPUT FILE DOES NOT EXIST
C          =   7 - OUTPUT FILENAME TOO LONG FOR VARIABLE LENGTH
C          =   8 - NON-SPECIFIC ERROR GETTING OUTPUT FILENAME
C          =   9 - UNEXPECTED STATUS GETTING OUTPUT FILENAME
C          =  98 - UNEXPECTED END-OF-FILE ON UNIT 11
C          =  99 - BAD IROW VALUE
C 
C ATTRIBUTES:
C   LANGUAGE:  FORTRAN 90
C   MACHINE:   NCEP WCOSS
C
C$$$

c
c  Program to plot and grib the 1/16-mesh snow data developed by 
c  NESDIS/SAB.  
c
      parameter(n=1024,xpnmcaf=513,ypnmcaf=512.,
     &                xmeshl=23.8125,orient=80.)
c  xpnmcaf, ypnmcaf - location of pole.
c  xmeshl - grid mesh length at 60N in km
c  orient - the orientation west longitude of the grid.
c
      dimension snow(n,n), cice(n,n), irow(n), KPDS(200),KGDS(200),
     &      timinc(5), idatin(8), idtout(8)
      logical*1 bitmap(n,n)
      character line*80,fileo*80,envvar*11
      data lun/51/
      CALL W3TAGB('SNO16GRB',2014,0020,0000,'NP2    ')                  
C--------------------------------------------------------------------
c
      read(5,'(i4,i3)') iyr, idy
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
          call err_exit(6)
        case(-1)
          print*,'env variable ',trim(envvar),' is set to string of',
     1     length,' characters which does not fit in fileo.'
          call err_exit(7)
        case(3)
          print*,'non-specific error(s) from GET_ENVIRONMENT_VARIABLE'
          call err_exit(8)
        case default
          print*,'unexpected status from GET_ENVIRONMENT_VARIABLE'
          call err_exit(9)
      end select

      call baopen(lun,fileo,iret)

c
c  Use w3movdat to convert julian date to mm/dd:
      idatin(1) = iyr
      idatin(2:3) = 1
      idatin(4:8) = 0
      timinc(1) = float(idy - 1)
      timinc(2:5) = 0.0
      call w3movdat(timinc,idatin,idtout)
      jmo = idtout(2)
      jda = idtout(3)
c
      do 15 m = 1, 30
 15   read(11,'(a80)',end=9999) line
c
      snow = 0.
      cice = 0.
      bitmap = .TRUE.
      ibitm = 1
c
      do 40 j = 1, n
        read(11,'(1024i1)') irow
        do 30 i = 1, n
           if (irow(i).eq.0) then
              bitmap(i,j) = .FALSE.
           elseif (irow(i).eq.3) then
              cice(i,j) = 1.
           elseif (irow(i).eq.4) then
              snow(i,j) = 100.
           elseif (irow(i).ne.1 .and. irow(i).ne.2) then
              write(6,*) 'Invalid irow value!'
              CALL W3TAGE('SNO16GRB')
              call err_exit(99)
           endif
 30     continue
 40   continue
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
      KPDS(9) =jmo
      KPDS(10)=jda
      KPDS(11)=22
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
      KPDS(22)=0
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
      call putgb(lun,n*n,kpds,kgds,bitmap,cice,iret)
      write(6,*) 'After PUTGB for sea ice, iret=', iret
c
c  Output snow:
c
      KPDS(5) = 238
      call putgb(lun,n*n,kpds,kgds,bitmap,snow,iret)
      write(6,*) 'After PUTGB for snow, iret=', iret
      CALL W3TAGE('SNO16GRB')                                           
      stop
c
9999  continue
      write(6,*) 'Unexpected end-of-file on unit 11'
      CALL W3TAGE('SNO16GRB')
      call err_exit(98)
c
      end
