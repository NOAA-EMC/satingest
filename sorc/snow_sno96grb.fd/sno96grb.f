      PROGRAM SNO96GRB
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: SNO96GRB     GRIBS THE 1/96TH BEDIENT
C                            NESDIS/SAB SNOW/ICE DATA
C   PRGMMR: GAYNO            ORG: NP2         DATE: 2005-05-13
C
C ABSTRACT: This code reads in the IMS 1/96th bedient snow/sea ice 
C           coverage data (ascii) and convert them to a grib file. 
C           It is based on the SNO16GRB program.
C
C PROGRAM HISTORY LOG:
C 2005-05-13  GEORGE GAYNO
C 2012-12-06  Diane C Stokes - Minor modifications to run on WCOSS.
C 2014-01-20  Dennis Keyser  - Minor changes.
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
C     LIBRARY:  PUTGB  W3MOVDAT  W3TAGB  W3TAGE
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =   6 - ENVIRONMENT VARIABLE FOR OUTPUT FILE DOES NOT EXIST
C          =   7 - OUTPUT FILENAME TOO LONG FOR VARIABLE LENGTH 
C          =   8 - NON-SPECIFIC ERROR GETTING OUTPUT FILENAME
C          =   9 - UNEXPECTED STATUS GETTING OUTPUT FILENAME
C          =  95 - BAD READ ON STDIN
C          =  96 - BAD WRITE ON UNIT 51
C          =  97 - BAD OPEN ON UNIT 51
C          =  98 - BAD READ ON UNIT 11
C          =  99 - UNRECOGNIZED DATA VALUE IN UNIT 11
C
C REMARKS: RUNNING AS REAL*8 AND REAL*4 GAVE THE SAME RESULT.
C          GRID IS NOT EXACTLY 1/96 BEDIENT, BUT RATHER 4 KM.
C 
C ATTRIBUTES:
C   LANGUAGE:  FORTRAN 90
C   MACHINE:   NCEP WCOSS
C
C$$$
      implicit none
c
      integer, parameter         :: n=6144   ! 96*64
c
c  alat1  - latitude of point (1,1)
c  alon1  - longitude of point (1,1)
c  xmeshl - grid mesh length at 60N in km
c  orient - the orientation east longitude of the grid.
c
      real, parameter            :: alat1=-21.492
      real, parameter            :: alon1=-125.0
      real, parameter            :: orient=280.
      real, parameter            :: xmeshl=4.0
c
      character                  :: envvar*6
      character                  :: fileo*80
      character                  :: line*80
c
      integer*1                  :: cice(n,n)
      integer                    :: i
      integer                    :: ibitm
      integer                    :: idatin(8)
      integer                    :: idtout(8)
      integer                    :: idy
      integer                    :: iret
      integer                    :: irow(n)
      integer                    :: iyr
      integer                    :: j
      integer                    :: jda
      integer                    :: jmo
      integer                    :: KGDS(200)
      integer                    :: KPDS(200)
      integer                    :: lun
      integer                    :: length
      integer                    :: iestatus
      integer                    :: m
      integer*1                  :: snow(n,n)
c
      logical*1                  :: bitmap_snow(n,n)
      logical*1                  :: bitmap_ice(n,n)
c
      real                       :: dummy(n,n)
      real                       :: timinc(5)
c
      data lun/51/
c
      CALL W3TAGB('SNO96GRB',2014,0020,0000,'NP2')                  
c
      read(5,'(i4,i3)',iostat=iret) iyr, idy
c
      if (iret /= 0) then
        write(6,*) 'Bad read of stdin, iret: ', iret
        CALL W3TAGE('SNO96GRB')
        call err_exit(95)
      end if       
c
c  Open output GRIB file
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
      if (iret /= 0) then
        write(6,*) 'Bad open of unit 51, iret: ', iret
        CALL W3TAGE('SNO96GRB')
        call err_exit(97)
      end if       
c
c  Use w3movdat to convert julian date to mm/dd:
c
      idatin(1) = iyr
      idatin(2:3) = 1
      idatin(4:8) = 0
      timinc(1) = float(idy - 1)
      timinc(2:5) = 0.0
      call w3movdat(timinc,idatin,idtout)
      jmo = idtout(2)
      jda = idtout(3)
c
c  Read past file header
c
      do m = 1, 30
        read(11,'(a80)',end=9999,err=9999) line
      enddo
c
      snow        = 0
      cice        = 0
      bitmap_ice  = .TRUE.
      bitmap_snow = .TRUE.
      ibitm       = 1    ! use a bitmap section
c
      do 40 j = 1, n
        read(11,'(6144i1)',end=9999,err=9999) irow
        do 30 i = 1, n
           if (irow(i).eq.0) then      ! off hemisphere
              bitmap_snow(i,j) = .FALSE.
              bitmap_ice(i,j)  = .FALSE.
           elseif (irow(i).eq.1) then  ! open water
              bitmap_snow(i,j) = .FALSE.
           elseif (irow(i).eq.2) then  ! snow-free land
              bitmap_ice(i,j) = .FALSE.
           elseif (irow(i).eq.3) then  ! sea ice
              cice(i,j)        = 1
              bitmap_snow(i,j) = .FALSE.
           elseif (irow(i).eq.4) then  ! snow
              snow(i,j)        = 100
              bitmap_ice(i,j)  = .FALSE.
           else
              write(6,*) 'Invalid irow value! ', irow(i)
              CALL W3TAGE('SNO96GRB')
              call err_exit(99)
           endif
 30     continue
 40   continue
c
      KPDS = 0
c
      KPDS(1) =7
      KPDS(2) =25
      KPDS(3) =255
      KPDS(4) =128+ibitm*64
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
      KGDS = 0
c
      KGDS(1)= 5
      KGDS(2)= n
      KGDS(3)= n
      KGDS(4)= 1000.*alat1
      KGDS(5)= 1000.*alon1
      KGDS(6)= 72 ! earth is an oblate spheroid
      KGDS(7)= 1000.*orient
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
      dummy = float(cice)
      KPDS(5) = 91
      call putgb(lun,n*n,kpds,kgds,bitmap_ice,dummy,iret)
      write(6,*) 'After PUTGB for sea ice, iret=', iret
      if (iret /= 0) goto 9998
c
c  Output snow:
c
      dummy = float(snow)
      KPDS(5) = 238
      call putgb(lun,n*n,kpds,kgds,bitmap_snow,dummy,iret)
      write(6,*) 'After PUTGB for snow, iret=', iret
      if (iret /= 0) goto 9998
      call baclose(lun,iret)
      CALL W3TAGE('SNO96GRB')                                           
      stop
c
c  Error handling:
c
9998  continue
      write(6,*) 'Bad write on grib file unit 51'
      CALL W3TAGE('SNO96GRB')
      call baclose(lun,iret)
      call err_exit(96)
c
9999  continue
      write(6,*) 'Unexpected end-of-file on unit 11'
      CALL W3TAGE('SNO96GRB')
      call err_exit(98)
c
      end program SNO96GRB
