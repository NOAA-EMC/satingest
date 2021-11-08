  program BUFR_TRANMLS

!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: BUFR_TRANMLS
!   PRGMMR: KEYSER           ORG: NP22        DATE: 2014-01-20
!
! Abstract: Reads in Aura MLS ozone mixing ratio data from a raw HDF5 format
!   file (ML2O3) and reformats them into BUFR in preparation for their ingest
!   into the BUFR data base on the NCEP supercompers.
!
! Program history log:
! 2010-06-16  Haixia Liu     - Modify the tranomi.f90 for MLS data set.
! 2011-11-02  Greg Krasowski - Final modifications incorporated.
! 2012-12-03  Haixia Liu     - Read in the nATscans and nLevels from the HDF5
!                              data file rather than hardwire to 3494 and 37,
!                              resp. (will prevent bogus scans between actual #
!                              and 3494 from being processed and later rejected
!                              n tranjb; MLS Version 2 has 37 levels, Version 3
!                              in 2013 has 55 levels).
! 2012-12-03  Dennis Keyser  - Updated to use delayed replication to write out
!                              BUFR levels (allows smooth transition from v2
!                              with 37 levels to v3 with 55 levels).
! 2012-12-07  Dennis Keyser  - Changes to run on WCOSS (minor).
! 2014-01-20  Dennis Keyser -- Minor changes
!
! Usage:
!
!   Input files:
!     command line argument
!                 - HDF5 binary file containing raw MLS ML2O3 ozone mixing
!                   ratio data
!     unit 31     - NCEP BUFR mnemonic table
!
!   Output files:
!     unit 06     - printout
!     unit 51     - NCEP BUFR file containing MLS data
!     unit 52     - diagnostic (ASCII) output file (currently not written to)
!
!   Subprograms called:
!     Unique:     - O3MR_DATASETS GRANULE_CALENDAR_DATE CHECK_ERROR
!     Library:
!       W3NCO     - W3TAGB     W3FI01    ERREXIT   W3MOVDAT  GBYTE
!       BUFRLIB   - DATELEN    OPENBF    OPENMB    UFBINT    WRITSB
!                   CLOSBF     UPFTBV
!       HDF5LIB   - H5OPEN_F   H5FOPEN_F H5GOPEN_F H5AOPEN_NAME_F H5AREAD_F
!                   H5ACLOSE_F H5DOPEN_F H5DREAD_F H5DCLOSE_F     H5GCLOSE_F
!                   H5FCLOSE_F H5CLOSE_F H5DGET_SPACE_F
!                   H5SGET_SIMPLE_EXTENT_NDIMS_F   H5SGET_SIMPLE_EXTENT_DIMS_F
!       SYSTEM    - GET_COMMAND_ARGUMENT
!
!
!   Exit states
!     0 = no errors detected
!    >0 = some type of error detected
!          1 = Cannot open HDF5 interface
!          2 = Cannot open HDF5 file
!          3 = Cannot open a groupname
!          4 = Cannot open an attribute (e.g., GranuleDay, GranuleMonth,
!              GranuleYear)
!          5 = Cannot read attribute-based variable ((e.g., GranuleDay,
!              GranuleMonth, GranuleYear)
!          6 = Invalid number of scan lines or levels in file
!          7 = Cannot close a groupname
!          8 = Error returned from an HDF5 interface routine
!          9 = Error returned from H5SGET_SIMPLE_EXTENT_NDIMS_F
!         10 = Error returned from H5SGET_SIMPLE_EXTENT_DIMS_F
!
! Remarks:
!     Note that input file is specified from command line argument.
!
!        Code proceeds as follows: 
!
!         1) Open the file.
!         2) Read the calendar date (UTC).
!         3) Read the number of scans and number of levels.
!         4) Allocate the variables, based on number of scans.
!         5) Read the datasets, one at a time.
!         6) Enocde dataset into BUFR.
!         7) Close interfaces and deallocate variables.
!  
!   
!
! Attributes:
!   Language: FORTRAN 90 (free format)
!   Machine:  NCEP WCOSS
!
!$$$

  use o3mr_datasets

!
! ***MUST DEFINE ALL VARIABLES***
  implicit none 

! Set parameters and declare variables
! ------------------------------------
  integer :: nLevels      ! MLS vertical pressure levels

  real    :: rinc(5)
  real(8) :: obs_8(13),bmiss
  real(8), allocatable, dimension(:,:) :: o3mrp_8
  real    :: max,min
  real(8) :: max_8,min_8

  real(kind=4), allocatable, dimension(:,:)   :: press2
  real(kind=4), allocatable, dimension(:)     :: bnry

  integer :: imin,jmin,imax,jmax
  integer :: error        ! Error flag returned by HDF5 interface calls
  integer(HID_T) :: fid          ! File ID number
  integer(HID_T) :: gid          ! Group ID number
  integer(HID_T) :: space_id     ! Dataspace identifier
  integer :: ndims        ! Number of dimensions
  integer(HID_T) :: attr_id      ! Attribute ID
  integer :: nLev         ! Index pointing to limb-Sounder measurement (sounder
                          !  position, 1 to nLevels)
  integer :: nscan        ! Index pointing to scan (1 to nATscans)
  integer :: lunbfr, lunprt, lundx, iwrite, iskip, iread, iret
  integer :: idat(8), jdat(8), ibit(18), ii, nib
  integer :: idate, iunpk, toqc, toqf, nbytw, nbitw, stko
  integer(hsize_t), dimension(1) :: data_dims  ! Required for call to read
                                               !  attribute
  integer(hsize_t), dimension(2) :: swath_dims ! Required for call to read
                                               !  dataset
  integer(hsize_t), dimension(1) :: temp_dims  ! Required for call to read
                                               !  dataset
  integer(hsize_t), allocatable, dimension(:) :: dims ! Array to store
                                                      !  dimension sizes
  integer(hsize_t), allocatable, dimension(:) :: maxdims ! Array to store max
                                                         !  dimension sizes
  integer(kind=4) :: granuleyear,granulemonth,granuleday

  integer :: decimal,binary(18),intbf

  character(LEN=256) :: o3mr_name     ! Input filename, read from command line
                                      !  argument
  character(LEN=256) :: o3mr_output
  character(LEN=8)   :: subset
  character(LEN=*), parameter :: o3mr_swath=&
                                          "/HDFEOS/SWATHS/O3"

  data lunbfr/51/, lunprt/52/, lundx/31/, bmiss/10e10/, iread/0/, iskip/0/, &
       iwrite/0/

!=============================================================================
!=============================================================================

  call W3TAGB('BUFR_TRANMLS',2014,0020,0068,'NP22')

  print *
  print *, 'Welcome to BUFR_TRANMLS - Version 01-20-2014'
  print *


! Get machine word length (nbytw) and specify number of bits per word (nbitw)
! ---------------------------------------------------------------------------
  call W3FI01(nbytw)
  nbitw = 8*nbytw


  o3mr_output='mls_output'


! Command line argument is input filename
! ---------------------------------------
  call GET_COMMAND_ARGUMENT(1,o3mr_name)


! Initialize HDF5 interface
! -------------------------
  call H5OPEN_F(error)
  if(error.ne.0) then 
     write(*,*) "##### Cannot open HDF5 interface, error = ",error
     call W3TAGE('BUFR_TRANMLS')
     call ERREXIT(1)
  endif


! Open HDF5 file
! --------------
  call H5FOPEN_F( o3mr_name, H5F_ACC_RDONLY_F, fid, error)
  if(error.ne.0) then 
     write(*,*) "##### PROBLEM: Cannot open HDF5 file ", trim(o3mr_name), &
      ", error = ", error
     call W3TAGE('BUFR_TRANMLS')
     call ERREXIT(2)
  endif


! Get file (granule) start date (UTC) (YYYYMMDD)
! ----------------------------------------------
  call GRANULE_CALENDAR_DATE(granuleyear,granulemonth,granuleday)

  print *
  write(*,'("File (granule) start Date (UTC): ", I04,I0.2,I0.2)') &
   granuleyear,granulemonth,granuleday
  print *


! Open groupname "/HDFEOS/SWATHS/O3"
! ----------------------------------
  call H5GOPEN_F(fid,trim(o3mr_swath), gid, error)
  if(error.ne.0) then 
     write(*,*) "##### Cannot open groupname ", trim(o3mr_swath), &
      ", error = ",error
     call W3TAGE('BUFR_TRANMLS')
     call ERREXIT(3)
  endif

! Extract number of scan lines in file and set to "nATscans"
! ----------------------------------------------------------
  call H5DOPEN_F(gid, "Data Fields/"//ds_o3mr%name , ds_o3mr%dataset_id, error)
  call CHECK_ERROR(error)
  call H5DGET_SPACE_F(ds_o3mr%dataset_id, space_id, error)
  call CHECK_ERROR(error)
  call H5SGET_SIMPLE_EXTENT_NDIMS_F(space_id, ndims, error)
  if(error.lt.0) then
     write(*,*) "error encountered at H5SGET_SIMPLE_EXTENT_NDIMS_F"
     call W3TAGE('BUFR_TRANMLS')
     call ERREXIT(9)
  end if
  allocate(dims(ndims))
  allocate(maxdims(ndims))
  call H5SGET_SIMPLE_EXTENT_DIMS_F(space_id, dims, maxdims, error)
  if(error.lt.0) then
     write(*,*) "error encountered at H5SGET_SIMPLE_EXTENT_DIMS_F"
     call W3TAGE('BUFR_TRANMLS')
     call ERREXIT(10)
  end if

  nATscans=dims(2)
  nLevels=dims(1)
  if(nATscans.le.0 .or. nLevels.le.0 .or. nLevels.gt.255) then
     write(*,*) "##### Invalid number of scan lines or levels in file (=", &
      nATscans,nLevels,")"
     call W3TAGE('BUFR_TRANMLS')
     call ERREXIT(6)
  endif

  print *
  write(*,*) "Number of pressure level measurements = ",nLevels
  write(*,*) "Number of scan lines                  = ",nATscans
  print *

! Allocate fields that will next be read
! --------------------------------------
  allocate(o3mr(nLevels,nATscans))
  allocate(precisn(nLevels,nATscans))
  allocate(qualt(nATscans))
  allocate(lat(nATscans))
  allocate(lon(nATscans))
  allocate(sza(nATscans))
  allocate(time(nATscans))
  allocate(press(nLevels))
  allocate(press2(nLevels,nATscans))
  allocate(stat(nATscans))
  allocate(bnry(nATscans))
  allocate(conv(nATscans))

  swath_dims=(/nLevels, nATscans /)


! Extract ozone mixing ratio values (in "Data Fields" swath) & set to "o3mr"
! --------------------------------------------------------------------------
  call H5DOPEN_F(gid, "Data Fields/"//ds_o3mr%name , ds_o3mr%dataset_id, error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_o3mr%dataset_id ,H5T_NATIVE_REAL , o3mr, swath_dims, error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_o3mr%dataset_id, error)
  call CHECK_ERROR(error)

  min=1.0e06
  max=-1.0e-06
  do nscan=1,nATscans
  do nLev=1,nLevels
     o3mr(nLev,nscan)=o3mr(nLev,nscan)*1.0e+06   ! convert to ppmv
     if(o3mr(nLev,nscan) > max ) then
         max=o3mr(nLev,nscan)
         imax=nLev;jmax=nscan
     end if
     if(o3mr(nLev,nscan) < min ) then
         min=o3mr(nLev,nscan)
         imin=nLev;jmin=nscan
     end if
  end do
  end do
  print*, 'the minimum value of o3mr is:', min, ' at level/scan ', imin,jmin
  print*, 'the maximum value of o3mr is:', max, ' at level/scan ', imax,jmax


! Extract radiative cloud fraction (in "Data Fields" swath) & set to "precisn"
! ---------------------------------------------------------------------------
  call H5DOPEN_F(gid, "Data Fields/"//ds_precisn%name , ds_precisn%dataset_id, &
   error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_precisn%dataset_id ,H5T_NATIVE_REAL , precisn, swath_dims, &
   error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_precisn%dataset_id, error)
  call CHECK_ERROR(error)

  min=1.0e06
  max=-1.0e-06
  do nscan=1,nATscans
  do nLev=1,nLevels
     precisn(nLev,nscan)=precisn(nLev,nscan)*1.0e+06   ! convert to ppmv
     if(precisn(nLev,nscan) > max ) then
         max=precisn(nLev,nscan)
         imax=nLev;jmax=nscan
     end if
     if(precisn(nLev,nscan) < min ) then
         min=precisn(nLev,nscan)
         imin=nLev;jmin=nscan
     end if
  end do
  end do
  print*, 'the minimum value of precisn is:', min, ' at level/scan ', imin,jmin
  print*, 'the maximum value of precisn is:', max, ' at level/scan ', imax,jmax

  temp_dims=(/nAtscans/)


! Extract quality (in "Data Fields" swath) and set to "qualt"
! ------------------------------------------------------------------
  call H5DOPEN_F(gid, "Data Fields/"//ds_qualt%name , ds_qualt%dataset_id, &
                 error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_qualt%dataset_id ,H5T_NATIVE_REAL , qualt, temp_dims, &
                 error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_qualt%dataset_id, error)
  call CHECK_ERROR(error)

  min=1.0e06
  max=-1.0e-06
  do nscan=1,nATscans
     qualt(nscan)=qualt(nscan)*10.0
     if(qualt(nscan) > max ) then
         max=qualt(nscan)
         jmax=nscan
     end if
     if(qualt(nscan) < min ) then
         min=qualt(nscan)
         jmin=nscan
     end if
  end do
  print*, 'the minimum value of qualt is:', min, ' at scan ', jmin
  print*, 'the maximum value of qualt is:', max, ' at scan ', jmax


! Extract status (in "Data Fields" swath) and set to "stat"
! ------------------------------------------------------------------
  call H5DOPEN_F(gid, "Data Fields/"//ds_stat%name , ds_stat%dataset_id, &
                 error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_stat%dataset_id ,H5T_NATIVE_REAL , stat, temp_dims, &
                 error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_stat%dataset_id, error)
  call CHECK_ERROR(error)

  min=1.0e06
  max=-1.0e-06
  do nscan=1,nATscans
     if(stat(nscan) > max ) then
         max=stat(nscan)
         jmax=nscan
     end if
     if(stat(nscan) < min ) then
         min=stat(nscan)
         jmin=nscan
     end if
  end do
  print*, 'the minimum value of stat is:', min, ' at scan ', jmin
  print*, 'the maximum value of stat is:', max, ' at scan ', jmax

! ------------------------------------------------------------------
! convert decimal to binary for the parameter status
  do nscan=1,nATscans
     decimal=stat(nscan)
     call dec2bin(decimal,binary)
     call bin2int(binary,intbf)
!    print*, 'decimal, binary= ', stat(nscan),'-----', binary,'-----',intbf
     bnry(nscan)=intbf
  end do
  if(1==0) then
  print*,'--------------------------------------------------------'
  call dec2bin(1,binary)
  call bin2int(binary,intbf)
  print*, 'decimal, binary= ', 1,'-----', binary,'-----',intbf
  call dec2bin(2,binary)
  call bin2int(binary,intbf)
  print*, 'decimal, binary= ', 2,'-----', binary,'-----',intbf
  call dec2bin(3,binary)
  call bin2int(binary,intbf)
  print*, 'decimal, binary= ', 3,'-----', binary,'-----',intbf
  call dec2bin(5,binary)
  call bin2int(binary,intbf)
  print*, 'decimal, binary= ', 5,'-----', binary,'-----',intbf
  print*,'--------------------------------------------------------'
  end if

! ------------------------------------------------------------------

! Extract convergence (in "Data Fields" swath) and set to "conv"
! ------------------------------------------------------------------
  call H5DOPEN_F(gid, "Data Fields/"//ds_conv%name , ds_conv%dataset_id, &
                 error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_conv%dataset_id ,H5T_NATIVE_REAL , conv, temp_dims, &
                 error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_conv%dataset_id, error)
  call CHECK_ERROR(error)

! print*,conv(1),conv(1000),conv(2000),conv(3000),conv(nATscans)

  min=1.0e06
  max=-1.0e-06
  do nscan=1,nATscans
     if(conv(nscan) > max ) then
         max=conv(nscan)
         jmax=nscan
     end if
     if(conv(nscan) < min ) then
         min=conv(nscan)
         jmin=nscan
     end if
  end do
  print*, 'the minimum value of conv is:', min, ' at scan ', jmin
  print*, 'the maximum value of conv is:', max, ' at scan ', jmax

! Extract latitude (in "Geolocation Fields" swath) and set to "lat"
! -----------------------------------------------------------------
  call H5DOPEN_F(gid, "Geolocation Fields/"//ds_lat%name , ds_lat%dataset_id, &
                 error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_lat%dataset_id ,H5T_NATIVE_REAL , lat, temp_dims, error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_lat%dataset_id, error)
  call CHECK_ERROR(error)

  min=1.0e06
  max=-1.0e-06
  do nscan=1,nATscans
     if(lat(nscan) > max ) then
         max=lat(nscan)
         jmax=nscan
     end if
     if(lat(nscan) < min ) then
         min=lat(nscan)
         jmin=nscan
     end if
  end do
  print*, 'the minimum value of lat is:', min, ' at scan ', jmin
  print*, 'the maximum value of lat is:', max, ' at scan ', jmax


! Extract longitude (in "Geolocation Fields" swath) and set to "lon"
! ------------------------------------------------------------------
  call H5DOPEN_F(gid, "Geolocation Fields/"//ds_lon%name , ds_lon%dataset_id, &
                 error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_lon%dataset_id ,H5T_NATIVE_REAL , lon, temp_dims, error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_lon%dataset_id, error)
  call CHECK_ERROR(error)

  min=1.0e06
  max=-1.0e-06
  do nscan=1,nATscans
     if(lon(nscan) > max ) then
         max=lon(nscan)
         jmax=nscan
     end if
     if(lon(nscan) < min ) then
         min=lon(nscan)
         jmin=nscan
     end if
  end do
  print*, 'the minimum value of lon is:', min, ' at scan ', jmin
  print*, 'the maximum value of lon is:', max, ' at scan ', jmax


! Extract solar zenith angle (in "Geolocation Fields" swath) and set to "sza"
! ---------------------------------------------------------------------------
  call H5DOPEN_F(gid, "Geolocation Fields/"//ds_sza%name , ds_sza%dataset_id, &
                 error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_sza%dataset_id ,H5T_NATIVE_REAL , sza, temp_dims, error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_sza%dataset_id, error)
  call CHECK_ERROR(error)

  min=1.0e06
  max=-1.0e-06
  do nscan=1,nATscans
     sza(nscan)=sza(nscan)-90.0  ! range (-90, 90)
     if(sza(nscan) > max ) then
         max=sza(nscan)
         jmax=nscan
     end if
     if(sza(nscan) < min ) then
         min=sza(nscan)
         jmin=nscan
     end if
  end do
  print*, 'the minimum value of sza is:', min, ' at scan ', jmin
  print*, 'the maximum value of sza is:', max, ' at scan ', jmax


! Extract local solar time (in "Geolocation Fields" swath) and set to "time"
! -----------------------------------------------------------------------------
  call H5DOPEN_F(gid,"Geolocation Fields/"//ds_time%name , ds_time%dataset_id, &
   error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_time%dataset_id ,H5T_NATIVE_DOUBLE , time, temp_dims, error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_time%dataset_id, error)
  call CHECK_ERROR(error)

  print('(1X,"Time at scan 1",4(1X,I0),": ",5F12.0)'), (nATscans/4)*1, &
   (nATscans/4)*2,(nATscans/4)*3,nATscans,time(1),time((nATscans/4)*1), &
   time((nATscans/4)*2),time((nATscans/4)*3),time(nATscans)

  min_8=1.0e09
  max_8=-1.0e-09
  do nscan=1,nATscans
     if(time(nscan) > max_8 ) then
         max_8=time(nscan)
         jmax=nscan
     end if
     if(time(nscan) < min_8 ) then
         min_8=time(nscan)
         jmin=nscan
     end if
  end do
  print('(" the minimum value of time is:",F12.0," at scan ",I0)'), min_8,jmin
  print('(" the maximum value of time is:",F12.0," at scan ",I0)'), max_8,jmax

! Extract pressure level (in "Geolocation Fields" swath) and set to "press"
! -----------------------------------------------------------------------------
  call H5DOPEN_F(gid,"Geolocation Fields/"//ds_press%name,ds_press%dataset_id, &
                 error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_press%dataset_id ,H5T_NATIVE_REAL , press, swath_dims,error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_press%dataset_id, error)
  call CHECK_ERROR(error)

  min=1.0e06
  max=-1.0e-06
  do nLev=1,nLevels
     do nscan=1,nATscans
        press2(nLev,nscan)=press(nLev)*100.0  !mb to Pascal
!        print*, 'press(',nLev,')=',press(nLev)
!        print*, 'press2(',nLev,',',nscan,')=',press2(nLev,nscan)
     end do
     if(press(nLev) > max ) then
         max=press(nLev)*100.0
         jmax=nLev
     end if
     if(press(nLev) < min ) then
         min=press(nLev)*100.0
         jmin=nLev
     end if
  end do
  print*, 'the minimum value of press is:', min, ' at level ', jmin
  print*, 'the maximum value of press is:', max, ' at level ', jmax


  subset = 'NC008015'  ! Message type for MLS is "NC008015"


  call DATELEN(10)


! Open output BUFR file
! ---------------------
!!!!!  call OPENBF(lunbfr,'OUT',lundx)
  call OPENBF(lunbfr,'NODX',lundx)

! Allocate fields that will next hold bufr data
! ---------------------------------------------
  allocate(o3mrp_8(3,nLevels))

! Open output diagnostic print file
! ---------------------------------
  open(lunprt,file=o3mr_output,form='formatted')

    
! Loop through the scans in the file
! ----------------------------------
     do nscan=1,nATscans

     write(lunprt,*) "   ---> At scan ",nscan
!!!  write(*,*) "==> At scan ",nscan

     write(lunprt,*) "pressure values on ", nLevels, " levels:"
     write(lunprt,*) press2(:,nscan)

     write(lunprt,*) "ozone mixing ratio values on ", nLevels, " levels:"
     write(lunprt,*) o3mr(:,nscan)

     write(lunprt,*) "precision on ", nLevels, " levels:"
     write(lunprt,*) precisn(:,nscan)
        iread = iread + 1

!       if(time(nscan).lt.0. .or. time(nscan).gt.24.) then
!          write(*,*) "##### Invalid date at scan ", &
!           nscan,", number of hours in LocalSolarTime = ",time(nscan), &
!            " -- skip!!"
!          iskip = iskip + 1
!          cycle
!       endif
        if(lat(nscan).lt.-90. .or. lat(nscan) .gt.90.) then
           write(*,*) "##### Invalid latitude at scan ", &
             nscan,", latitude = ",lat(nscan)," -- skip!!"
           iskip = iskip + 1
           cycle
        endif
        if(lon(nscan).lt.-180. .or. lon(nscan).gt.180.) then
           write(*,*) "#### Invalid longitude at scan ", &
             nscan,", longitude = ",lon(nscan)," -- skip!!"
           iskip = iskip + 1
           cycle
        endif

        rinc=0.0
        rinc(4)=time(nscan)

        idat=0
        idat(1)=granuleyear
        idat(2)=granulemonth
        idat(3)=granuleday
        idat(1)=1993
        idat(2)=01
        idat(3)=01

        call W3MOVDAT(rinc,idat,jdat)

        idate = jdat(1)*1000000 + jdat(2)*10000 + jdat(3)*100 + jdat(5)

!       print*, "idate= ", idate, "year month day: ", idat(1),idat(2),idat(3)

        write(lunprt,*) "          time of scan (HH : MM : SS)=",jdat(5), &
         ":",jdat(6),":",jdat(7)

!       print*, 'seconds from TAI93: ', time(nscan)
!       print*, 'jdate= ',jdat 
!       print*, "          time of scan (HH : MM : SS)=",jdat(5), &
!        ":",jdat(6),":",jdat(7)
    

! Open a new output BUFR message (first time in, or if idate is different than
!  for previous output message 
! ----------------------------------------------------------------------------
        call OPENMB(lunbfr,subset,idate)

!    stop


!  TRANSLATE THE MLS REPORT TO BUFR FORMAT
!  ---------------------------------------------------------------------
!  NC008015 | SAID     CLAT     CLON     SOZA     YEAR     MNTH
!  NC008015 | DAYS     HOUR     MINU     SECO     PCCF     CONV     MLST
!  NC008015 | {OZOMXPSQ}           
!           |
!  OZOMXPSQ | PRLC     OZMX     OZMP 
!  ---------------------------------------------------------------------

        obs_8 = bmiss

        obs_8(1)  = 785                      ! satellite id (Aura = 785)
        obs_8(2)  = lat(nscan)               ! latitude  (N+, S-)
        obs_8(3)  = lon(nscan)               ! longitude (-180 to +180, W-, E+)
        obs_8(4)  = sza(nscan)               ! solar zenith angle
        obs_8(5)  = jdat(1)                  ! year
        obs_8(6)  = jdat(2)                  ! month
        obs_8(7)  = jdat(3)                  ! day
        obs_8(8)  = jdat(5)                  ! hour
        obs_8(9)  = jdat(6)                  ! minute
        obs_8(10) = jdat(7)                  ! second
        obs_8(11) = qualt(nscan)             ! Quality*10
        obs_8(12) = conv(nscan)              ! convergence

!!! BE CAREFUL - NEED TO REDEFINE stat(nscan) as bnry(nscan) TO COMPLY WITH
!!!  BUFR FLAG TABLE DEFINITION (BUFR BITS ARE DEFINED FROM 1,2,3,...,18
!!!  WHERE AS HDF BITS ARE DEFINED FROM 18,17,16,....,0)
        obs_8(13) = bnry(nscan)              ! status

        if(nscan.eq.1 .or. nscan.eq.50) then  ! sample scans 1 and 50
           write(*,*)"    ----> At scan ",nscan
           write(*,'("           satellite id           = ",F7.0)') obs_8(1)
           write(*,'("           latitude               = ",F7.2)') obs_8(2)
           write(*,'("           longitude              = ",F7.2)') obs_8(3)
           write(*,'("           solar zenith angle     = ",F7.2)') obs_8(4)
!          write(*,'("          UTC seconds in day     = ",)',*) time(nscan)
           write(*,*)"          UTC seconds in day     = ", time(nscan)
           write(*,&
             '("           UTC time of scan       = ",I4,3I2.2,2(":",I2.2))') &
            jdat(1),jdat(2),jdat(3),jdat(5),jdat(6),jdat(7)
           write(*,'("           quality                = ",F7.0)') obs_8(11)
           write(*,'("           convergence            = ",F7.3)') obs_8(12)
           write(*,'("           ozone status           = ",F7.0)') obs_8(13)
        endif

! Loop through the levels in this scan
! ------------------------------------
  do nLev=1,nLevels

        o3mrp_8(1,nLev) = press2(nLev,nscan)   ! pressure in Pascals
        o3mrp_8(2,nLev) = o3mr(nLev,nscan)     ! ozone mixing ratio on pressure
                                               !  level in PPMV
        o3mrp_8(3,nLev) = precisn(nLev,nscan)  ! precision of the data in PPMV

        if(nscan.eq.1 .or. nscan.eq.50) then  ! sample scans 1 and 50
           write(*,*)"    ----> On level               = ", nLev
           write(*,*)"          pressure level         = ", o3mrp_8(1,nLev)
           write(*,*)"          ozone mixing ratio     = ", o3mrp_8(2,nLev)
           write(*,*)"    ozone mixing ratio precision = ", o3mrp_8(3,nLev)
        endif

  enddo

! Store obs_8 array into BUFR subset
! ----------------------------------
        call UFBINT(lunbfr,obs_8(1),7,1,iret, &
         'SAID CLAT CLON SOZA YEAR MNTH DAYS')
        call UFBINT(lunbfr,obs_8(8),6,1,iret, &
         'HOUR MINU SECO PCCF CONV MLST')
        call UFBINT(lunbfr,o3mrp_8,3,nLevels,iret,'PRLC OZMX OZMP')

! Encode subset into BUFR message
! -------------------------------
        call WRITSB(lunbfr)
        iwrite = iwrite + 1

  enddo

! All reports have been processed
! Close output BUFR file & write out any incomplete messages
! ----------------------------------------------------------

  call CLOSBF(lunbfr)


! Deallocate allocated arrays
! ---------------------------
  deallocate(o3mr)
  deallocate(precisn)
  deallocate(lat)
  deallocate(lon)
  deallocate(sza)
  deallocate(time)
  deallocate(qualt)
  deallocate(press)
  deallocate(press2)
  deallocate(stat)
  deallocate(bnry)
  deallocate(conv)


  close(lunprt)


! Close the Group
! ---------------
  call H5GCLOSE_F(gid,error)


! Close the file
! --------------
  call H5FCLOSE_F(fid,error)
  call CHECK_ERROR(error)


! Close FORTRAN interface
! -----------------------
  call H5CLOSE_F(error)
  call CHECK_ERROR(error)

  print *
  print *, 'Number of scans read    = ',iread
  print *, 'Number of scans skipped = ',iskip
  print *, 'Number of scans written = ',iwrite
  print *

  call W3TAGE('BUFR_TRANMLS')

  stop

  contains 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Subroutine to get year, month, day at start of file (granule)
! -------------------------------------------------------------

  subroutine GRANULE_CALENDAR_DATE(year,month,day)

  ! ***MUST DEFINE ALL VARIABLES***
    implicit none

    integer, intent(inout) :: year,month,day
    integer(HID_T) :: fileattrib_gid
    integer :: date(1)  ! Must be array of one, for calling hdf5 attribute
                        !  function
    character(LEN=*), parameter :: file_attr_swath=&
                                          "/HDFEOS/ADDITIONAL/FILE_ATTRIBUTES/"
    character(LEN=256) :: datename


  ! Open groupname "/HDFEOS/ADDITIONAL/FILE_ATTRIBUTES/"
  ! ----------------------------------------------------
    call H5GOPEN_F(fid,file_attr_swath, fileattrib_gid, error)
    if(error.ne.0) then 
       write(*,*) "##### Cannot open groupname ",file_attr_swath, &
        ", error = ",error
       call W3TAGE('BUFR_TRANMLS')
       call ERREXIT(3)
    endif


  ! Extract file day (day of month at start of the granule) and set to "day"
  ! ------------------------------------------------------------------------
    datename="GranuleDay"
    call H5AOPEN_NAME_F(fileattrib_gid, trim(datename), attr_id, error) 
    if(error.ne.0) then 
       write(*,*) "##### Cannot open GranuleDay attribute, error = ",error
       call W3TAGE('BUFR_TRANMLS')
       call ERREXIT(4)
    endif
    call H5AREAD_F(attr_id,  H5T_NATIVE_INTEGER, date,data_dims,error) 
    if(error.ne.0) then 
       write(*,*) "##### Cannot read GranuleDay, error = ",error
       call W3TAGE('BUFR_TRANMLS')
       call ERREXIT(5)
    endif
    call H5ACLOSE_F(attr_id,error)
    if(error.ne.0) then 
       write(*,*) &
        "----- Cannot close GranuleDay attribute (non-fatal), error = ",error
    endif
    day=date(1)


  ! Extract file month (month at start of granule) and set to "month"
  ! -----------------------------------------------------------------
    datename="GranuleMonth"
    call H5AOPEN_NAME_F(fileattrib_gid, trim(datename), attr_id, error) 
    if(error.ne.0) then 
       write(*,*) "##### Cannot open GranuleMonth attribute, error = ",error
       call W3TAGE('BUFR_TRANMLS')
       call ERREXIT(4)
    endif
    call H5AREAD_F(attr_id,  H5T_NATIVE_INTEGER, date,data_dims,error) 
    if(error.ne.0) then 
       write(*,*) "##### Cannot read GranuleMonth, error = ",error
       call W3TAGE('BUFR_TRANMLS')
       call ERREXIT(5)
    endif
    call H5ACLOSE_F(attr_id,error)
    if(error.ne.0) then 
       write(*,*) &
        "----- Cannot close GranuleMonth attribute (non-fatal), error = ",error
    endif
    month=date(1)


  ! Extract file year (4-digit year at start of granule) and set to "year"
  ! ----------------------------------------------------------------------
    datename="GranuleYear"
    call H5AOPEN_NAME_F(fileattrib_gid, trim(datename), attr_id, error) 
    if(error.ne.0) then 
       write(*,*) "##### Cannot open GranuleYear attribute, error = ",error
       call W3TAGE('BUFR_TRANMLS')
       call ERREXIT(4)
    endif
    call H5AREAD_F(attr_id,  H5T_NATIVE_INTEGER, date,data_dims,error) 
    if(error.ne.0) then 
       write(*,*) "##### Cannot read GranuleYear, error = ",error
       call W3TAGE('BUFR_TRANMLS')
       call ERREXIT(5)
    endif
    call H5ACLOSE_F(attr_id,error)
    if(error.ne.0) then 
       write(*,*) &
        "----- Cannot close GranuleYear attribute (non-fatal), error = ",error
    endif
    year=date(1)


  ! Close the Group
  ! ---------------
    call H5GCLOSE_F( fileattrib_gid, error)
    if(error.ne.0) then 
       write(*,*) "##### Cannot close groupname ", file_attr_swath, &
        ", error = ",error
       call W3TAGE('BUFR_TRANMLS')
       call ERREXIT(7)
    endif

!!!!write(*,*) "granule_calendar_date day is ", day, month, year

  end subroutine GRANULE_CALENDAR_DATE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Generic check_error subroutine
! ------------------------------

  subroutine CHECK_ERROR(error_code)
    implicit none
    integer, intent(in) :: error_code

    if(error_code.ne.0) then 
       write(*,*) &
        "#### Error returned from an HDF5 interface routine, error = ",error
       call W3TAGE('BUFR_TRANMLS')
       call ERREXIT(8)
    endif

  end subroutine CHECK_ERROR

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program BUFR_TRANMLS

SUBROUTINE dec2bin(dec,bin)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dec2bin                  convert decimal number to binary
!   prgmmr: unknown             org: np23                date: 2010-04-06
!
! abstract:  This routine convert a decimal number to binary
!
! program history log:
!   2010-04-06  hliu

!   input argument list:
!     dec  - observation type to process
!
!   output argument list:
!     bin    - number of mls ozone observations read
!
! remarks:
!
! attributes:
!   language: FORTRAN 90 (free format)
!   machine:  NCEP WCOSS
!

    implicit none

! Declare passed variables
    integer,intent(inout) :: dec
    integer,intent(out)   :: bin(18)

! Declare local variables
    integer:: bindec, i

!   Check to determine decimal # is within bounds
    i = 18
    IF ((dec - 2**i) >= 0) THEN
       STOP 'Decimal Number too Large. Must be < 2^17'
    END IF

!   Determine the scalar for each of the decimal positions
    DO WHILE (i >= 1)
       bindec = 2**(i-1)
       IF ((dec - bindec) >= 0) THEN
          bin(i) = 1
          dec = dec - bindec
       ELSE
          bin(i) = 0
       END IF
       i = i - 1
    END DO

    RETURN
END subroutine dec2bin

subroutine bin2int(bin,intbf)

    implicit none

! Declare passed variables
    integer,intent(out) :: intbf
    integer,intent(in)   :: bin(18)

! Declare local variables
    integer:: i

  intbf=0
  do i=1,18
     intbf=intbf+bin(i)*2**(18-i)
  end do

    RETURN
END subroutine bin2int
