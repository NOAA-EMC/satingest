  program BUFR_TRANOMI

!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: BUFR_TRANOMI
!   PRGMMR: KEYSER           ORG: NP22        DATE: 2014-01-20
!
! Abstract: Reads in Aura OMI total ozone data from a raw HDF5 format file
!   (OMTO3) and reformats them into BUFR in preparation for their ingest into
!   the BUFR data base on the NCEP supercomputers.
!
! Program history log:
! 2008-12-31  Greg Krasowski - Original author (adapted from decoder written
!             by Trevor Beck, NESDIS Sensor Physics Branch)
! 2009-09-30  Dennis Keyser  - Prepared for implementation into NCEP production
! 2012-11-16  Dennis Keyser  - Changes to run on WCOSS (minor).
! 2014-01-20  Dennis Keyser  - Minor changes
!
! Usage:
!
!   Input files:
!     command line argument
!                 - HDF5 binary file containing raw OMI OMTO3 data
!     unit 31     - NCEP BUFR mnemonic table
!
!   Output files:
!     unit 06     - printout
!     unit 51     - NCEP BUFR file containing OMI data
!     unit 52     - diagnostic (ASCII) output file (currently not written to)
!
!   Subprograms called:
!     Unique:     - TO3_DATASETS GRANULE_CALENDAR_DATE CHECK_ERROR
!     Library:
!       W3NCO     - W3TAGB     W3FI01    ERREXIT   W3MOVDAT  GBYTE
!       BUFRLIB   - DATELEN    OPENBF    OPENMB    UFBINT
!                   WRITSB     CLOSBF    UPFTBV
!       HDF5LIB   - H5OPEN_F   H5FOPEN_F H5GOPEN_F H5AOPEN_NAME_F H5AREAD_F
!                   H5ACLOSE_F H5DOPEN_F H5DREAD_F H5DCLOSE_F     H5GCLOSE_F
!                   H5FCLOSE_F H5CLOSE_F
!       SYSTEM    - GET_COMMAND_ARGUMENT
!
!
!   Exit states
!     0 = no errors detected
!    >0 = some type of error detected
!          1 = Cannot open HDF5 interface
!          2 = Cannot open HDF5 file
!          3 = Cannot open a groupname
!          4 = Cannot open an attribute
!          5 = Cannot read attribute-based variable
!          6 = Invalid number of along-track scan lines in file
!          7 = Cannot close a groupname
!          8 = Error returned from an HDF5 interface routine
!
! Remarks:
!     Note that input file is specified from command line argument.
!
!        Code proceeds as follows: 
!
!         1) Open the file.
!         2) Read the calendar date (UTC).
!         3) Read the number of along-track scans, typically about 1644.
!         4) Allocate the variables, based on number of along track scans.
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

  use to3_datasets

!
! ***MUST DEFINE ALL VARIABLES***
  implicit none 

! Set parameters and declare variables
! ------------------------------------
  real    :: rinc(5)
  real(8) :: obs_8(19),bmiss

  integer :: error        ! Error flag returned by HDF5 interface calls
  integer(HID_T) :: fid          ! File ID number
  integer(HID_T) :: gid          ! Group ID number
  integer(HID_T) :: attr_id      ! Attribute ID
  integer :: nXtrk        ! Index pointing to cross-track measurement (scan
                          !  position, 1 to nXtracks)
  integer :: nscan        ! Index pointing to along-track scan (1 to nATscans)
  integer :: numTimes(1)  ! Must be array of one, for calling hdf5 attribute
                          !  function
  integer :: lunbfr, lunprt, lundx, iwrite, iskip, iread, iret
  integer :: idat(8), jdat(8), ibit(31), ii, nib
  integer :: idate, iunpk, toqc, toqf, nbytw, nbitw, stko
  integer(hsize_t), dimension(1) :: data_dims  ! Required for call to read
                                               !  attribute
  integer(hsize_t), dimension(2) :: swath_dims ! Required for call to read
                                               !  dataset
  integer(kind=4) :: granuleyear,granulemonth,granuleday
  integer, parameter :: nXtracks=60   ! OMI has exactly 60 across-track
                                      !  measurements (scan positions)

  character(LEN=256) :: to3_name      ! Input filename, read from command line
                                      !  argument
  character(LEN=256) :: to3_output
  character(LEN=8)   :: subset
  character(LEN=*), parameter :: totaloz_swath=&
                                          "/HDFEOS/SWATHS/OMI Column Amount O3"

  data lunbfr/51/, lunprt/52/, lundx/31/, bmiss/10e10/, iread/0/, iskip/0/, &
       iwrite/0/

!=============================================================================
!=============================================================================

  call W3TAGB('BUFR_TRANOMI',2014,0020,0068,'NP22')

  print *
  print *, 'Welcome to BUFR_TRANOMI - Version 01-20-2014'
  print *


! Get machine word length (nbytw) and specify number of bits per word (nbitw)
! ---------------------------------------------------------------------------
  call W3FI01(nbytw)
  nbitw = 8*nbytw


  to3_output='omi_output'


! Command line argument is input filename
! ---------------------------------------
  call GET_COMMAND_ARGUMENT(1,to3_name)


! Initialize HDF5 interface
! -------------------------
  call H5OPEN_F(error)
  if(error.ne.0) then 
     write(*,'(" ##### Cannot open HDF5 interface, error = ",I0)') error
     call W3TAGE('BUFR_TRANOMI')
     call ERREXIT(1)
  endif


! Open HDF5 file
! --------------
  call H5FOPEN_F( to3_name, H5F_ACC_RDONLY_F, fid, error)
  if(error.ne.0) then 
     write(*,'(" ##### PROBLEM: Cannot open HDF5 file ",A,", error = ",I0)') &
       trim(to3_name),error
     call W3TAGE('BUFR_TRANOMI')
     call ERREXIT(2)
  endif


! Get file (granule) start date (UTC) (YYYYMMDD)
! ----------------------------------------------
  call GRANULE_CALENDAR_DATE(granuleyear,granulemonth,granuleday)

  print *
  write(*,'(" File (granule) start Date (UTC): ", I04,I0.2,I0.2)') &
   granuleyear,granulemonth,granuleday
  print *


! Open groupname "/HDFEOS/SWATHS/OMI Column Amount O3"
! ----------------------------------------------------
  call H5GOPEN_F(fid,trim(totaloz_swath), gid, error)
  if(error.ne.0) then 
     write(*,'(" ##### Cannot open groupname ",A,", error = ",I0)') &
      trim(totaloz_swath),error
     call W3TAGE('BUFR_TRANOMI')
     call ERREXIT(3)
  endif


! Extract number of along-track scan lines in file and set to "nATscans"
! ----------------------------------------------------------------------
  call H5AOPEN_NAME_F(gid, "NumTimes", attr_id, error) 
  if(error.ne.0) then 
     write(*,'(" ##### Cannot open NumTimes attribute, error = ",I0)') error
     call W3TAGE('BUFR_TRANOMI')
     call ERREXIT(4)
  endif
  call H5AREAD_F(attr_id,  H5T_NATIVE_INTEGER, numTimes,data_dims,error) 
  if(error.ne.0) then 
     write(*,'(" ##### Cannot read NumTimes, error = ",I0)') error
     call W3TAGE('BUFR_TRANOMI')
     call ERREXIT(5)
  endif
  call H5ACLOSE_F(attr_id,error)
  if(error.ne.0) then 
     write(*,'(" ----- Cannot close NumTimes attribute (non-fatal), error = ",&
     I0)') error
  endif
  nATscans=numTimes(1)
  if(nATscans.le.0) then 
     write(*,'(" ##### Invalid number of along-track scan lines in file (=", &
      I0)') nATscans
     call W3TAGE('BUFR_TRANOMI')
     call ERREXIT(6)
  endif

  print *
  write(*,'(" Number of cross-track measurements (scan positions) = ",I0)') &
   nXtracks
  write(*,'(" Number of along-track scan lines                    = ",I0)') &
   nATscans
  print *

! Allocate fields that will next be read
! --------------------------------------
  allocate(tozone(nXtracks,nATscans))
  allocate(cldfcn(nXtracks,nATscans))
  allocate(aflags(nXtracks,nATscans))
  allocate(qflags(nXtracks,nATscans))
  allocate(acidx(nXtracks,nATscans))
  allocate(lat(nXtracks,nATscans))
  allocate(lon(nXtracks,nATscans))
  allocate(vza(nXtracks,nATscans))
  allocate(sza(nXtracks,nATscans))
  allocate(sec(nATscans))

  swath_dims=(/nXtracks, nATscans /)


! Extract best total column ozone solution (in "Data Fields" swath) & set to
!  "tozone"
! --------------------------------------------------------------------------
  call H5DOPEN_F(gid, "Data Fields/"//ds_toz%name , ds_toz%dataset_id, error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_toz%dataset_id ,H5T_NATIVE_REAL , tozone, swath_dims, error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_toz%dataset_id, error)
  call CHECK_ERROR(error)


! Extract radiative cloud fraction (in "Data Fields" swath) & set to "cldfcn"
! ---------------------------------------------------------------------------
  call H5DOPEN_F(gid, "Data Fields/"//ds_cld%name , ds_cld%dataset_id, error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_cld%dataset_id ,H5T_NATIVE_REAL , cldfcn, swath_dims, error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_cld%dataset_id, error)
  call CHECK_ERROR(error)


! Extract alg. flag for best ozone (in "Data Fields" swath) and set to "aflags"
! -----------------------------------------------------------------------------
  call H5DOPEN_F(gid, "Data Fields/"//ds_algflag%name , ds_algflag%dataset_id, &
                 error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_algflag%dataset_id ,H5T_NATIVE_INTEGER , aflags, &
                 swath_dims, error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_algflag%dataset_id, error)
  call CHECK_ERROR(error)


! Extract quality flags (in "Data Fields" swath) and set to "qflags"
! ------------------------------------------------------------------
  call H5DOPEN_F(gid, "Data Fields/"//ds_qflag%name , ds_qflag%dataset_id, &
                 error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_qflag%dataset_id ,H5T_NATIVE_INTEGER , qflags, swath_dims, &
                 error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_qflag%dataset_id, error)
  call CHECK_ERROR(error)


! Extract UV aerosol index (in "Data Fields" swath) and set to "acidx"
! --------------------------------------------------------------------
  call H5DOPEN_F(gid, "Data Fields/"//ds_acidx%name , ds_acidx%dataset_id, &
                 error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_acidx%dataset_id ,H5T_NATIVE_REAL , acidx, swath_dims, &
                 error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_acidx%dataset_id, error)
  call CHECK_ERROR(error)


! Extract latitude (in "Geolocation Fields" swath) and set to "lat"
! -----------------------------------------------------------------
  call H5DOPEN_F(gid, "Geolocation Fields/"//ds_lat%name , ds_lat%dataset_id, &
                 error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_lat%dataset_id ,H5T_NATIVE_REAL , lat, swath_dims, error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_lat%dataset_id, error)
  call CHECK_ERROR(error)


! Extract longitude (in "Geolocation Fields" swath) and set to "lon"
! ------------------------------------------------------------------
  call H5DOPEN_F(gid, "Geolocation Fields/"//ds_lon%name , ds_lon%dataset_id, &
                 error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_lon%dataset_id ,H5T_NATIVE_REAL , lon, swath_dims, error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_lon%dataset_id, error)
  call CHECK_ERROR(error)


! Extract viewing zenith angle (in "Geolocation Fields" swath) and set to "vza"
! -----------------------------------------------------------------------------
  call H5DOPEN_F(gid, "Geolocation Fields/"//ds_vza%name , ds_vza%dataset_id, &
                 error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_vza%dataset_id ,H5T_NATIVE_REAL , vza, swath_dims, error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_vza%dataset_id, error)
  call CHECK_ERROR(error)


! Extract solar zenith angle (in "Geolocation Fields" swath) and set to "sza"
! ---------------------------------------------------------------------------
  call H5DOPEN_F(gid, "Geolocation Fields/"//ds_sza%name , ds_sza%dataset_id, &
                 error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_sza%dataset_id ,H5T_NATIVE_REAL , sza, swath_dims, error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_sza%dataset_id, error)
  call CHECK_ERROR(error)


! Extract seconds in day (UTC) (in "Geolocation Fields" swath) and set to "sec"
!  (Note: No nXtracks dimension on sec)
! -----------------------------------------------------------------------------
  call H5DOPEN_F(gid, "Geolocation Fields/"//ds_sec%name , ds_sec%dataset_id, &
   error) 
  call CHECK_ERROR(error)
  call H5DREAD_F(ds_sec%dataset_id ,H5T_NATIVE_REAL , sec, swath_dims, error)
  call CHECK_ERROR(error)
  call H5DCLOSE_F(ds_sec%dataset_id, error)
  call CHECK_ERROR(error)


  subset = 'NC008013'  ! Message type for OMI is "NC008013"


  call DATELEN(10)


! Open output BUFR file
! ---------------------
!!!!!  call OPENBF(lunbfr,'OUT',lundx)
  call OPENBF(lunbfr,'NODX',lundx)


! Open output diagnostic print file
! ---------------------------------
!!open(lunprt,file=to3_output,form='formatted')


! Loop through the cross-track measurements (scan positions) in data file
! -----------------------------------------------------------------------
  do nXtrk=1,nXtracks

  !!!write(lunprt,*) "==> At cross-track measurement (scan position) ",nXtrk
     write(*,*) "==> At cross-track measurement (scan position) ",nXtrk

  !!!write(lunprt,*) "latitude:"
  !!!write(lunprt,*) lat(nXtrk,:)

  !!!write(lunprt,*) "longitude:"
  !!!write(lunprt,*) lon(nXtrk,:)

  !!!write(lunprt,*) "viewing zenith angle:"
  !!!write(lunprt,*) vza(nXtrk,:)

  !!!write(lunprt,*) "solar zenith angle:"
  !!!write(lunprt,*) sza(nXtrk,:)

  !!!write(lunprt,*) "best total column ozone solution:"
  !!!write(lunprt,*) tozone(nXtrk,:)

  !!!write(lunprt,*) "radiative cloud fraction:"
  !!!write(lunprt,*) cldfcn(nXtrk,:)

  !!!write(lunprt,*) "aerosol index:"
  !!!write(lunprt,*) acidx(nXtrk,:)

  !!!write(lunprt,*) "algorithm flag for best ozone:"
  !!!write(lunprt,*) aflags(nXtrk,:)

  !!!write(lunprt,*) "quality flags:"
  !!!write(lunprt,*) qflags(:,:)

! Loop through the along-track scans in each cross-track measurement (scan pos.)
! ------------------------------------------------------------------------------
     do nscan=1,nATscans

        iread = iread + 1

    !!! write(lunprt,*) "   ---> At along-track scan ",nscan
        if(sec(nscan).lt.0. .or. sec(nscan).gt.86400.) then
           write(*,'(" ##### Invalid date at scan -  track ",I0,", scan ",I0, &
            ", number of seconds in day = ",E," -- skip!!")') &
            nXtrk,nscan,sec(nscan)

           iskip = iskip + 1
           cycle
        endif
        if(lat(nXtrk,nscan).lt.-90. .or. lat(nXtrk,nscan) .gt.90.) then
           write(*,'(" ##### Invalid latitude at scan position ",I0,", scan ", &
            I0,", latitude = ",I0," -- skip!!")') nXtrk,nscan,lat(nXtrk,nscan)
           iskip = iskip + 1
           cycle
        endif
        if(lon(nXtrk,nscan).lt.-180. .or. lon(nXtrk,nscan).gt.180.) then
           write(*,'(" #### Invalid longitude at scan position ",I0,", scan ", &
            I0,", longitude = ",I0," -- skip!!")') nXtrk,nscan,lon(nXtrk,nscan)
           iskip = iskip + 1
           cycle
        endif

        if(qflags(nXtrk,nscan).lt.65535.) then

! Pull the last 4 bits out of qflags to determine if orbit is ascending or
!  descending and to obtain the total ozone quality code
! ------------------------------------------------------------------------
           call GBYTE(qflags(nXtrk,nscan),iunpk,nbitw-4,4)
           toqc = mod(iunpk,8)      ! total ozone quality code
           stko = 0                 ! ascending orbit
           if(iunpk.gt.7) stko = 1  ! descending orbit

! Pull the first 28 (4-byte word machine) or 60 (8-byte word machine) bits out
!  of qflags and multiply by 2 to obtain the total ozone quality flag {multiply
!  by 2 allows for extra bit (=0) at end to hold "missing" when all bits on}
! -----------------------------------------------------------------------------
           call GBYTE(qflags(nXtrk,nscan),iunpk,0,nbitw-4)
           toqf = 2*iunpk           ! total ozone quality flag
    !!!    write(*,*) "qflags, stko, toqc, toqf: ", &
    !!!     qflags(nXtrk,nscan), stko, toqc, toqf
        endif

    !!! write(lunprt,*) "          seconds in day (UTC)=",sec(nscan)

        rinc=0.0
        rinc(4)=sec(nscan)

        idat=0
        idat(1)=granuleyear
        idat(2)=granulemonth
        idat(3)=granuleday

        call W3MOVDAT(rinc,idat,jdat)

        idate = jdat(1)*1000000 + jdat(2)*10000 + jdat(3)*100 + jdat(5)

    !!! write(lunprt,*) "          time of scan (HH : MM : SS)=",jdat(5), &
    !!!  ":",jdat(6),":",jdat(7)
    

! Open a new output BUFR message (first time in, or if idate is different than
!  for previous output message 
! ----------------------------------------------------------------------------
        call OPENMB(lunbfr,subset,idate)


!  TRANSLATE THE OMI REPORT TO BUFR FORMAT
!  ---------------------------------------------------------------------
!  NC008013 | SAID     CLAT     CLON     VZAN     SOZA     YEAR     MNTH
!  NC008013 | DAYS     HOUR     MINU     SECO     STKO     OZON     AFBO
!  NC008013 | TOQC     TOQF     ACIDX    CLDMNT   FOVN
!  ---------------------------------------------------------------------

        obs_8 = bmiss

        obs_8(1)  = 785                      ! satellite id (Aura = 785)
        obs_8(2)  = lat(nXtrk,nscan)         ! latitude  (N+, S-)
        obs_8(3)  = lon(nXtrk,nscan)         ! longitude (-180 to +180, W-, E+)
        obs_8(4)  = vza(nXtrk,nscan)         ! viewing zenith angle
        obs_8(5)  = sza(nXtrk,nscan)         ! solar zenith angle
        obs_8(6)  = jdat(1)                  ! year
        obs_8(7)  = jdat(2)                  ! month
        obs_8(8)  = jdat(3)                  ! day
        obs_8(9)  = jdat(5)                  ! hour
        obs_8(10) = jdat(6)                  ! minute
        obs_8(11) = jdat(7)                  ! second
        obs_8(12) = stko                     ! ascending/descending orbit id
        obs_8(13) = tozone(nXtrk,nscan)      ! best total column ozone solution
        obs_8(14) = aflags(nXtrk,nscan)      ! algorithm flag for best ozone
        obs_8(15) = toqc                     ! total ozone quality code
        obs_8(16) = toqf                     ! total ozone quality flag
        obs_8(17) = acidx(nXtrk,nscan)       ! UV aerosol index
        obs_8(18) = cldfcn(nXtrk,nscan)*100. ! radiative cloud fraction (%)
        obs_8(19) = nXtrk                    ! scan position

        if(nscan.eq.1 .or. nscan.eq.50) then  ! sample scans 1 and 50
           write(*,*) "   ---> At scan ",nscan
           write(*,'("          satellite id           = ",F7.0)') obs_8(1)
           write(*,'("          latitude               = ",F7.2)') obs_8(2)
           write(*,'("          longitude              = ",F7.2)') obs_8(3)
           write(*,'("          viewing zenith angle   = ",F7.2)') obs_8(4)
           write(*,'("          solar zenith angle     = ",F7.2)') obs_8(5)
           write(*,'("          UTC seconds in day     = ",F7.0)') sec(nscan)
           write(*,&
             '("           UTC time of scan      = ",I4,3I2.2,2(":",I2.2))') &
            jdat(1),jdat(2),jdat(3),jdat(5),jdat(6),jdat(7)
           write(*,'("          quality flags          = ",F7.0)') &
            qflags(nXtrk,nscan)
           write(*,'("          asc/des orbit id       = ",F7.0)') obs_8(12)
           write(*,'("          total col ozone sol    = ",F7.2)') obs_8(13)
           write(*,'("          alg flg for best ozone = ",F7.0)') obs_8(14)
           write(*,'("          total ozone qual code  = ",F7.0)') obs_8(15)
           write(*,'("          total ozone qual flag  = ",F7.0)') obs_8(16)
           call upftbv(lunbfr,'TOQF',obs_8(16),31,ibit,nib)
           if(nib.gt.0) print 100, (ibit(ii),ii=1,nib)
 100       format(35X,' (',<nib>I3,')')
           write(*,'("          UV aerosol index       = ",F7.2)') obs_8(17)
           write(*,'("          radiative cld fraction = ",F7.2)') obs_8(18)
           write(*,'("          scan position          = ",F7.0)') obs_8(19)
        endif

! Store obs_8 array into BUFR subset
! ----------------------------------
        call UFBINT(lunbfr,obs_8(1),  9,1,iret, &
                             'SAID CLAT CLON VZAN SOZA YEAR MNTH DAYS HOUR')
        call UFBINT(lunbfr,obs_8(10),10,1,iret, &
                     'MINU SECO STKO OZON AFBO TOQC TOQF ACIDX CLDMNT FOVN')

! Encode subset into BUFR message
! -------------------------------
        call WRITSB(lunbfr)
        iwrite = iwrite + 1

     enddo

  enddo

! All reports have been processed
! Close output BUFR file & write out any incomplete messages
! ----------------------------------------------------------

  call CLOSBF(lunbfr)


! Deallocate allocated arrays
! ---------------------------
  deallocate(tozone)
  deallocate(cldfcn)
  deallocate(lat)
  deallocate(lon)
  deallocate(vza)
  deallocate(sza)
  deallocate(sec)
  deallocate(aflags)
  deallocate(qflags)
  deallocate(acidx)


!!close(lunprt)


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

  call W3TAGE('BUFR_TRANOMI')

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
       write(*,'(" ##### Cannot open groupname ",A,", error = ",I0)') &
        file_attr_swath,error
       call W3TAGE('BUFR_TRANOMI')
       call ERREXIT(3)
    endif


  ! Extract file day (day of month at start of the granule) and set to "day"
  ! ------------------------------------------------------------------------
    datename="GranuleDay"
    call H5AOPEN_NAME_F(fileattrib_gid, trim(datename), attr_id, error) 
    if(error.ne.0) then 
       write(*,'(" ##### Cannot open GranuleDay attribute, error = ",I0)') error
       call W3TAGE('BUFR_TRANOMI')
       call ERREXIT(4)
    endif
    call H5AREAD_F(attr_id,  H5T_NATIVE_INTEGER, date,data_dims,error) 
    if(error.ne.0) then 
       write(*,'(" ##### Cannot read GranuleDay, error = ",I0)') error
       call W3TAGE('BUFR_TRANOMI')
       call ERREXIT(5)
    endif
    call H5ACLOSE_F(attr_id,error)
    if(error.ne.0) then 
       write(*,'(" ----- Cannot close GranuleDay attribute (non-fatal), ", &
        "error = ",I0)') error
    endif
    day=date(1)


  ! Extract file month (month at start of granule) and set to "month"
  ! -----------------------------------------------------------------
    datename="GranuleMonth"
    call H5AOPEN_NAME_F(fileattrib_gid, trim(datename), attr_id, error) 
    if(error.ne.0) then 
       write(*,'(" ##### Cannot open GranuleMonth attribute, error = ",I0)') &
        error
       call W3TAGE('BUFR_TRANOMI')
       call ERREXIT(4)
    endif
    call H5AREAD_F(attr_id,  H5T_NATIVE_INTEGER, date,data_dims,error) 
    if(error.ne.0) then 
       write(*,'(" ##### Cannot read GranuleMonth, error = ",I0)') error
       call W3TAGE('BUFR_TRANOMI')
       call ERREXIT(5)
    endif
    call H5ACLOSE_F(attr_id,error)
    if(error.ne.0) then 
       write(*,'(" ----- Cannot close GranuleMonth attribute (non-fatal), ", &
        "error = ",I0)') error
    endif
    month=date(1)


  ! Extract file year (4-digit year at start of granule) and set to "year"
  ! ----------------------------------------------------------------------
    datename="GranuleYear"
    call H5AOPEN_NAME_F(fileattrib_gid, trim(datename), attr_id, error) 
    if(error.ne.0) then 
       write(*,'(" ##### Cannot open GranuleYear attribute, error = ",I0)') &
        error
       call W3TAGE('BUFR_TRANOMI')
       call ERREXIT(4)
    endif
    call H5AREAD_F(attr_id,  H5T_NATIVE_INTEGER, date,data_dims,error) 
    if(error.ne.0) then 
       write(*,'(" ##### Cannot read GranuleYear, error = ",I0)') error
       call W3TAGE('BUFR_TRANOMI')
       call ERREXIT(5)
    endif
    call H5ACLOSE_F(attr_id,error)
    if(error.ne.0) then 
       write(*,'(" ----- Cannot close GranuleYear attribute (non-fatal), ", &
        "error = ",I0)') error
    endif
    year=date(1)


  ! Close the Group
  ! ---------------
    call H5GCLOSE_F( fileattrib_gid, error)
    if(error.ne.0) then 
       write(*,'(" ##### Cannot close groupname ",A,", error = ",I0)') &
        file_attr_swath,error
       call W3TAGE('BUFR_TRANOMI')
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
       write(*,'(" #### Error returned from an HDF5 interface routine, ", &
        "error = ",I0)') error
       call W3TAGE('BUFR_TRANOMI')
       call ERREXIT(8)
    endif

  end subroutine CHECK_ERROR

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program BUFR_TRANOMI

