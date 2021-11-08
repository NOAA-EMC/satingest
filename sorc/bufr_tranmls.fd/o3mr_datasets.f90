module o3mr_datasets

!$$$  MODULE DOCUMENTATION BLOCK
!
! MODULE:    O3MR_DATASETS
!   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2010-06-19
!
! ABSTRACT: Provides the datatypes for reading MLS O3MR data from HDF-EOS5 file.
!
! Program history log:
! 2008-12-31  Greg Krasowski - Original author (adapted from decoder written
!             by Trevor Beck, NESDIS Sensor Physics Branch)
! 2010-06-16  Haixia Liu - revise it for MLS data set
!
! USAGE:    CALL O3MR_DATASETS
!
!   OUTPUT FILES:
!     UNIT 06  - PRINTOUT
!     UNIT 68  - RADCOR INFORMATION FILE
!
! REMARKS:
!
! Attributes:
!   Language: FORTRAN 90 (free format)
!   Machine:  NCEP WCOSS
!
!$$$

  use hdf5

!
! ***MUST DEFINE ALL VARIABLES***
  implicit none

! Set parameters
  integer, parameter :: MAXRANK=4
  TYPE, PUBLIC :: DSh5_T
     CHARACTER (LEN = 80) :: name
     INTEGER (HID_T)  :: dataset_id 
     INTEGER (HID_T)  :: datatype  ! datatype determines class, order, size, 
                                   !  and vise versa 
     INTEGER          :: class
     INTEGER (SIZE_T) :: size
     INTEGER (HID_T)  :: dataspace ! dataspace determines rank, dims 
     INTEGER          :: rank
     INTEGER(HSIZE_T), DIMENSION(MAXRANK) :: dims
  END TYPE DSh5_T

  integer :: nATscans

  ! Information for reading datasets from the hdf5 file, ds=dataset
  type(DSh5_T) :: ds_o3mr   =DSH5_T("O3",    -1, -1,-1,4,-1,2,(/-1,-1,-1,-1/) )
  type(DSh5_T) :: ds_precisn=DSH5_T("O3Precision",    -1, -1,-1,4,-1,2,(/-1,-1,-1,-1/) )
  type(DSh5_T) :: ds_qualt  =DSH5_T("Quality",      -1, -1,-1,4,-1,1,(/-1,-1,-1,-1/) )
  type(DSh5_T) :: ds_stat   =DSH5_T("Status",      -1, -1,-1,4,-1,1,(/-1,-1,-1,-1/) )
  type(DSh5_T) :: ds_conv   =DSH5_T("Convergence",      -1, -1,-1,4,-1,1,(/-1,-1,-1,-1/) )
  type(DSh5_T) :: ds_lat    =DSH5_T("Latitude",          -1, -1,-1,4,-1,1,(/-1,-1,-1,-1/) )
  type(DSh5_T) :: ds_lon    =DSH5_T("Longitude",         -1, -1,-1,4,-1,1,(/-1,-1,-1,-1/) )
  type(DSh5_T) :: ds_sza    =DSH5_T("SolarZenithAngle",  -1, -1,-1,4,-1,1,(/-1,-1,-1,-1/) )
  type(DSh5_T) :: ds_time   =DSH5_T("Time",      -1, -1,-1,4,-1,1,(/-1,-1,-1,-1/) )
  type(DSh5_T) :: ds_press  =DSH5_T("Pressure",    -1, -1,-1,4,-1,1,(/-1,-1,-1,-1/) )


  ! Arrays to hold the datasets read from hdf5.
  real(kind=4),   allocatable, dimension(:,:) :: o3mr
  real(kind=4),   allocatable, dimension(:,:) :: precisn
  real(kind=4),   allocatable, dimension(:) :: qualt
  real(kind=4),   allocatable, dimension(:) :: conv
  real(kind=4),   allocatable, dimension(:) :: lat
  real(kind=4),   allocatable, dimension(:) :: lon
  real(kind=4),   allocatable, dimension(:) :: sza
  real(kind=8),   allocatable, dimension(:)   :: time
  real(kind=4),   allocatable, dimension(:)   :: press
  real(kind=4),   allocatable, dimension(:)   :: stat


end module o3mr_datasets

