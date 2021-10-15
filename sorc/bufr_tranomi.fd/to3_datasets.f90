module to3_datasets

!$$$  MODULE DOCUMENTATION BLOCK
!
! MODULE:    TO3_DATASETS
!   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2009-01-19
!
! ABSTRACT: Provides the datatypes for reading OMTO3 data from HDF-EOS5 file.
!
! Program history log:
! 2008-12-31  Greg Krasowski - Original author (adapted from decoder written
!             by Trevor Beck, NESDIS Sensor Physics Branch)
! 2009-01-16  Dennis Keyser  - Prepared for implementation into NCEP production
!
! USAGE:    CALL TO3_DATASETS
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
  type(DSh5_T) :: ds_toz    =DSH5_T("ColumnAmountO3",    -1, -1,-1,4,-1,2,(/-1,-1,-1,-1/) )
  type(DSh5_T) :: ds_lat    =DSH5_T("Latitude",          -1, -1,-1,4,-1,2,(/-1,-1,-1,-1/) )
  type(DSh5_T) :: ds_lon    =DSH5_T("Longitude",         -1, -1,-1,4,-1,2,(/-1,-1,-1,-1/) )
  type(DSh5_T) :: ds_cld    =DSH5_T("RadiativeCloudFraction",-1, -1,-1,4,-1,2,(/-1,-1,-1,-1/) )
  type(DSh5_T) :: ds_sza    =DSH5_T("SolarZenithAngle",  -1, -1,-1,4,-1,2,(/-1,-1,-1,-1/) )
  type(DSh5_T) :: ds_vza    =DSH5_T("ViewingZenithAngle",-1, -1,-1,4,-1,2,(/-1,-1,-1,-1/) )
  type(DSh5_T) :: ds_sec    =DSH5_T("SecondsInDay",      -1, -1,-1,4,-1,1,(/-1,-1,-1,-1/) )
  type(DSh5_T) :: ds_algflag=DSH5_T("AlgorithmFlags",    -1, -1,-1,4,-1,2,(/-1,-1,-1,-1/) )
  type(DSh5_T) :: ds_qflag  =DSH5_T("QualityFlags",      -1, -1,-1,4,-1,2,(/-1,-1,-1,-1/) )
  type(DSh5_T) :: ds_acidx  =DSH5_T("UVAerosolIndex",    -1, -1,-1,4,-1,2,(/-1,-1,-1,-1/) )


  ! Arrays to hold the datasets read from hdf5.
  real(kind=4),   allocatable, dimension(:,:) :: tozone
  real(kind=4),   allocatable, dimension(:,:) :: cldfcn
  real(kind=4),   allocatable, dimension(:,:) :: lat
  real(kind=4),   allocatable, dimension(:,:) :: lon
  real(kind=4),   allocatable, dimension(:,:) :: sza
  real(kind=4),   allocatable, dimension(:,:) :: vza
  real(kind=4),   allocatable, dimension(:)   :: sec
  real(kind=4),   allocatable, dimension(:,:) :: acidx
  integer(kind=4),allocatable, dimension(:,:) :: aflags
  integer(kind=4),allocatable, dimension(:,:) :: qflags


end module to3_datasets

