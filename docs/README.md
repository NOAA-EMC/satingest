# Satellite Data Ingest

Description of the repository and its contents, use and what have you ...

## Authors

NCEP/EMC and NCEP/NCO developers.

Code manager: Satingest Crew (ObsProc team)

## Supported Compilers

- Intel Fortran Compiler

## Pre-requisites
- [HDF5](https://www.hdfgroup.org/solutions/hdf5/) >= v1.10.6
- [NCEPLIBS-bacio](https://github.com/NOAA-EMC/NCEPLIBS-bacio) >= v2.4.1
- [NCEPLIBS-w3emc](https://github.com/NOAA-EMC/NCEPLIBS-w3emc) >= v2.9.1
- [NCEPLIBS-bufr](https://github.com/NOAA-EMC/NCEPLIBS-bufr) >= 11.5.0

## How to Build and Install

A shell script to build and install on NOAA platforms is provided [build.sh](./ush/build.sh).  Its usage on WCOSS2 is:
```bash
INSTALL_PREFIX=<installation_prefix> ./ush/build.sh
```

## More info on Build and Install

The utilities under this repository use CMake for configuration.

```bash
mkdir build && cd build
cmake -DCMAKE_INSTALL_PREFIX=<installation_prefix> /path/to/satingest
make -j4
make install
```

`<installation_prefix>` is the path to install the package and may be full or relative pathname.

Installation of the utilities will be under `<installation_prefix>`.

### Further notes for WCOSS installation

Prior to executing the above "Build and Install" instructions, source and load modules for WCOSS2:
```bash
source versions/build.ver
module use $PWD/modulefiles
module load satingest_wcoss2
```

NCEP/NCO requires executables be placed under `exec/`.  Provide additional argument during CMake configuration as follows
```bash
-DCMAKE_INSTALL_BINDIR=exec
```

## Disclaimer

The United States Department of Commerce (DOC) GitHub project code is
provided on an "as is" basis and the user assumes responsibility for
its use. DOC has relinquished control of the information and no longer
has responsibility to protect the integrity, confidentiality, or
availability of the information. Any claims against the Department of
Commerce stemming from the use of its GitHub project will be governed
by all applicable Federal law. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of
Commerce. The Department of Commerce seal and logo, or the seal and
logo of a DOC bureau, shall not be used in any manner to imply
endorsement of any commercial product or activity by DOC or the United
States Government.
