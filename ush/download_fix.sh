#!/bin/bash

####  UNIX Script Documentation Block
#
# Script name:   download_fix.sh    Script for downloading binary fix files
#
# Author:        R. Mahajan         Org: NCEP/EMC       Date: 2021-10-21
#
#
# Abstract: This script downloads the binary fix files that used to be a part of the
# VLab repository.
# The downloaded files are:
#     bufr_hirsrtcf_ibm.dat
#     bufr_lowtopog.dat
#     nesdis.lstags_transsmi
#
# These files are currently hosted on the EMC FTP server at:
#    https://ftp.emc.ncep.noaa.gov/static_files/public/obsproc/satingest-fix-HASH.tgz
#
# HASH is the truncated 6 character sha256sum hash of the tar file and can be obtained as:
# $> sha256sum satingest-fix.tgz
#      867fd8c2ca1f87d5207fab0e1f9ea5ad8e7154aecc2df58dcde483c41dbaef5c
# The data can be downloaded manually and into the cloned
# repository by executing this script from the root of the cloned directory
# e.g.
# $> pwd
#     /path/to/satingest
# $> ls -1
#    fix
#    jobs
#    modulefiles
#    scripts
#    sorc
#    ush
#    versions
# $> ./ush/download_fix.sh
#
# Usage: ./ush/download_fix.sh [yes|no]
#    Script parameters:
#      yes|no - Force download of fix files to overwrite previous download [Default: NO]
#
####

set -eu

# Force option to overwrite fix binary files from a previous download
force=${1:-"NO"}

URL="https://ftp.emc.ncep.noaa.gov/static_files/public/obsproc"
SHA="867fd8c2ca1f87d5207fab0e1f9ea5ad8e7154aecc2df58dcde483c41dbaef5c"
SHORTSHA=$(echo $SHA | cut -c1-6)
TAR="satingest-fix-${SHORTSHA}.tgz" # poor-man's version control

# fix files to download (contents of the $TAR should match)
fix_files=( \
  bufr_hirsrtcf_ibm.dat \
  bufr_lowtopog.dat \
  nesdis.lstags_transsmi \
)

# Toggle download if any of the fix files are not present
download=false
for file in ${fix_files[@]}; do
  if [[ ! -f fix/$file ]]; then
    echo -e "\nFix file $file not found in cloned repository!"
    download=true
    break
  fi
done

# Force download if desired
if [[ "$force" =~ [yYtY] ]]; then
  echo -e "\nUsing force to fresh download of fix files"
  download=true
fi

if $download; then
  echo -e "\nDownloading $TAR from $URL"
  rm -f $TAR
  wget -q $URL/$TAR
  SHA256=$(sha256sum $TAR 2>/dev/null | awk '{print $1}')
  if [[ ! "$SHA256" == "$SHA" ]]; then
    echo -e "\nIncorrect checksum, ABORT!"
    exit 1
  fi
  tar xzf $TAR
  rm -f $TAR
else
  echo -e "\nFix files present in the cloned repository. Nothing to download!\n"
fi
