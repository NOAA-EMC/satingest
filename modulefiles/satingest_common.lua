help([[
Load common modules to build satingest on all machines
]])

hdf5_ver=os.getenv("hdf5_ver") or "default"
bacio_ver=os.getenv("bacio_ver") or "default"
w3emc_ver=os.getenv("w3emc_ver") or "default"
bufr_ver=os.getenv("bufr_ver") or "default"

load(pathJoin("hdf5", hdf5_ver))
load(pathJoin("bacio", bacio_ver))
load(pathJoin("w3emc", w3emc_ver))
load(pathJoin("bufr", bufr_ver))
