help([[
Load environment to build satingest on WCOSS2
]])

load("envvar")
load("PrgEnv-intel")
load(pathJoin("intel/19.1.3.304"))

-- Load common modules for this package
load("satingest_common")

whatis("Description: satingest build environment")
