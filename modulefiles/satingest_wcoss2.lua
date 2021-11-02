help([[
Load environment to build satingest on WCOSS2
]])

load("envvar")
load("PrgEnv-intel")
load("intel/19.1.3.304")
load("cmake/3.20.2")

-- Load common modules for this package
load("satingest_common")

whatis("Description: satingest build environment")
