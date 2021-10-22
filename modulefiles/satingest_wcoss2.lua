help([[
Load environment to build satingest on WCOSS2
]])

intel_ver=os.getenv("intel_ver") or "default"

load("envvar")
load("PrgEnv-intel")
load(pathJoin("intel/19.1.3.304"))

-- Load common modules for this package
load("satingest_common")

whatis("Description: satingest build environment")
