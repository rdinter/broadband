#Robert Dinterman, NCSU Economics PhD Student

sessionInfo()

# Data Compiling ----------------------------------------------------------

# Broadband Data ----------------------------------------------------------

source("0-data/0-broadband-data_county.R")
source("0-data/0-broadband-data_tract.R")

# Employment Data ---------------------------------------------------------

source("0-data/0-QCEW.R")
source("0-data/0-LAU-data.R")

# Population Data ---------------------------------------------------------

source("0-data/0-IRS-data.R")

# Control Data ------------------------------------------------------------

source("0-data/0-shapefile.R")
source("0-data/0-terrain.R")
source("0-data/0-control-data.R")
source("0-data/0-permit-data.R")