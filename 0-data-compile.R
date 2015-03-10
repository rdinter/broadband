#Started: 10-13-2014
#Last Update: 12-5-2014
#Robert Dinterman, NCSU Economics PhD Student


# Data Compiling ----------------------------------------------------------


# Broadband Data ----------------------------------------------------------

source("0-broadband-data_county.R")
source("0-broadband-data_tract.R")

# Employment Data ---------------------------------------------------------

source("0-QCEW.R")
source("0-LAU-data.R")

# Population Data ---------------------------------------------------------

source("0-IRS-data.R") #needs updating of .xls files from 92-04

# Control Data ------------------------------------------------------------

source("0-shapefile.R")
source("0-terrain.R")
source("0-control-data.R")
source("0-permit-data.R")
