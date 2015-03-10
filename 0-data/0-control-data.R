#Started: 10-15-2014
#Last Update: 12-10-2014
#Robert Dinterman, NCSU Economics PhD Student

print(paste0("Started 0-control-data at ", Sys.time()))

rm(list=ls())
tempDir <- tempdir()

# Create a directory for the data
localDir <- "Controls"
if (!file.exists(localDir)) dir.create(localDir)

##### Population Data 2000 to 2010
# http://www.census.gov/popest/data/intercensal/county/county2010.html
url      <- "http://www.census.gov/popest/data/intercensal/county/files/CO-EST00INT-TOT.csv"
file     <- paste(localDir, basename(url), sep = "/")
if (!file.exists(file)) download.file(url, file)
POP      <- read.csv(file)
POP$FIPS <- 1000*POP$STATE+POP$COUNTY

### Population from 2010 to 2012
unzip("Controls/PEP_2012_PEPANNRES.zip",exdir=tempDir)
list.files(tempDir)
POP1        <- read.csv(paste0(tempDir, "/PEP_2012_PEPANNRES_with_ann.csv"))
POP1        <- POP1[,c(2,6:8)]
names(POP1) <- c("FIPS", "POPESTIMATE2010_", "POPESTIMATE2011",
                 "POPESTIMATE2012")

data        <- merge(POP, POP1, by = c("FIPS"), all.x=T)
rm(POP,POP1)

### Quick Facts: 2008 to 2012
url          <- "http://quickfacts.census.gov/qfd/download/DataSet.txt"
file         <- paste0(localDir,'/Control08-12.txt')
if (!file.exists(file)) download.file(url, file)
qfact        <- read.csv(file)
vars         <- c("fips", "RHI225212", "EDU685212", "HSG495212",
                  "INC110212", "LND110210", "POP060210")
qfact        <- qfact[, vars]
names(qfact) <- c("FIPS", "BLACK", "EDUC", "MEDHOMVAL",
                  "MEDHHINC", "AREA", "POPSQ2010")
data         <- merge(data, qfact, by = c("FIPS"), all.x = T)
rm(qfact)

# 2007 Household Income and Poverty
#https://www.census.gov/did/www/saipe/data/statecounty/data/2007.html
url <- "http://www.census.gov/did/www/saipe/downloads/estmod07/est07ALL.xls"
file         <- paste0(localDir,'/poverty07.xls')
if (!file.exists(file)) download.file(url, file)
# Need to do something to change .xls to .csv
pov          <- read.csv("Controls/est07ALL.csv")
pov$FIPS     <- 1000*pov$State.FIPS + pov$County.FIPS
pov          <- pov[,c("FIPS", "Poverty.Estimate.All.Ages",
                       "Poverty.Percent.All.Ages",
                       "Poverty.Estimate.Under.Age.18",
                       "Poverty.Percent.Under.Age.18",
                       "Poverty.Estimate.Ages.5.17",
                       "Poverty.Percent.Ages.5.17",
                       "Median.Household.Income",
                       "Poverty.Estimate.Ages.0.4",
                       "Poverty.Percent.Ages.0.4")]
data         <- merge(data, pov, by = "FIPS", all.x = T)
rm(pov)

### Vacation Homes
vaca         <- read.csv("http://www4.ncsu.edu/~rdinter/docs/acs2009.csv")
vaca$share   <- vaca$Vacation / vaca$Total
vaca         <- vaca[, -c(2:6)]
data         <- merge(data, vaca, by = "FIPS", all.x = T)
rm(vaca)

### Natural Amenities
nat          <- read.csv("http://www4.ncsu.edu/~rdinter/docs/natamen.csv")
nat$climate  <- nat$JAN.TEMP...Z + nat$JAN.SUN...Z + nat$JUL.TEMP...Z +
  nat$JUL.HUM...Z
nat$topo     <- nat$TOPOG...Z
nat          <- nat[, c("FIPS", "Scale", "climate", "topo")]
data         <- merge(data, nat, by = "FIPS", all.x = T)
rm(nat)

### Rural Urban Continuum Codes
url          <- "http://www4.ncsu.edu/~rdinter/docs/ruralurbancodes2003.csv"
ruc          <- read.csv(url)
ruc          <- ruc[, c(1,5)]
names(ruc)   <- c("FIPS", "ruc")
data         <- merge(data, ruc, by = "FIPS", all.x = T)
rm(ruc)

### Highway
hwy          <- read.csv("http://www4.ncsu.edu/~rdinter/docs/road2005.txt")
hwy          <- hwy[,c(2,4,5,9,11)]
names(hwy)   <- c("FIPS",  "HWYCOUNT", "HWYAREA", "HWYMEAN", "HWYSUM")
data         <- merge(data, hwy, by = "FIPS", all.x = T)
rm(hwy)

write.csv(data,"Controls/controls.csv")

print(paste0("Finished 0-control-data at ", Sys.time()))
