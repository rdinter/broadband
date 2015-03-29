#Robert Dinterman, NCSU Economics PhD Student

# Downloading Quarterly Census of Employment and Wages Program
# ReadMe: http://www.bls.gov/cew/doc/layouts/csv_annual_layout.htm

# http://www.bls.gov/cew/datatoc.htm
print(paste0("Started 0-QCEW at ", Sys.time()))

library(plyr)
# library(readr)
# library(reshape2)

# Create a directory for the data
localDir <- "0-data/QCEW"
if (!file.exists(localDir)) dir.create(localDir)

tempDir  <- tempdir()
unlink(tempDir, recursive = T)

##### QCEW Data, as of 3-23-2015 the 2014 Data has not been released.
# http://www.bls.gov/cew/data/files/1990/csv/1990_annual_singlefile.zip
years  <- seq(1990,2013)
url    <- "http://www.bls.gov/cew/data/files/"
files  <- matrix(NA, nrow = length(years))
for (i in years){
  temp <- paste0(url, i, "/csv/", i, "_annual_singlefile.zip")
  file <- paste(localDir, basename(temp), sep = "/")
  files[i - years[1] + 1,] <- file
  if (!file.exists(file)) download.file(temp, file)
}

lapply(files, function(x) unzip(x, exdir = tempDir))
j5    <- list.files(tempDir, pattern = "*.csv", ignore.case = T)
j5    <- paste(tempDir, j5, sep = "/")
datal <- lapply(j5, read.csv)
Sys.time()

system.time(data.fill <- rbind.fill(datal))
data.fill$area_fips <- as.character(data.fill$area_fips)

naics    <- c("10", "11", "21", "22", "23", "31-33", "42", "44-45",
              "48-49", "51",  "52", "53", "54", "55", "56", "61",
              "62", "71", "72", "81", "92")

annual <- subset(data.fill, subset = (!grepl("[a-zA-Z]+", data.fill$area_fips)) &
                 (own_code %in% c(0, 5)) & (industry_code %in% naics))
rm(data.fill)
annual$FIPS <- as.numeric(annual$area_fips)

write.csv(annual, paste0(localDir, "/QCEWAnnual_2.csv"), row.names = F)
save(annual, file = paste0(localDir, "/QCEWAnnual_2.RData"))
