#Started: 10-14-2014
#Last Update: 10-14-2014
#Robert Dinterman, NCSU Economics PhD Student

# Downloading NC FCC Broadband Data 2008-13 from:
#   http://transition.fcc.gov/wcb/iatd/comp.html

print(paste0("Started 0-broadband-data_tract at ", Sys.time()))

# Create a directory for the data
localDir <- "FCC Tract Data"
tempDir  <- tempdir()
if (!file.exists(localDir)) dir.create(localDir)
##### FCC County Data http://www.fcc.gov/Bureaus/Common_Carrier/Reports/FCC-State_Link/IAD/
url   = "http://www.fcc.gov/Bureaus/Common_Carrier/Reports/FCC-State_Link/IAD/"
files = c("csv_dec_2008_tract.zip", "csv_tractdata_june_2009.zip",
          "csv_tractdata_dec_2009.zip", "csv_tractdata_june_2010.zip",
          "csv_tractdata_dec_2010.zip", "csv_tractdata_june_2011.zip",
          "csv_tractdata_dec_2011.zip", "csv_tractdata_june_2012.zip",
          "csv_tractdata_dec_2012.zip", "csv_tractdata_june_2013.zip")

for (i in files){
  temp <- paste0(url, i)
  file <- paste(localDir, basename(temp), sep = "/")
  if (!file.exists(file)) download.file(temp, file)
  zipf <- paste(tempDir, basename(temp), sep = "/")
  unzip(file, exdir = tempDir)
}

files <- list.files(tempDir, pattern = "*.csv")
data  <- data.frame()
for (i in files) {
  file     <- paste(tempDir, i, sep = "/")
  inp      <- read.csv(file)
  tmp      <- substr(i, nchar(i) - 11, nchar(i) - 4)
  year     <- as.Date(paste0(1,tmp), format = "%d%b_%Y")
  inp$year <- year
  cname    <- name <- sub(".csv", "", i)
  cat("Read:", i, "\trows: ", nrow(inp), " cols: ", ncol(inp), 
      "\n")
  assign(name, inp)
  
  if (i == "hs_tractdata_v1_jun_2013.csv") names(inp) = names(data) #recode var
  
  if (ncol(inp)==14)  data = rbind(data,inp)
}
#Recode variables to account for 2008 data
names(hs_mapdata_dec_2008) = names(data)[-12]
hs_mapdata_dec_2008$total_residential_prov_nbp = NA
hs_mapdata_dec_2008        = hs_mapdata_dec_2008[, names(data)]
data                       = rbind(data,hs_mapdata_dec_2008)
write.csv(data, paste0(localDir,"/FCC_tract_08-13.csv"))

print(paste0("Finished 0-broadband-data_tract at ", Sys.time()))

#http://www2.ntia.doc.gov/broadband-data