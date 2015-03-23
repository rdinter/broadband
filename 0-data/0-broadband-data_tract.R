#Robert Dinterman, NCSU Economics PhD Student

# Downloading NC FCC Broadband Data 2008-13 from:
#   http://transition.fcc.gov/wcb/iatd/comp.html

print(paste0("Started 0-broadband-data_tract at ", Sys.time()))

library(plyr)
# library(readr)

# Create a directory for the data
localDir <- "0-data/FCC Tract Data"
if (!file.exists(localDir)) dir.create(localDir)

tempDir  <- tempdir()
unlink(tempDir, recursive = T)

##### FCC County Data http://www.fcc.gov/Bureaus/Common_Carrier/Reports/FCC-State_Link/IAD/
url   = "http://www.fcc.gov/Bureaus/Common_Carrier/Reports/FCC-State_Link/IAD/"
files = c("csv_dec_2008_tract.zip", "csv_tractdata_june_2009.zip",
          "csv_tractdata_dec_2009.zip", "csv_tractdata_june_2010.zip",
          "csv_tractdata_dec_2010.zip", "csv_tractdata_june_2011.zip",
          "csv_tractdata_dec_2011.zip", "csv_tractdata_june_2012.zip",
          "csv_tractdata_dec_2012.zip", "csv_tractdata_june_2013.zip",
          "csv_tractdata_dec_2013.zip")

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
  inp      <- read.csv(file, stringsAsFactors = F)
  tmp      <- substr(i, nchar(i) - 11, nchar(i) - 4)
  inp$year <- as.Date(paste0(1,tmp), format = "%d%b_%Y")
  cname    <- name <- sub(".csv", "", i)
  
  cat("Read:", i, "\trows: ", nrow(inp), " cols: ", ncol(inp), 
      "\n")
  
  data = rbind.fill(data, inp)
  rm(inp)
}
apply(data, 2, function(x) sum(is.na(x)))

miss <- is.na(data$rfc_per_1000_hhs)
data$rfc_per_1000_hhs[miss] <- data$rfhsc_per_1000_hhs[miss]

miss <- is.na(data$rfc_per_1000_hhs_btop)
data$rfc_per_1000_hhs_btop[miss] <- data$rfhsc_per_1000_hhs_btop[miss]
miss <- is.na(data$rfc_per_1000_hhs_btop)
data$rfc_per_1000_hhs_btop[miss] <- data$rfc_per_1000_hhs_nbp[miss]

data <- data[, -c(15, 16, 17)]

write.csv(data, paste0(localDir,"/FCC_tract_08-13.csv"), row.names = F)

print(paste0("Finished 0-broadband-data_tract at ", Sys.time()))

#http://www2.ntia.doc.gov/broadband-data