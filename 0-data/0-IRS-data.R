#Started 10-15-2014
#Updated 10-15-2014
#Robert Dinterman, NCSU Economics PhD Student

#####
# Data Compiler
##
print(paste0("Started 0-IRS-data at ", Sys.time()))

library(gdata)
library(plyr)
library(reshape2)

# Create a directory for the data
localDir <- "IRS"
tempDir  <- tempdir()
if (!file.exists(localDir)) dir.create(localDir)

#####
# IRS Migration Data for 1990 to 2004
#http://www.irs.gov/uac/SOI-Tax-Stats-Migration-Data

url    <- "http://www.irs.gov/file_source/pub/irs-soi/"
years  <- seq(1992, 2004) #the 90 to 92 data are in text files
files  <- matrix(NA, nrow = length(years) - 1)
for (i in 1:length(files)){
  temp <- paste0(url, years[i], "to", years[i + 1], "countymigration.zip")
  file <- paste(tempDir, basename(temp), sep = "/")
  files[i,] <- file
  if (!file.exists(file)) download.file(temp, file)
}

tempDirr   <- paste0(tempDir, "/temp")
allindata  <- data.frame()
alloutdata <- data.frame()
for (i in files){
  if (!file.exists(tempDirr)) dir.create(tempDirr)
  unzip(i, exdir = tempDirr)
  j5     <- list.files(paste0(tempDirr))
  j6     <- list.files(paste0(tempDirr, "/", j5))
  j5i    <- list.files(paste0(tempDirr, "/", j5, "/", j6[1]))
  j5o    <- list.files(paste0(tempDirr, "/", j5, "/", j6[2]))
  namesi <- c("State_Code_Dest", "County_Code_Dest", "State_Code_Origin",
              "County_Code_Origin", "State_Abbrv", "County_Name", "Return_Num",
              "Exmpt_Num", "Aggr_AGI")
  indata <- data.frame()
  for (j in j5i){
    file   <- paste0(tempDirr, "/", j5[1], "/", j6[1], "/", j)
    data   <- read.xls(file)
    data   <- data[c(6:nrow(data)),c(1:9)]
    
    data[,c(1:4,7:9)] <- lapply(data[,c(1:4,7:9)],
                                function(x){as.numeric(gsub(",", "", x))})
    data[,c(5:6)]     <- lapply(data[,c(5:6)], function(x){as.character(x)})
    names(data)       <- namesi
    data$ofips = data$State_Code_Origin*1000 + data$County_Code_Origin
    data$dfips = data$State_Code_Dest*1000   + data$County_Code_Dest
    indata     = rbind(indata, data)
    
    print(paste0("Finished ", basename(j), " at ", Sys.time()))
    
  }
  write.csv(indata, paste0(localDir, "/", basename(i),"i.csv"))
  
  
  nameso <- c("State_Code_Origin", "County_Code_Origin", "State_Code_Dest",
              "County_Code_Dest", "State_Abbrv", "County_Name", "Return_Num",
              "Exmpt_Num", "Aggr_AGI")
  outdata <- data.frame()
  for (j in j5o){
    file   <- paste0(tempDirr, "/", j5[1], "/", j6[2], "/", j)
    data   <- read.xls(file)
    data   <- data[c(6:nrow(data)),c(1:9)]
    
    data[,c(1:4,7:9)] <- lapply(data[,c(1:4,7:9)],
                                function(x){as.numeric(gsub(",", "", x))})
    data[,c(5:6)]     <- lapply(data[,c(5:6)], function(x){as.character(x)})
    names(data)       <- nameso
    data$ofips  = data$State_Code_Origin*1000 + data$County_Code_Origin
    data$dfips  = data$State_Code_Dest*1000   + data$County_Code_Dest
    outdata     = rbind(outdata, data)
    
    print(paste0("Finished ", basename(j), " at ", Sys.time()))
  }  
  write.csv(outdata, paste0(localDir, "/", basename(i),"o.csv"))
  
  unlink(tempDirr, recursive = T)
  allindata  <- rbind(allindata, indata)
  alloutdata <- rbind(alloutdata, outdata)
  print(paste0("Finished ", basename(i), " at ", Sys.time()))
}
write.csv(allindata, paste0(localDir, "/inflows9204.csv"))
write.csv(alloutdata, paste0(localDir, "/outflows9204.csv"))


# Data from 2004 to 2010
inflows  <- c("countyinflow0405.csv", "countyinflow0506.csv",
              "countyinflow0607.csv", "countyinflow0708.csv",
              "countyinflow0809.csv", "countyinflow0910.csv",
              "countyinflow1011.csv")
indata   <- data.frame()
for (i in inflows){
  file       = paste0(localDir, "/", i)
  if (!file.exists(file)) (download.file(paste0(url, i), file))
  data       = read.csv(file)
  data$year  = 2000 + as.numeric(substr(i, nchar(i) - 5, nchar(i) - 4))
  data$ofips = data$State_Code_Origin*1000 + data$County_Code_Origin
  data$dfips = data$State_Code_Dest*1000   + data$County_Code_Dest
  indata     = rbind(indata, data)
  print(paste0("Finished ", basename(i), " at ", Sys.time()))
  
}
write.csv(indata, paste0(localDir, "/inflows0410.csv"))

outflows <- c("countyoutflow0405.csv", "countyoutflow0506.csv",
              "countyoutflow0607.csv", "countyoutflow0708.csv",
              "countyoutflow0809.csv", "countyoutflow0910.csv",
              "countyoutflow1011.csv")
outdata  <- data.frame()
for (i in outflows){
  file       = paste0(localDir, "/", i)
  if (!file.exists(file)) (download.file(paste0(url, i),file))
  data       = read.csv(file)
  data$year  = 2000 + as.numeric(substr(i, nchar(i) - 5, nchar(i) - 4))
  data$ofips = data$State_Code_Origin*1000 + data$County_Code_Origin
  data$dfips = data$State_Code_Dest*1000   + data$County_Code_Dest
  outdata    = rbind(outdata, data)
  print(paste0("Finished ", basename(i), " at ", Sys.time()))
  
}
write.csv(outdata, paste0(localDir, "/outflows0410.csv"))

#Net Migration
#
print(paste0("Finished 0-IRS-data at ", Sys.time()))
