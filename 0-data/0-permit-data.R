#Started: 11-19-2014
#Last Update: 11-19-2014
#Robert Dinterman, NCSU Economics PhD Student

print(paste0("Started 0-permit-data at ", Sys.time()))

rm(list=ls())
tempDir <- tempdir()

# Create a directory for the data
localDir <- "Permits"
files    <- list.files(paste0(localDir,"/Raw"), pattern = ".*.txt$") #files ending in .txt

names    <- c("year", "STFIPS",	"CFIPS", "Region", "Division", "County",
              "Bldgs_1-unit",	"Units_1-unit",	"Value_1-unit",	"Bldgs_2-units",
              "Units_2-units", "Value_2-units",	"Bldgs3-4_units",	
              "Units3-4_units",	"Value3-4_units",	"Bldgs5_units",	
              "Units_5_units", "Value_5_units",	"Bldgs_1-unit_rep",
              "Units_1-unit_rep", "Value_1-unit_rep",	"Bldgs_2-units_rep",
              "Units_2-units_rep", "Value_2-units_rep",	"Bldgs_3-4_units_rep",
              "Units_3-4_units_rep", "Value_3-4_units_rep",	"Bldgs_5units_rep",
              "Units_5units_rep",	"Value_5units_rep")
permits <- data.frame()
for (i in files){
  j5 <- read.csv(paste0(localDir,"/Raw/",i), header = F, skip = 3)
  names(j5) <- names
  j5$FIPS   <- 1000*j5$STFIPS + j5$CFIPS
  
  permits     <- rbind(permits, j5)
  
  print(paste0("Finished ", basename(i), " at ", Sys.time()))
}

permits$year[permits$year == 9099] <- 1990
permits$year[permits$year == 9199] <- 1991
permits$year[permits$year == 9299] <- 1992
permits$year[permits$year == 9399] <- 1993
permits$year[permits$year == 9499] <- 1994
permits$year[permits$year == 9599] <- 1995
permits$year[permits$year == 9699] <- 1996
permits$year[permits$year == 9799] <- 1997
permits$year[permits$year == 9899] <- 1998

write.csv(permits, file = paste0(localDir, "/permits.csv"))
save(permits, file = paste0(localDir, "/permits.RData"))

print(paste0("Finished 0-permit-data at ", Sys.time()))