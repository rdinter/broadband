#Robert Dinterman, NCSU Economics PhD Student

library(reshape2)

# Update bls website: http://www.bls.gov/bls/ftp_migration_crosswalk.htm

#######
# Local Area Unemployment 1990 to 2014
#ftp://ftp.bls.gov/pub/time.series/la/
year1    <- 1990
year2    <- 2014
localDir <- "0-data/Local Area Unemployment"
if (!file.exists(localDir)) dir.create(localDir)

#url       <- "ftp://ftp.bls.gov/pub/time.series/la/"
url       <- "http://download.bls.gov/pub/time.series/la/"

download.file(paste0(url,"la.txt"), paste0(localDir, "/la.txt"))
download.file(paste0(url,"/la.area"), paste0(localDir, "/la.area"))
download.file(paste0(url,"/la.area_type"), paste0(localDir, "/la.area_type"))
# download.file("http://www.bls.gov/lau/crosswalk.xlsx",
#               paste0(localDir, "/crosswalk.xlsx"))

states <- c("10.Arkansas", "11.California", "12.Colorado", "13.Connecticut",
            "14.Delaware", "15.DC", "16.Florida", "17.Georgia", "18.Hawaii",
            "19.Idaho", "20.Illinois", "21.Indiana", "22.Iowa", "23.Kansas",
            "24.Kentucky", "25.Louisiana", "26.Maine", "27.Maryland",
            "28.Massachusetts", "29.Michigan", "30.Minnesota",
            "31.Mississippi", "32.Missouri", "33.Montana", "34.Nebraska",
            "35.Nevada", "36.NewHampshire", "37.NewJersey", "38.NewMexico",
            "39.NewYork", "40.NorthCarolina", "41.NorthDakota", "42.Ohio",
            "43.Oklahoma", "44.Oregon", "45.Pennsylvania", "47.RhodeIsland",
            "48.SouthCarolina", "49.SouthDakota", "50.Tennessee", "51.Texas",
            "52.Utah", "53.Vermont", "54.Virginia", "56.Washington",
            "57.WestVirginia", "58.Wisconsin", "59.Wyoming", "7.Alabama",
            "8.Alaska", "9.Arizona")
filenames <- paste0(url, "la.data.", states)

for (i in filenames){
  file <- paste(localDir, basename(i), sep = "/")
  if (!file.exists(file)) download.file(i, file)
  print(paste0("Downloaded ", basename(i), " at ", Sys.time()))
}
bls        <- read.delim("http://www4.ncsu.edu/~rdinter/docs/bls-to-fips.txt")
bls$fips   <- bls$ST_FIPS*1000 + bls$CTY_FIPS

cross      <- read.csv(paste0(localDir, "/la.area"), skip = 1, header = F)
cross$lev  <- substr(cross$V1, 1, 1)  #Level of series
cross      <- subset(cross, lev == "F")
cross$id   <- substr(cross$V1, 2, 17) #Series ID
cross$id   <- gsub("\t", "", cross$id)
cross$FIPS <- as.numeric(substr(cross$id, 4, 8))
cross$LAUS <- substr(cross$id, 2, 8)

unemp <- data.frame()
for (i in filenames){
  file <- paste(localDir, basename(i), sep = "/")
  data <- read.delim(file, header = T)
  #       colClasses=c("character","integer","character","numeric","character"))
  data <- subset(data, year >= year1 & year <= year2 & period == "M13")
  data <- subset(data, substr(data$series_id, 4, 18) %in% cross$id
                 & (substr(data$series_id, 19, 20) == "04"
                 | substr(data$series_id, 19, 20) == "05"))
  data$LAUS  <- substr(data$series_id, 4, 11)
  data$var   <- factor(substr(data$series_id, 19, 20),
                       labels = c("Unemp", "Emp"))
  data$value <- as.numeric(as.character(data$value))
  data       <- data[, c("LAUS", "year", "value", "var")]
  unemp      <- rbind(unemp, data)
  print(paste0("Finished ", basename(i), " at ", Sys.time()))
}

unemp        <- unemp[!is.na(unemp$value),]
unemp$FIPS   <- as.numeric(substr(unemp$LAUS, 3, 7))

unemp        <- dcast(unemp, FIPS + year ~ var, value.var = "value",
                     fun.aggregate = mean)

write.csv(unemp, paste0(localDir, "/LAUnemp.csv"), row.names = F)
