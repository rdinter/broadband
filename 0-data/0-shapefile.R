#Started: 10-15-2014
#Last Update: 10-18-2014
#Robert Dinterman, NCSU Economics PhD Student

print(paste0("Started 0-shapefile at ", Sys.time()))

# County Shapefile --------------------------------------------------------

library(rgdal)
library(maptools)

localDir   <- "Shapefiles"
tempDir    <- tempdir()
if (!file.exists(localDir)) dir.create(localDir)

url = "http://dds.cr.usgs.gov/pub/data/nationalatlas/countyp020_nt00009.tar.gz"
file       <- paste(localDir, basename(url) ,sep = "/")
if (!file.exists(file)) download.file(url, file)
untar(file, exdir = tempDir)

# Raw File
all        <- readOGR(tempDir, "countyp020", p4s = "+proj=longlat")
all$FIPS   <- as.numeric(levels(all$FIPS))[all$FIPS]

# Unmerged
usa        <- subset(all, FIPS < 57000)
usa        <- subset(usa, subset = !(STATE %in% c("AK", "HI")))

# Useful
USA        <- unionSpatialPolygons(usa, usa$FIPS)
USA        <- SpatialPolygonsDataFrame(USA, as.data.frame(row.names(USA)),
                                       match.ID=F)
names(USA) <- "FIPS"
USA$FIPS   <- as.numeric(as.character(USA$FIPS))
USA@data   <- data.frame(USA@data,usa[match(USA$FIPS, usa$FIPS),])
USA        <- USA[order(USA$FIPS),]

writeOGR(USA, localDir, "Lower48_2010_county", "ESRI Shapefile")
save(all, file = paste0(localDir, "/county2010.RData"))
save(USA, file = paste(localDir, "Lower48_2010_county.RData", sep = "/"))


# Weight Matrices ---------------------------------------------------------

library(spdep)

USA$fips           <- as.character(USA$FIPS)
place              <- substr(USA$fips, nchar(USA$fips) - 2,
                             nchar(USA$fips)) == "000"
USA                <- USA[!place,]

weights            <- poly2nb(USA)
summary(weights)

weights            <- nb2mat(weights, style = "B", zero.policy = T)
colnames(weights)  <- row.names(weights)
islands            <- knn2nb(knearneigh(coordinates(USA), k=2, longlat=T))
islands            <- nb2mat(islands, style = "B")
row.names(islands) <- USA$FIPS
colnames(islands)  <- USA$FIPS
contig             <- weights + islands
contig[contig > 1] <- 1
contig             <- contig/rowSums(contig)
xcontig            <- mat2listw(contig)
xW                 <- nb2listw(xcontig$neighbours, style = "W")

W                  <- nb2mat(xcontig$neighbours, style = "W")
colnames(W)        <- row.names(W)

save(xW, W, file = paste0(localDir, "/contigW.RData"))
#write.csv(neighbors, "neighborsmatrix.csv")
#write.csv(count, "neighborscount.csv")



print(paste0("Finished 0-shapefile at ", Sys.time()))
