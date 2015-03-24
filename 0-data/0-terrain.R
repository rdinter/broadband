#Robert Dinterman, NCSU Economics PhD Student

# Terrain, this will take a substantial amount of time.

print(paste0("Started 0-terrain at ", Sys.time()))

rm(list=ls())
library(rgdal)
library(raster)

tempDir <- tempdir()
unlink(tempDir, recursive = T)

# Create a directory for the data
localDir   <- "0-data/Terrain"
if (!file.exists(localDir)) dir.create(localDir)
load("0-data/Shapefiles/Lower48_2010_county.RData")

## Get elevation data
elevation <- getData("alt", country = "USA", path = localDir)
# elevation is a list of different rasters, only use the first one because the
#  contiguous US is first and the rest are AK, islands, HI

x         <- terrain(elevation[[1]],
                     c("slope", "aspect", "TPI", "TRI", "roughness"))
plot(x)
cellStats(x, mean)

data      <- extract(x, USA, fun = mean, na.rm = T, weights = T, df = T)
data$ID   <- getSpPPolygonsIDSlots(USA)

write.csv(data, paste(localDir, "terrain.csv", sep = "/"), row.names = F)

rm(list = ls())

print(paste0("Finished 0-terrain at ", Sys.time()))
