#Robert Dinterman, NCSU Economics PhD Student

# Figures used in paper

# Load Data ---------------------------------------------------------------

load("1-data.RData")
library(spdep)
library(rgdal)

# Create a directory for the data
localDir <- "Figures"
if (!file.exists(localDir)) dir.create(localDir)


keep <- data$FIPS %in% as.numeric(row.names(W))
data <- data[keep,]

data$migration <- data$Exmpt_Num.2005 + data$Exmpt_Num.2006 + 
  data$Exmpt_Num.2007 + data$Exmpt_Num.2008 + data$Exmpt_Num.2009 +
  data$Exmpt_Num.2010 + data$Exmpt_Num.2011
top            <- quantile(data$migration, 0.9)
bottom         <- quantile(data$migration, 0.1)
data$migration[data$migration > top]    <- top
data$migration[data$migration < bottom] <- bottom

data$migrationchange <- data$Exmpt_Num.2008 + data$Exmpt_Num.2009 +
  data$Exmpt_Num.2010
top            <- quantile(data$migrationchange, 0.9)
bottom         <- quantile(data$migrationchange, 0.1)
data$migrationchange[data$migrationchange > top]    <- top
data$migrationchange[data$migrationchange < bottom] <- bottom

data$broadband <- data$total_prov.2010B - data$total_prov.2008B

data$bb <- cut(data$total_prov.2008B, breaks = c(0, 3, 7, 11, 15, 19, 50))
levels(data$bb) <- c("0 to 3", "4 to 7", "8 to 11", "12 to 15", "16 to 19",
                     "more than 20")

data$jobs  <- data$employA.2010 - data$employA.2008
top        <- quantile(data$jobs, 0.9)
bottom     <- quantile(data$jobs, 0.1)
data$jobs[data$jobs > top]    <- top
data$jobs[data$jobs < bottom] <- bottom

data$firms <- data$establishmentsA.2010 - data$establishmentsA.2008
top        <- quantile(data$firms, 0.9)
bottom     <- quantile(data$firms, 0.1)
data$firms[data$firms > top]    <- top
data$firms[data$firms < bottom] <- bottom

load("0-data/Shapefiles/Lower48_2010_county.RData")

usa <- merge(USA, data, by = "FIPS", all.x = T)
aea.proj = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-100
+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m'
#   http://www.remotesensing.org/geotiff/proj_list/albers_equal_area_conic.html
#   +proj=aea   +lat_1=Latitude of first standard parallel
#               +lat_2=Latitude of second standard parallel
#               +lat_0=Latitude of false origin 
#               +lon_0=Longitude of false origin
#               +x_0=Easting of false origin
#               +y_0=Northing of false origin
usa = spTransform(usa,CRS(aea.proj))
plot(usa)
title(main = 'Albers Equal Area')

# spplot(usa, zcol = "bb")
# spplot(usa, zcol = "migration")
# spplot(usa, zcol = "broadband")
# spplot(usa, zcol = "jobs")
# spplot(usa, zcol = "firms")

library(ggplot2)
usagg    <- fortify(usa, region = "FIPS")
usagg.df <- merge(usagg, usa@data, by.x = "id", by.y = "FIPS", all.x = T)

states  <- subset(usa, subset =                         #remove lakes
                    (FIPS - as.numeric(as.character(STATE_FIPS))*1000 !=0))
states  <- unionSpatialPolygons(states,states$STATE)
STATE   <- names(states)
states  <- SpatialPolygonsDataFrame(states,as.data.frame(STATE),match.ID=F)
stategg <- fortify(states, region = "STATE")

#Set the first layer for the red to green plots:
usa.plot <- ggplot(data = usagg.df, aes(x = long, y = lat, group = group)) +
  scale_fill_gradient2(low="red", high="green", na.value="blue", name="") +
  labs(x = "",y = "") +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(),
        plot.title = element_text(face="bold"), axis.text.x = element_blank(),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(), legend.position = c(1,0),
        legend.justification = c(1,0), legend.background = element_blank())
#         legend.direction = "horizontal")

#Migration 2004 -- 2010
mig04_10 <- usa.plot + geom_polygon(aes(fill = migration)) + 
  geom_path(data = stategg, colour = "black", lwd = 0.25)
png(filename = "Figures/migration-change-04-10.png", width = 600, units = "px")
print(mig04_10)
dev.off()
mig04_10 + labs(x = "",y = "", title = "Net Migration from 2004 to 2010")
ggsave("Figures/migration-change-04-10.pdf", width = 9, height = 7.5)

#Migration 2008 -- 2010
mig08_10 <- usa.plot + geom_polygon(aes(fill = migrationchange)) + 
  geom_path(data = stategg, colour = "black", lwd = 0.25)
png(filename = "Figures/migration-change-08-10.png", width = 600, units = "px")
print(mig08_10)
dev.off()
mig08_10 + labs(x='',y='', title="Net Migration from 2008 to 2010")
ggsave("Figures/migration-change-08-10.pdf", width = 9, height = 7.5)

#Broadband 2008 -- 2010
bb08_10 <- usa.plot + geom_polygon(aes(fill = broadband)) + 
  geom_path(data = stategg, colour = "black", lwd = 0.25)
png(filename = "Figures/broadband-change-08-10.png", width = 600, units = "px")
print(bb08_10)
dev.off()
bb08_10 + labs(x='',y='', title="Broadband Change from 2008 to 2010")
ggsave("Figures/broadband-change-08-10.pdf", width = 9, height = 7.5)

#Employment 2008 -- 2010
job08_10 <- usa.plot + geom_polygon(aes(fill = jobs)) + 
  geom_path(data = stategg, colour = "black", lwd = 0.25)
png(filename = "Figures/employment-change-08-10.png", width = 600, units = "px")
print(job08_10)
dev.off()
job08_10 + labs(x='',y='', title="Employment Change from 2008 to 2010")
ggsave("Figures/employment-change-08-10.pdf", width = 9, height = 7.5)

firm08_10 <- usa.plot + geom_polygon(aes(fill = firms)) + 
  geom_path(data = stategg, colour = "black", lwd = 0.25)
png(filename = "Figures/establishments-change-08-10.png", width = 600,
    units = "px")
print(firm08_10)
dev.off()
firm08_10 + labs(x="",y="", title="Establishment Change from 2008 to 2010")
ggsave("Figures/establishments-change-08-10.pdf", width = 9, height = 7.5)

#Single Year Plots
usa.plot <- ggplot(data = usagg.df, aes(x = long, y = lat, group = group)) +
  scale_fill_brewer(palette = "Greens", na.value = "blue", name = "") +
  labs(x = "",y = "") +
  guides(colour = guide_legend(override.aes = list(size = 4)))
  
bb08 <- usa.plot + geom_polygon(aes(fill = bb)) + 
  geom_path(data = stategg, colour = "black", lwd = 0.25) +
    theme(axis.ticks = element_blank(), axis.text.y = element_blank(),
        plot.title = element_text(face="bold"), axis.text.x = element_blank(),
        panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom", legend.direction = "horizontal")
png(filename = "Figures/broadband-2008.png", width = 600, units = "px")
print(bb08)
dev.off()

bb08 + labs(x='',y='', title="Broadband Providers, 2008")
ggsave("Figures/broadband-2008.pdf", width = 9, height = 7.5)