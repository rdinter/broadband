#Robert Dinterman, NCSU Economics PhD Student

# Tidying up the data. This format appears to be wide

rm(list = ls())

library(readr)
load("0-data/Shapefiles/contigW.RData")


# Data Combining ----------------------------------------------------------

temp <- read_csv("0-data/FCC County Data/FCC08-13.csv") #broadband data
temp$FIPS <- temp$county_fips
temp$year <- factor(temp$year)
levels(temp$year) <- c("2008B", "2009A", "2009B", "2010A", "2010B", "2011A",
                        "2011B", "2012A", "2012B", "2013A", "2013B")
temp <- temp[,c("FIPS", "year", "rfc_per_1000_hhs", "tmw_prov",
                "rfc_per_1000_hhs_btop", "total_prov", "total_residential_prov",
                "total_residential_prov_nbp")]
data <- reshape(temp, idvar = "FIPS", timevar = "year", direction = "wide")

load("0-data/QCEW/QCEWAnnual_.RData") #employment, called annual
# qcew <- read_csv("0-data/QCEW/QCEW_.csv")
temp <- subset(annual, year >= 2000 & ownership == 0)
temp <- temp[,c("year", "FIPS", "establishmentsA", "employA", "wagesA",
                "taxwageA", "contrA", "avgwageA", "avgpayA")]
temp <- reshape(temp, idvar = "FIPS", timevar = "year", direction = "wide")

# #NAICS Classifications
# annual[is.na(annual)] <- -1
# qcew       <-subset(annual, ownership == 5) #Only Private
# qcew$NAICS <- gsub("\\s","", qcew$NAICS)
# qcew$NAICS <- factor(qcew$NAICS)
# qcew       <- qcew[,c("FIPS", "year", "NAICS",
#                       "employA", "establishmentsA", "wagesA")]
# NAICS      <- reshape(qcew, idvar = c("FIPS","year"), timevar = "NAICS",
#                 v.names = c("employA", "establishmentsA", "wagesA"),
#                 direction = "wide")
rm(annual)

### Change FIPS with 999 at end to 000
temp$fips <- as.character(temp$FIPS)
place     <- substr(temp$fips, nchar(temp$fips) - 2, nchar(temp$fips)) == "999"
temp$FIPS[place] = temp$FIPS[place] - 999


check <- temp$FIPS %in% data$FIPS
temp$FIPS[!check] #should only be state aggregate levels

check <- data$FIPS %in% temp$FIPS
data$FIPS[!check] #missing territories and 15005 in BB data

data <- merge(data, temp, by = "FIPS", all.y = T)

#Break down QCEW by NAICS... needs work
# temp <- subset(annual, year >= 2007 & ownership != 0)
# temp <- temp[,c("year", "FIPS", "NAICS", "establishmentsA", "employA", "wagesA",
#                 "taxwageA", "contrA", "avgwageA", "avgpayA")]
# atemp <- reshape(temp, idvar = "FIPS", timevar = "year", v.names = "NAICS",
#                  direction = "wide")
# rm(annual)


tempi <- read_csv("0-data/IRS/inflows0410.csv") #migration
tempi <- subset(tempi, ofips == 96000)
tempi$FIPS <- tempi$dfips
tempi <- tempi[,c("Return_Num", "Exmpt_Num", "Aggr_AGI", "year", "FIPS")]
tempi <- reshape(tempi, idvar = "FIPS", timevar = "year", direction = "wide")

tempo <- read_csv("0-data/IRS/outflows0410.csv")
tempo <- subset(tempo, dfips == 96000)
tempo$FIPS <- tempo$ofips
tempo <- tempo[,c("Return_Num", "Exmpt_Num", "Aggr_AGI", "year", "FIPS")]
tempo <- reshape(tempo, idvar = "FIPS", timevar = "year", direction = "wide")

temp <- tempi - tempo
temp$FIPS <- tempi$FIPS
rm(tempi, tempo)

check <- temp$FIPS %in% data$FIPS
temp$FIPS[!check] #should be state and national levels

check <- data$FIPS %in% temp$FIPS
data$FIPS[!check] #should be state levels

data <- merge(data, temp, by = "FIPS", all.x = T)


temp <- read_csv("0-data/LAU/LAUnemp.csv") #unemployment
temp <- subset(temp, year >= 2000)
temp <- reshape(temp, idvar = "FIPS", timevar = "year", direction = "wide")

check <- temp$FIPS %in% data$FIPS
temp$FIPS[!check] #should be none

check <- data$FIPS %in% temp$FIPS
data$FIPS[!check] #state level 999

data <- merge(data, temp, by = "FIPS", all.x = T)


temp <- read_csv("0-data/Controls/controls.csv") #controls
temp <- subset(temp, select = -c(SUMLEV, REGION, DIVISION, STATE, COUNTY,
                                 STNAME, CTYNAME))
# temp <- temp[,c("FIPS", "ESTIMATESBASE2000", "POPESTIMATE2000",
#                 "POPESTIMATE2001", "POPESTIMATE2002", "POPESTIMATE2003",
#                 "POPESTIMATE2004", "POPESTIMATE2005", "POPESTIMATE2006",
#                 "POPESTIMATE2007", "POPESTIMATE2008", "POPESTIMATE2009",
#                 "CENSUS2010POP", "POPESTIMATE2010", "POPESTIMATE2010_",
#                 "POPESTIMATE2011", "POPESTIMATE2012", "BLACK", "EDUC",
#                 "MEDHOMVAL", "MEDHHINC", "AREA", "POPSQ2010",
#                 "Poverty.Estimate.All.Ages", "Poverty.Percent.All.Ages",
#                 "Poverty.Estimate.Under.Age.18", "Poverty.Percent.Under.Age.18",
#                 "Poverty.Estimate.Ages.5.17", "Poverty.Percent.Ages.5.17",
#                 "share", "Scale", "climate", "topo", "ruc", "HWYCOUNT", 
#                 "HWYAREA",
#                 "HWYMEAN", "HWYSUM")]

check <- temp$FIPS %in% data$FIPS
temp$FIPS[!check] #state levels with 000

check <- data$FIPS %in% temp$FIPS
data$FIPS[!check] #state levels with 999, missing 2201, 2201, 2201, 2201, 2232,
                   # 2232, 2232, 2280, 2280, 2280, 2280

data <- merge(data, temp, by = "FIPS", all.x = T)


temp <- read_csv("0-data/Terrain/terrain.csv") #terrain
temp$FIPS <- temp$ID
temp <- temp[,c("FIPS", "tri", "tpi", "roughness", "slope", "aspect")]

check <- temp$FIPS %in% data$FIPS
temp$FIPS[!check] #state levels with 000

check <- data$FIPS %in% temp$FIPS
data$FIPS[!check] #state levels with 999, missing 2013, 2016, 2020, 2050, 2060,
                   # 2068, 2070, 2090, 2100, 2105, 2110, 2122, 2130, 2150, 2164,
                   # 2170, 2180, 2185, 2188, 2195, 2198, 2201, 2220, 2230, 2232,
                   # 2240, 2261, 2270, 2275, 2280, 2282, 2290
data <- merge(data, temp, by = "FIPS", all.x = T)

temp <- read_csv("0-data/Permits/permits.csv") #Residential Permit Data
names(temp) <- gsub("-", ".", names(temp))
temp <- subset(temp, year == 2007)

temp$permitbuild <- temp$Bldgs_1.unit + temp$Bldgs_2.units + temp$Bldgs3.4_units +
  temp$Bldgs5_units
temp$permitunit  <- temp$Units_1.unit + temp$Units_2.units + temp$Units3.4_units +
  temp$Units_5_units
temp$permitval   <- temp$Value_1.unit + temp$Value_2.units + temp$Value3.4_units +
  temp$Value_5_units

temp <- temp[,c("FIPS", "permitbuild", "permitunit", "permitval")]

check <- temp$FIPS %in% data$FIPS
temp$FIPS[!check] #Missing: 2231 12025 26999 35099 51780 2231 2999 12025 26999
                  #35099 51780 2231 12025 51780 2231 12025 51780 2231 12025
                  #51780 2231 12025 2231 12025 12025 12025 43001 52003 52010
                  #43001 52003 52010
#temp$FIPS[temp$FIPS == 12025] <- 12086 #Miami-Dade


check <- data$FIPS %in% temp$FIPS
data$FIPS[!check] #Missing state levels with 000 and: 2068 2105 2164 2195 2198
                  #2230 2240 2275 5097 5109 13183 13307 17047 17169 20049 20179
                  #21007 21023 21039 21061 21063 21105 21119 21129 21131 21139
                  #21153 21159 21165 21169 21171 21189 21201 21223 21237 28015
                  #29223 31005 31007 31009 31075 31113 31117 31171 32009 32011
                  #35003 35011 35019 35021 35023 35033 35059 46017 46113 46137
                  #47007 47137 47175 48033 48045 48095 48105 48131 48137 48173
                  #48205 48229 48235 48243 48247 48261 48269 48289 48301 48311
                  #48327 48333 48393 48431 48433 48443 48447 48505 54013 54017
data <- merge(data, temp, by = "FIPS", all.x = T)
rm(temp, check, place)

save(data, xW, W, file = "1-data.RData")

# Summary Statistics ------------------------------------------------------
load("1-data.RData")
library(spdep)

keep <- data$FIPS %in% as.numeric(row.names(W))
data <- data[keep,]

mean(data$total_prov.2008B)
sd(data$total_prov.2008B)
mean(data$total_prov.2010B)
sd(data$total_prov.2010B)
mean(data$total_prov.2012B)
sd(data$total_prov.2012B)

mean(data$rfc_per_1000_hhs_btop.2008B)*20
sd(data$rfc_per_1000_hhs_btop.2008B)*20
mean(data$rfc_per_1000_hhs_btop.2010B)*20
sd(data$rfc_per_1000_hhs_btop.2010B)*20
mean(data$rfc_per_1000_hhs_btop.2012B)*20
sd(data$rfc_per_1000_hhs_btop.2012B)*20

mean(data$employA.2008)
sd(data$employA.2008)
mean(data$employA.2010)
sd(data$employA.2010)
mean(data$employA.2012)
sd(data$employA.2012)

mean(data$establishmentsA.2008)
sd(data$establishmentsA.2008)
mean(data$establishmentsA.2010)
sd(data$establishmentsA.2010)
mean(data$establishmentsA.2012)
sd(data$establishmentsA.2012)

mean(data$POPESTIMATE2008)
sd(data$POPESTIMATE2008)
mean(data$POPESTIMATE2010)
sd(data$POPESTIMATE2010)
mean(data$POPESTIMATE2012)
sd(data$POPESTIMATE2012)

summary(data$Exmpt_Num.2008)
summary(data$Exmpt_Num.2010)