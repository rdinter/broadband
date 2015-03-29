#Robert Dinterman, NCSU Economics PhD Student

# Control variable summary

# Load Data ---------------------------------------------------------------

load("1-data.RData")
library(xtable)

keep <- data$FIPS %in% as.numeric(row.names(W))
data <- data[keep,]
est  <- data.frame(data$FIPS)

#Unemployment Rate
est$UNrate = data$Unemp.2008 / (data$Unemp.2008 + data$Emp.2008)

#Metro
est$metro  = factor(data$ruc)
levels(est$metro) = c("metro", "metro", "metro",
                      "rural-adjacent", "rural-nonadjacent", "rural-adjacent",
                      "rural-nonadjacent","rural-adjacent","rural-nonadjacent")

#Highway
est$hwy    = data$HWYSUM / data$HWYAREA

#Per Capita Wages
# est$wagesA.2008 = data$wagesA.2008 / data$Emp.2008
# est$taxwageA.2008 = data$taxwageA.2008 / data$Emp.2008
est$wagesA.2008 = data$wagesA.2008 / data$employA.2008
est$taxwageA.2008 = data$taxwageA.2008 / data$employA.2008


est$MEDHOMVAL   <- data$MEDHOMVAL
est$MEDHHINC    <- data$MEDHHINC
est$BLACK       <- data$BLACK
est$Scale       <- data$Scale
est$share       <- data$share
est$tpi         <- data$tpi
est$EDUC        <- data$EDUC
est$permitunit  <- data$permitunit
est$share65     <- data$Over64_2000_per
est[is.na(est)] <- 0
est$ones        <- 1
vars            <- c("MEDHHINC", "MEDHOMVAL", "wagesA.2008",
                     "taxwageA.2008", "UNrate", "BLACK", "Scale",
                     "share", "tpi", "hwy", "EDUC", "permitunit",
                     "share65")

tab <- cbind(sapply(est[,vars], mean), sapply(est[,vars], sd))
xtable(tab, digits = -4)