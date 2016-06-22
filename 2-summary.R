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
est$rurala <- est$metro == "rural-adjacent"
est$ruraln <- est$metro == "rural-nonadjacent"

#Highway
est$hwy    = data$HWYSUM / data$HWYAREA

#Per Capita Wages
# est$wagesA.2008   = data$wagesA.2008 / data$Emp.2008
# est$taxwageA.2008 = data$taxwageA.2008 / data$Emp.2008
est$wagesA.2008   = data$wagesA.2008 / data$employA.2008
est$taxwageA.2008 = data$taxwageA.2008 / data$employA.2008

#
est$MEDHOMVAL   <- data$MEDHOMVAL
est$MEDHHINC    <- data$MEDHHINC
est$BLACK       <- data$BLACK
est$Scale       <- data$Scale
est$share       <- data$share
est$tpi         <- data$tpi
est$EDUC        <- data$EDUC
est$permitunit  <- data$permitunit
est$share65     <- data$Over64_2000_per
est$poverty     <- data$Poverty.Percent.Ages.5.17
est$area        <- log(data$AREA)
est[is.na(est)] <- 0
est$ones        <- 1
vars            <- c("UNrate", "MEDHOMVAL", "MEDHHINC", "BLACK",
                     "Scale", "share", "tpi", "hwy", "EDUC", "wagesA.2008",
                     "taxwageA.2008", "permitunit", "share65", "rurala",
                     "ruraln", "poverty", "area")

tab1 <- cbind(sapply(est[,vars], mean), sapply(est[,vars], sd))
xtable(tab1, digits = -4)

# Summary Statistics ------------------------------------------------------

vars08 <- c("total_prov.2008B", "employA.2008", "establishmentsA.2008",
            "POPESTIMATE2008")
vars10 <- c("total_prov.2010B", "employA.2010", "establishmentsA.2010",
            "POPESTIMATE2010")

tab2 <- cbind(sapply(data[,vars08], mean), sapply(data[,vars08], sd), 
              sapply(data[,vars10], mean), sapply(data[,vars10], sd),
              sapply(data[,vars10]- data[,vars08], mean),
              sapply(data[,vars10]- data[,vars08], sd))
xtable(tab2)
mean(data$Exmpt_Num.2009 + data$Exmpt_Num.2010)
sd(data$Exmpt_Num.2009 + data$Exmpt_Num.2010)

# Spatial -----------------------------------------------------------------

data$WBB2008 = W %*% data$total_prov.2008B
data$WBB2010 = W %*% data$total_prov.2010B
data$WEMP2008 = W %*% data$employA.2008
data$WEMP2010 = W %*% data$employA.2010
data$WEST2008 = W %*% data$establishmentsA.2008
data$WEST2010 = W %*% data$establishmentsA.2010
data$WPOP2008 = W %*% data$POPESTIMATE2008
data$WPOP2010 = W %*% data$POPESTIMATE2010
data$WMIG = W %*% (data$Exmpt_Num.2009 + data$Exmpt_Num.2010)

new08 <- c("WBB2008", "WEMP2008", "WEST2008", "WPOP2008")
new10 <- c("WBB2010", "WEMP2010", "WEST2010", "WPOP2010")

tab3 <- cbind(sapply(data[,new08], mean), sapply(data[,new08], sd), 
              sapply(data[,new10], mean), sapply(data[,new10], sd),
              sapply(data[,new10]- data[,new08], mean),
              sapply(data[,new10]- data[,new08], sd))
xtable(tab3)
mean(data$WMIG)
sd(data$WMIG)