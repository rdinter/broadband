#Robert Dinterman, NCSU Economics PhD Student


# Load Data ---------------------------------------------------------------

load("1-data.RData")

keep <- data$FIPS %in% as.numeric(row.names(W))
data <- data[keep,]
est  <- data.frame(data$FIPS)

# Define Variables for Regression -----------------------------------------

#Population
est$y1   = data$Exmpt_Num.2010 + data$Exmpt_Num.2009 #+ data$Exmpt_Num.2008
est$y1   = scale(est$y1)

est$WY1  = W %*% est$y1
#est$WY1  = scale(est$WY1)

est$Wy1  = est$y1 + est$WY1

est$y1_l = scale(data$POPESTIMATE2008)
# est$y1_l = est$y1_l + W %*% est$y1_l
est$Wy1_l = W %*% est$y1_l

#Employment
est$y2   = data$employA.2010 - data$employA.2008
est$y2   = scale(est$y2)

est$WY2  = W %*% est$y2
#est$WY2  = scale(est$WY2)

est$Wy2  = est$y2 + est$WY2

est$y2_l = scale(data$employA.2008)
# est$y2_l = est$y2_l + W %*% est$y2_l
est$Wy2_l= W %*% est$y2_l

#Broadband
est$dBB   = data$total_prov.2010B - data$total_prov.2008B
est$BB    = data$total_prov.2008B

est$y3    = scale(est$dBB)
est$WY3   = W %*% est$y3

est$Wy3   = est$y3 + est$WY3

est$y3_l  = scale(data$total_prov.2008B)
# est$y3_l  = est$y3_l + W %*% est$y3_l
est$Wy3_l = W %*% est$y3_l

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
# est$wagesA.2008 = data$wagesA.2008 / data$Emp.2008
# est$taxwageA.2008 = data$taxwageA.2008 / data$Emp.2008
est$wagesA.2008 = data$wagesA.2008 / data$employA.2008
est$taxwageA.2008 = data$taxwageA.2008 / data$employA.2008

#
est$MEDHOMVAL   <- log(data$MEDHOMVAL)
est$MEDHHINC    <- log(data$MEDHHINC)
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
vars            <- c("BB", "UNrate", "MEDHOMVAL", "MEDHHINC", "BLACK",
                     "Scale", "share", "tpi", "hwy", "EDUC", "wagesA.2008",
                     "taxwageA.2008", "permitunit", "share65", "rurala",
                     "ruraln", "poverty", "area")
est[, vars]     <- scale(est[, vars])

# Estimation Procedures ---------------------------------------------------

source("0-functions.R")

ols   <- data.frame()
stage <- data.frame()
sperr <- data.frame()

# Instrument Matrix -------------------------------------------------------

H   <- as.matrix(est[, vars[-1]])
WH  <- W %*% H
WWH <- W %*% WH
Ph  <- cbind(rep(1, nrow(W)), H, WH, WWH)
# Prj <- Ph %*% solve(t(Ph) %*% Ph) %*% t(Ph)

# Equation 1 (Population) -------------------------------------------------

endo1        <- c("WY2", "y2", "BB") #endogenous variables
xnames1      <- c("y1_l", "y2_l", "Wy2_l",# "BB",
                  "ones", "MEDHOMVAL", "BLACK",# "MEDHHINC", "UNrate", 
                  "Scale", "share", "share65", "rurala",
                  "ruraln", "poverty")#, "area")#, "permitunit")

equation1 <- two.stage(data = est, n = 1, endo = endo1, xnames = xnames1,
                       y = "y1", Ph = Ph, xW = xW, W = W)


# Equation 2 (Employment)--------------------------------------------------


endo2        <- c("WY1", "y1", "BB") #endogenous variables
xnames2       <- c("y1_l", "Wy1_l", "y2_l",# "BB",
                   "ones", "hwy", "EDUC", "wagesA.2008", "taxwageA.2008",
                   "UNrate", "MEDHHINC", "share65", "rurala", "ruraln")#, "area")

equation2 <- two.stage(data = est, n = 2, endo = endo2, xnames = xnames2,
                       y = "y2", Ph = Ph, xW = xW, W = W)

# FGS3SLS -----------------------------------------------------------------

fgs.results <- fgs3sls.2(equation1$work, equation2$work,
                         endo1, endo2,
                         xnames1, xnames2)

ols    <- rbind(equation1$ols, equation2$ols)
ols    <- table.results(ols)
olsVAR <- adiag(equation1$olsVAR, equation2$olsVAR)

stage    <- rbind(equation1$stage, equation2$stage)
stage    <- table.results(stage)
stageVAR <- adiag(equation1$stageVAR, equation2$stageVAR)

sperr  <- rbind(equation1$sperr, equation2$sperr)
sperr  <- table.results(sperr, df = 1)

rtrue  <- fgs.results$r
rtrue1 <- rtrue[1:nrow(W)]
rtrue2 <- rtrue[(nrow(W)+1):(2*nrow(W))]

m3eq1  <- moran.test(rtrue1, xW, randomisation = F, alternative = "two.sided")
m3eq2  <- moran.test(rtrue2, xW, randomisation = F, alternative = "two.sided")


# Wald Tests --------------------------------------------------------------

source("3-wald-tests.R")

waldols     <- wald2eq(ols, olsVAR)
waldstage   <- wald2eq(stage, stageVAR)
waldresults <- wald2eq(fgs.results$results, fgs.results$VAR)


# Preliminary Results -----------------------------------------------------

olsresult   <- names2eq(ols)
stageresult <- names2eq(stage)
fgsresult   <- names2eq(fgs.results$results)

m3eq1
m3eq2

dir = "3-reg-2eq"
if (!file.exists(dir)) dir.create(dir)
write.csv(specify_decimal(ols, 4), file = paste0(dir,"/ols-raw.csv"))
write.csv(specify_decimal(olsresult$results, 4), file = paste0(dir,"/ols.csv"))
write.csv(specify_decimal(waldols, 4), file = paste0(dir,"/wald-ols.csv"))


write.csv(specify_decimal(stage, 4), file = paste0(dir,"/stage2-raw.csv"))
write.csv(specify_decimal(stageresult$results, 4),
          file = paste0(dir,"/stage2.csv"))
write.csv(specify_decimal(waldstage, 4), file = paste0(dir,"/wald-stage2.csv"))

write.csv(specify_decimal(fgs.results$results, 4),
          file = paste0(dir,"/results-raw.csv"))
write.csv(specify_decimal(fgsresult$results, 4),
          file = paste0(dir,"/results.csv"))
write.csv(specify_decimal(waldresults, 4),
          file = paste0(dir,"/wald-results.csv"))

# Table Results
moranA  <- cbind(c(equation1$moran1$estimate[1], equation1$moran1$p.value),
                 c(equation1$moran2$estimate[1], equation1$moran2$p.value),
                 c(m3eq1$estimate[1], m3eq1$p.value))
moranA  <- specify_decimal(moranA, 4)
tableA  <- cbind(olsresult$tableA, stageresult$tableA, fgsresult$tableA)
tableA  <- rbind(tableA, moranA)

moranB  <- cbind(c(equation2$moran1$estimate[1], equation2$moran1$p.value),
                 c(equation2$moran2$estimate[1], equation2$moran2$p.value),
                 c(m3eq2$estimate[1], m3eq2$p.value))
moranB  <- specify_decimal(moranB, 4)
tableB  <- cbind(olsresult$tableB, stageresult$tableB, fgsresult$tableB)
tableB  <- rbind(tableB, moranB)

waldtab <- cbind(latable(round(waldols, 4), wald = T),
                 latable(round(waldstage, 4), wald = T),
                 latable(round(waldresults, 4), wald = T))

write.csv(tableA,  file = paste0(dir, "/tableA.csv"))
write.csv(tableB,  file = paste0(dir, "/tableB.csv"))

write.csv(waldtab, file = paste0(dir, "/waldtab.csv"))