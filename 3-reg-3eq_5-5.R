#Robert Dinterman, NCSU Economics PhD Student

# Load Data ---------------------------------------------------------------

load("1-data.RData")

keep <- data$FIPS %in% as.numeric(row.names(W))
data <- data[keep,]

# Define Variables for Regression -----------------------------------------
source("2-data-set.R")
est  <- setvars(y1.1 = "Exmpt_Num.2009", y1.2 = "Exmpt_Num.2010",
                y1.l = "POPESTIMATE2008", y2.1 = "employA.2008",
                y2.2 = "employA.2010", y3.1 = "total_prov.2008B",
                y3.2 = "total_prov.2010B",
                scal = T, data = data, W = W)
est1 <- est
names(est1) <- paste0(names(est), "1")
est2 <- est
names(est2) <- paste0(names(est), "2")
est3 <- est
names(est3) <- paste0(names(est), "3")

# Estimation Procedures ---------------------------------------------------

source("0-functions.R")

# Equation 1 (Population) -------------------------------------------------

source("spreg2.R")
allvars      <- c("y1_l", "y2_l", "y3_l", "Wy1_l", "Wy2_l", "Wy3_l", "UNrate",
                  "rurala", "ruraln", "hwy", "wagesA.2008", "taxwageA.2008",
                  "MEDHOMVAL", "MEDHHINC", "Scale", "share", "tpi", "EDUC",
                  "permitunit", "share65", "poverty", "area")

endo        <- c("WY2", "y2", "WY3", "y3") #endogenous variables
xnames      <- c("y1_l", "y2_l", "Wy2_l", "y3_l", "Wy3_l",
                 "MEDHOMVAL", #"BLACK", # "UNrate", "MEDHHINC",
                 "Scale", "share", "share65", "rurala", "ruraln")

endo1   <- paste0(endo, "1")
xnames1 <- paste0(xnames, "1")

form1 <- reformulate(xnames1, "y11")
end1  <- reformulate(endo1)
inst1 <- reformulate(termlabels = paste0(allvars, "1"))

ols1 <- lm(reformulate(c(xnames1, endo1), "y11"), data = est1)

gs2sls1 <- spreg2(formula = form1, data = est1, listw = xW, lag.instr = T,
                  endog = end1, instruments = inst1, model = "error",
                  het = T)

# Equation 2 (Employment)--------------------------------------------------


endo        <- c("WY1", "y1", "WY3", "y3") #endogenous variables
xnames       <- c("y1_l", "Wy1_l", "y2_l", "y3_l", "Wy3_l",
                   "hwy", "EDUC", "wagesA.2008", "taxwageA.2008",
                   "UNrate", "rurala", "ruraln")

endo2   <- paste0(endo, "2")
xnames2 <- paste0(xnames, "2")

form2 <- reformulate(xnames2, "y22")
end2  <- reformulate(endo2)
inst2 <- reformulate(termlabels = paste0(allvars, "2"))

ols2 <- lm(reformulate(c(xnames2, endo2), "y22"), data = est2)

gs2sls2 <- spreg2(formula = form2, data = est2, listw = xW, lag.instr = T,
                  endog = end2, instruments = inst2, model = "error",
                  het = T)

# Equation 3 (Broadband)--------------------------------------------------

endo        <- c("WY1", "y1", "WY2", "y2") #endogenous variables
xnames      <- c("y1_l", "Wy1_l", "y2_l", "Wy2_l", "y3_l",
                  "tpi", "MEDHHINC", "share", "hwy", "wagesA.2008",
                  "permitunit", "rurala", "ruraln")

endo3   <- paste0(endo, "3")
xnames3 <- paste0(xnames, "3")

form3 <- reformulate(xnames3, "y33")
end3  <- reformulate(endo3)
inst3 <- reformulate(termlabels = paste0(allvars, "3"))

ols3 <- lm(reformulate(c(xnames3, endo3), "y33"), data = est3)

gs2sls3 <- spreg2(formula = form3, data = est3, listw = xW, lag.instr = T,
                  endog = end3, instruments = inst3, model = "error",
                  het = T)


# FGS3SLS -----------------------------------------------------------------
source("0-fgs3sls.R")
fgs.results <- fgs3sls_3(gs2sls1, gs2sls2, gs2sls3,
                         as.matrix(est1[,endo1]), as.matrix(est2[,endo2]),
                         as.matrix(est3[,endo3]),
                         est$Ph, W)


ols    <- rbind(ols.results(ols1), ols.results(ols2), ols.results(ols3))
ols    <- table.results(ols)
olsVAR <- adiag(vcovHC(ols1), vcovHC(ols2), vcovHC(ols3))

stage1   <- cbind(gs2sls1$coefficients, sqrt(diag(gs2sls1$var)))
stage2   <- cbind(gs2sls2$coefficients, sqrt(diag(gs2sls2$var)))
stage3   <- cbind(gs2sls3$coefficients, sqrt(diag(gs2sls3$var)))

stage    <- rbind(stage1, stage2, stage3)
stage    <- table.results(stage)
stageVAR <- adiag(gs2sls1$var, gs2sls2$var, gs2sls3$var)

rtrue  <- fgs.results$r
rtrue1 <- rtrue[1:nrow(W)]
rtrue2 <- rtrue[(nrow(W)+1):(2*nrow(W))]
rtrue3 <- rtrue[(2*nrow(W)+1):(3*nrow(W))]

# Moran Tests -------------------------------------------------------------
library(spdep)
m1eq1  <- moran.test(ols1$residuals, xW, randomisation = F,
                     alternative = "two.sided")
m1eq2  <- moran.test(ols2$residuals, xW, randomisation = F,
                     alternative = "two.sided")
m1eq3  <- moran.test(ols3$residuals, xW, randomisation = F,
                     alternative = "two.sided")

m2eq1  <- moran.test(gs2sls1$residuals, xW, randomisation = F,
                     alternative = "two.sided")
m2eq2  <- moran.test(gs2sls2$residuals, xW, randomisation = F,
                     alternative = "two.sided")
m2eq3  <- moran.test(gs2sls3$residuals, xW, randomisation = F,
                     alternative = "two.sided")

m3eq1  <- moran.test(rtrue1, xW, randomisation = F, alternative = "two.sided")
m3eq2  <- moran.test(rtrue2, xW, randomisation = F, alternative = "two.sided")
m3eq3  <- moran.test(rtrue3, xW, randomisation = F, alternative = "two.sided")


# Wald Tests --------------------------------------------------------------

source("3-wald-tests.R")

waldols     <- wald3eq_new(ols, olsVAR)
waldstage   <- wald3eq_new(stage, stageVAR)
waldresults <- wald3eq_new(fgs.results$results, fgs.results$VAR)


# Preliminary Results -----------------------------------------------------

olsresult   <- names3eq_new(ols)
stageresult <- names3eq_new(stage)
fgsresult   <- names3eq_new(fgs.results$results)

dir = "3-reg-3eq_5-5"
if (!file.exists(dir)) dir.create(dir)
write.csv(specify_decimal(ols, 4), file = paste0(dir,"/ols-raw.csv"))
write.csv(specify_decimal(olsresult$results, 4), file = paste0(dir,"/ols.csv"))
write.csv(specify_decimal(waldols$waldtests, 4),
          file = paste0(dir,"/wald-ols.csv"))
write.csv(specify_decimal(waldols$waldspatial, 4),
          file = paste0(dir,"/wald-spatial-ols.csv"))


write.csv(specify_decimal(stage, 4), file = paste0(dir,"/stage2-raw.csv"))
write.csv(specify_decimal(stageresult$results, 4),
          file = paste0(dir,"/stage2.csv"))
write.csv(specify_decimal(waldstage$waldtests, 4),
          file = paste0(dir,"/wald-stage2.csv"))
write.csv(specify_decimal(waldstage$waldspatial, 4),
          file = paste0(dir,"/wald-spatial-stage2.csv"))


write.csv(specify_decimal(fgs.results$results, 4),
          file = paste0(dir,"/results-raw.csv"))
write.csv(specify_decimal(fgsresult$results, 4),
          file = paste0(dir,"/results.csv"))
write.csv(specify_decimal(waldresults$waldtests, 4),
          file = paste0(dir,"/wald-results.csv"))
write.csv(specify_decimal(waldresults$waldspatial, 4),
          file = paste0(dir,"/wald-spatial-results.csv"))


# Table Results
moranA  <- cbind(c(m1eq1$estimate[1], m1eq1$p.value),
                 c(m2eq1$estimate[1], m2eq1$p.value),
                 c(m3eq1$estimate[1], m3eq1$p.value))
moranA  <- specify_decimal(moranA, 4)
tableA  <- cbind(olsresult$tableA, stageresult$tableA, fgsresult$tableA)
tableA  <- rbind(tableA, moranA)

moranB  <- cbind(c(m1eq2$estimate[1], m1eq2$p.value),
                 c(m2eq2$estimate[1], m2eq2$p.value),
                 c(m3eq2$estimate[1], m3eq2$p.value))
moranB  <- specify_decimal(moranB, 4)
tableB  <- cbind(olsresult$tableB, stageresult$tableB, fgsresult$tableB)
tableB  <- rbind(tableB, moranB)

moranC  <- cbind(c(m1eq3$estimate[1], m1eq3$p.value),
                 c(m2eq3$estimate[1], m2eq3$p.value),
                 c(m3eq3$estimate[1], m3eq3$p.value))
moranC  <- specify_decimal(moranC, 4)
tableC  <- cbind(olsresult$tableC, stageresult$tableC, fgsresult$tableC)
tableC  <- rbind(tableC, moranC)

waldtab <- cbind(latable(round(waldols$waldtests, 4), wald = T),
                 latable(round(waldstage$waldtests, 4), wald = T),
                 latable(round(waldresults$waldtests, 4), wald = T))

waldtabsp <- cbind(latable(round(waldols$waldspatial, 4), wald = T),
                   latable(round(waldstage$waldspatial, 4), wald = T),
                   latable(round(waldresults$waldspatial, 4), wald = T))

write.csv(tableA,  file = paste0(dir, "/tableA.csv"))
write.csv(tableB,  file = paste0(dir, "/tableB.csv"))
write.csv(tableC,  file = paste0(dir, "/tableC.csv"))
write.csv(waldtab, file = paste0(dir, "/waldtab.csv"))
write.csv(waldtabsp, file = paste0(dir, "/waldtab-spatial.csv"))