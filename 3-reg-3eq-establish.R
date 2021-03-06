#Robert Dinterman, NCSU Economics PhD Student

# Load Data ---------------------------------------------------------------

load("1-data.RData")

keep <- data$FIPS %in% as.numeric(row.names(W))
data <- data[keep,]

# Define Variables for Regression -----------------------------------------
source("2-data-set.R")
est  <- setvars(y1.1 = "Exmpt_Num.2009", y1.2 = "Exmpt_Num.2010",
                y1.l = "POPESTIMATE2008", y2.1 = "establishmentsA.2008",
                y2.2 = "establishmentsA.2010", y3.1 = "total_prov.2008B",
                y3.2 = "total_prov.2010B",
                scal = T, data = data, W = W)

# Estimation Procedures ---------------------------------------------------

source("0-functions.R")

ols   <- data.frame()
stage <- data.frame()
sperr <- data.frame()

# Equation 1 (Population) -------------------------------------------------

endo1        <- c("WY2", "y2", "WY3", "y3") #endogenous variables
xnames1      <- c("y1_l", "y2_l", "Wy2_l", "y3_l", "Wy3_l", "ones",
                  "MEDHOMVAL", #"BLACK", # "UNrate", "MEDHHINC",
                  "Scale", "share", "share65", "rurala", "ruraln")

equation1 <- two.stage(data = est, n = 1, endo = endo1, xnames = xnames1,
                       y = "y1", Ph = est$Ph, xW = xW, W = W)


# Equation 2 (Employment)--------------------------------------------------


endo2        <- c("WY1", "y1", "WY3", "y3") #endogenous variables
xnames2       <- c("y1_l", "Wy1_l", "y2_l", "y3_l", "Wy3_l", "ones",
                   "hwy", "EDUC", "wagesA.2008", "taxwageA.2008",
                   "UNrate", "rurala", "ruraln")

equation2 <- two.stage(data = est, n = 2, endo = endo2, xnames = xnames2,
                       y = "y2", Ph = est$Ph, xW = xW, W = W)


# Equation 3 (Broadband)--------------------------------------------------

endo3        <- c("WY1", "y1", "WY2", "y2") #endogenous variables
xnames3      <- c("y1_l", "Wy1_l", "y2_l", "Wy2_l", "y3_l", "ones",
                  "tpi", "MEDHHINC", "share", "hwy", "wagesA.2008",
                  "permitunit", "rurala", "ruraln")

equation3 <- two.stage(data = est, n = 3, endo = endo3, xnames = xnames3,
                       y = "y3", Ph = est$Ph, xW = xW, W = W)


# FGS3SLS -----------------------------------------------------------------

fgs.results <- fgs3sls.3(equation1$work, equation2$work, equation3$work,
                         endo1, endo2, endo3,
                         xnames1, xnames2, xnames3)

ols    <- rbind(equation1$ols, equation2$ols, equation3$ols)
ols    <- table.results(ols)
olsVAR <- adiag(adiag(equation1$olsVAR, equation2$olsVAR), equation3$olsVAR)

stage    <- rbind(equation1$stage, equation2$stage, equation3$stage)
stage    <- table.results(stage)
stageVAR <- adiag(adiag(equation1$stageVAR, equation2$stageVAR),
                  equation3$stageVAR)

sperr  <- rbind(equation1$sperr, equation2$sperr, equation3$sperr)
sperr  <- table.results(sperr, df = 1)

rtrue  <- fgs.results$r
rtrue1 <- rtrue[1:nrow(W)]
rtrue2 <- rtrue[(nrow(W)+1):(2*nrow(W))]
rtrue3 <- rtrue[(2*nrow(W)+1):(3*nrow(W))]

m3eq1  <- moran.test(rtrue1, xW, randomisation = F, alternative = "two.sided")
m3eq2  <- moran.test(rtrue2, xW, randomisation = F, alternative = "two.sided")
m3eq3  <- moran.test(rtrue3, xW, randomisation = F, alternative = "two.sided")


# Wald Tests --------------------------------------------------------------

source("3-wald-tests.R")

waldols     <- wald3eq(ols, olsVAR)
waldstage   <- wald3eq(stage, stageVAR)
waldresults <- wald3eq(fgs.results$results, fgs.results$VAR)


# Preliminary Results -----------------------------------------------------

olsresult   <- names3eq(ols)
stageresult <- names3eq(stage)
fgsresult   <- names3eq(fgs.results$results)

m3eq1
m3eq2
m3eq3

dir = "3-reg-3eq-establish"
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

moranC  <- cbind(c(equation3$moran1$estimate[1], equation3$moran1$p.value),
                 c(equation3$moran2$estimate[1], equation3$moran2$p.value),
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