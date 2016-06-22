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
                y3.2 = NULL,
                scal = T, data = data, W = W)
est1 <- est
names(est1) <- paste0(names(est), "1")
est2 <- est
names(est2) <- paste0(names(est), "2")

# Estimation Procedures ---------------------------------------------------

source("0-functions.R")

ols   <- data.frame()
stage <- data.frame()
sperr <- data.frame()

# Equation 1 (Population) -------------------------------------------------

source("spreg2.R")
allvars      <- c("y1_l", "y2_l", "Wy1_l", "Wy2_l", "UNrate", "rurala",
                  "ruraln", "hwy", "wagesA.2008", "taxwageA.2008", "MEDHOMVAL",
                  "MEDHHINC", "Scale", "share", "tpi", "EDUC", "permitunit",
                  "share65", "poverty", "area")

endo        <- c("WY2", "y2", "BB") #endogenous variables
xnames      <- c("y1_l", "y2_l", "Wy2_l",# "BB",
                "MEDHOMVAL", #"BLACK",# "MEDHHINC", "UNrate", 
                "Scale", "share", "share65", "rurala",
                "ruraln", "poverty")#, "area")#, "permitunit")

endo1   <- paste0(endo, "1")
xnames1 <- paste0(xnames, "1")

form1 <- reformulate(xnames1, "y11")
end1  <- reformulate(endo1)
inst1 <- reformulate(termlabels = paste0(allvars, "1"))

ols1 <- lm(reformulate(c(xnames1, endo1), "y11"), data = est1)

gs2sls1 <- spreg2(formula = form1, data = est1, listw = xW, lag.instr = T,
                   endog = end1, instruments = inst1, model = "error",
                   het = T)


# Equation 2 (Employment) -------------------------------------------------
endo        <- c("WY1", "y1", "BB") #endogenous variables
xnames      <- c("y1_l", "Wy1_l", "y2_l",# "BB",
                "hwy", "EDUC", "wagesA.2008", "taxwageA.2008",
                "UNrate", "MEDHHINC", "share65", "rurala", "ruraln")#,"area")

endo2   <- paste0(endo, "2")
xnames2 <- paste0(xnames, "2")

form2 <- reformulate(xnames2, "y22")
end2  <- reformulate(endo2)
inst2 <- reformulate(termlabels = paste0(allvars, "2"))


ols2 <- lm(reformulate(c(xnames2, endo2), "y22"), data = est2)

gs2sls2 <- spreg2(formula = form2, data = est2, listw = xW, lag.instr = T,
                    endog = end2, instruments = inst2, model = "error",
                    het = T)


# FGS3SLS -----------------------------------------------------------------
source("0-fgs3sls.R")
fgs.results <- fgs3sls_2(gs2sls1, gs2sls2,
                         as.matrix(est1[,endo1]), as.matrix(est2[,endo2]),
                         est$Ph, W)





ols    <- rbind(ols.results(ols1), ols.results(ols2))
ols    <- table.results(ols)
olsVAR <- adiag(vcovHC(ols1), vcovHC(ols2))

stage1   <- cbind(gs2sls1$coefficients, sqrt(diag(gs2sls1$var)))
stage2   <- cbind(gs2sls2$coefficients, sqrt(diag(gs2sls2$var)))

stage    <- rbind(stage1, stage2)
stage    <- table.results(stage)
stageVAR <- adiag(gs2sls1$var, gs2sls2$var)

# sperr  <- rbind(equation1$sperr, equation2$sperr)
# sperr  <- table.results(sperr, df = 1)

rtrue  <- fgs.results$r
rtrue1 <- rtrue[1:nrow(W)]
rtrue2 <- rtrue[(nrow(W)+1):(2*nrow(W))]

m3eq1  <- moran.test(rtrue1, xW, randomisation = F, alternative = "two.sided")
m3eq2  <- moran.test(rtrue2, xW, randomisation = F, alternative = "two.sided")


# Wald Tests --------------------------------------------------------------

source("3-wald-tests.R")

waldols     <- wald2eq_new(ols, olsVAR)
waldstage   <- wald2eq_new(stage, stageVAR)
waldresults <- wald2eq_new(fgs.results$results, fgs.results$VAR)


# Preliminary Results -----------------------------------------------------

olsresult   <- names2eq_new(ols)
stageresult <- names2eq_new(stage)
fgsresult   <- names2eq_new(fgs.results$results)

m3eq1
m3eq2

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