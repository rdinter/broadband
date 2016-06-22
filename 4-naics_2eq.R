

naics_2eq <- function(NAICS1, NAICS2, data = data, W = W, ...){
  # Define Variables for Regression -----------------------------------------
  source("2-data-set.R")
  est  <- setvars(y1.1 = "Exmpt_Num.2009", y1.2 = "Exmpt_Num.2010",
                  y1.l = "POPESTIMATE2008", y2.1 = NAICS1,
                  y2.2 = NAICS2, y3.1 = "total_prov.2008B",
                  y3.2 = NULL,
                  scal = T, data = data, W = W)
  source("0-functions.R")
  # Equation 1 (Population) -------------------------------------------------
  endo1        <- c("WY2", "y2", "BB") #endogenous variables
  xnames1      <- c("y1_l", "y2_l", "Wy2_l",# "BB",
                    "ones", "MEDHOMVAL", #"BLACK",# "MEDHHINC", "UNrate", 
                    "Scale", "share", "share65", "rurala",
                    "ruraln", "poverty")#, "area")#, "permitunit")
  
  equation1 <- two.stage.simple(data = est, n = 1, endo = endo1,
                                xnames = xnames1, y = "y1", Ph = est$Ph,
                                xW = xW, W = W)
  # Equation 2 (Employment)--------------------------------------------------
  endo2        <- c("WY1", "y1", "BB") #endogenous variables
  xnames2      <- c("y1_l", "Wy1_l", "y2_l",# "BB",
                    "ones", "hwy", "EDUC", "wagesA.2008", "taxwageA.2008",
                    "UNrate", "MEDHHINC", "share65", "rurala", "ruraln")#,"area")
  
  equation2 <- two.stage.simple(data = est, n = 2, endo = endo2,
                                xnames = xnames2, y = "y2", Ph = est$Ph,
                                xW = xW, W = W)
  # FGS3SLS -----------------------------------------------------------------
  fgs.results <- fgs3sls.2(equation1$work, equation2$work,
                           endo1, endo2,
                           xnames1, xnames2)
  rtrue  <- fgs.results$r
  rtrue1 <- rtrue[1:nrow(W)]
  rtrue2 <- rtrue[(nrow(W)+1):(2*nrow(W))]
  # Wald Tests --------------------------------------------------------------
  source("3-wald-tests.R")
  
  deltas  <- se2eq(fgs.results$results, fgs.results$VAR)
  results <- deltas[c("alpha1", "beta1"), ]
  return(results)
}



naics_2eq_alpha <- function(NAICS1, NAICS2, data = data, W = W, ...){
  # Define Variables for Regression -----------------------------------------
  source("2-data-set.R")
  est  <- setvars(y1.1 = "Exmpt_Num.2009", y1.2 = "Exmpt_Num.2010",
                  y1.l = "POPESTIMATE2008", y2.1 = NAICS1,
                  y2.2 = NAICS2, y3.1 = "total_prov.2008B",
                  y3.2 = NULL,
                  scal = T, data = data, W = W)
  source("0-functions.R")
  # Equation 1 (Population) -------------------------------------------------
  endo1        <- c("WY2", "y2", "BB") #endogenous variables
  xnames1      <- c("y1_l", "y2_l", "Wy2_l",# "BB",
                    "ones", "MEDHOMVAL", #"BLACK",# "MEDHHINC", "UNrate", 
                    "Scale", "share", "share65", "rurala",
                    "ruraln", "poverty")#, "area")#, "permitunit")
  
  equation1 <- two.stage.simple(data = est, n = 1, endo = endo1,
                                xnames = xnames1, y = "y1", Ph = est$Ph,
                                xW = xW, W = W)
  # Equation 2 (Employment)--------------------------------------------------
  endo2        <- c("WY1", "y1", "BB") #endogenous variables
  xnames2      <- c("y1_l", "Wy1_l", "y2_l",# "BB",
                    "ones", "hwy", "EDUC", "wagesA.2008", "taxwageA.2008",
                    "UNrate", "MEDHHINC", "share65", "rurala", "ruraln")#,"area")
  
  equation2 <- two.stage.simple(data = est, n = 2, endo = endo2,
                                xnames = xnames2, y = "y2", Ph = est$Ph,
                                xW = xW, W = W)
  # FGS3SLS -----------------------------------------------------------------
  fgs.results <- fgs3sls.2(equation1$work, equation2$work,
                           endo1, endo2,
                           xnames1, xnames2)
  rtrue  <- fgs.results$r
  rtrue1 <- rtrue[1:nrow(W)]
  rtrue2 <- rtrue[(nrow(W)+1):(2*nrow(W))]
  # Wald Tests --------------------------------------------------------------
  source("3-wald-tests.R")
  
  deltas  <- se2eq(fgs.results$results, fgs.results$VAR)
  results <- deltas["alpha1", ]
  return(results)
}

naics_2eq_beta <- function(NAICS1, NAICS2, data = data, W = W, ...){
  # Define Variables for Regression -----------------------------------------
  source("2-data-set.R")
  est  <- setvars(y1.1 = "Exmpt_Num.2009", y1.2 = "Exmpt_Num.2010",
                  y1.l = "POPESTIMATE2008", y2.1 = NAICS1,
                  y2.2 = NAICS2, y3.1 = "total_prov.2008B",
                  y3.2 = NULL,
                  scal = T, data = data, W = W)
  source("0-functions.R")
  # Equation 1 (Population) -------------------------------------------------
  endo1        <- c("WY2", "y2", "BB") #endogenous variables
  xnames1      <- c("y1_l", "y2_l", "Wy2_l",# "BB",
                    "ones", "MEDHOMVAL", #"BLACK",# "MEDHHINC", "UNrate", 
                    "Scale", "share", "share65", "rurala",
                    "ruraln", "poverty")#, "area")#, "permitunit")
  
  equation1 <- two.stage.simple(data = est, n = 1, endo = endo1,
                                xnames = xnames1, y = "y1", Ph = est$Ph,
                                xW = xW, W = W)
  # Equation 2 (Employment)--------------------------------------------------
  endo2        <- c("WY1", "y1", "BB") #endogenous variables
  xnames2      <- c("y1_l", "Wy1_l", "y2_l",# "BB",
                    "ones", "hwy", "EDUC", "wagesA.2008", "taxwageA.2008",
                    "UNrate", "MEDHHINC", "share65", "rurala", "ruraln")#,"area")
  
  equation2 <- two.stage.simple(data = est, n = 2, endo = endo2,
                                xnames = xnames2, y = "y2", Ph = est$Ph,
                                xW = xW, W = W)
  # FGS3SLS -----------------------------------------------------------------
  fgs.results <- fgs3sls.2(equation1$work, equation2$work,
                           endo1, endo2,
                           xnames1, xnames2)
  rtrue  <- fgs.results$r
  rtrue1 <- rtrue[1:nrow(W)]
  rtrue2 <- rtrue[(nrow(W)+1):(2*nrow(W))]
  # Wald Tests --------------------------------------------------------------
  source("3-wald-tests.R")
  
  deltas  <- se2eq(fgs.results$results, fgs.results$VAR)
  results <- deltas["beta1", ]
  return(results)
}


naics_2eq_alpha_new <- function(NAICS1, NAICS2, data = data, W = W, ...){
  est  <- setvars(y1.1 = "Exmpt_Num.2009", y1.2 = "Exmpt_Num.2010",
                  y1.l = "POPESTIMATE2008", y2.1 = NAICS1,
                  y2.2 = NAICS2, y3.1 = "total_prov.2008B",
                  y3.2 = NULL,
                  scal = T, data = data, W = W)
  est1 <- est
  names(est1) <- paste0(names(est), "1")
  est2 <- est
  names(est2) <- paste0(names(est), "2")
  # Estimation Procedures ---------------------------------------------------
  source("0-functions.R")
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
  
  gs2sls2 <- spreg2(formula = form2, data = est2, listw = xW, lag.instr = T,
                    endog = end2, instruments = inst2, model = "error",
                    het = T)
  
  # FGS3SLS -----------------------------------------------------------------
  source("0-fgs3sls.R")
  fgs.results <- fgs3sls_2(gs2sls1, gs2sls2,
                           as.matrix(est1[,endo1]), as.matrix(est2[,endo2]),
                           est$Ph, W)
  rtrue  <- fgs.results$r
  rtrue1 <- rtrue[1:nrow(W)]
  rtrue2 <- rtrue[(nrow(W)+1):(2*nrow(W))]
  # Wald Tests --------------------------------------------------------------
  source("3-wald-tests.R")
  
  deltas  <- se2eq_new(fgs.results$results, fgs.results$VAR)
  results <- deltas["alpha1", ]
  return(results)
}

naics_2eq_beta_new <- function(NAICS1, NAICS2, data = data, W = W, ...){
  est  <- setvars(y1.1 = "Exmpt_Num.2009", y1.2 = "Exmpt_Num.2010",
                  y1.l = "POPESTIMATE2008", y2.1 = NAICS1,
                  y2.2 = NAICS2, y3.1 = "total_prov.2008B",
                  y3.2 = NULL,
                  scal = T, data = data, W = W)
  est1 <- est
  names(est1) <- paste0(names(est), "1")
  est2 <- est
  names(est2) <- paste0(names(est), "2")
  # Estimation Procedures ---------------------------------------------------
  source("0-functions.R")
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
  
  gs2sls2 <- spreg2(formula = form2, data = est2, listw = xW, lag.instr = T,
                    endog = end2, instruments = inst2, model = "error",
                    het = T)
  
  # FGS3SLS -----------------------------------------------------------------
  source("0-fgs3sls.R")
  fgs.results <- fgs3sls_2(gs2sls1, gs2sls2,
                           as.matrix(est1[,endo1]), as.matrix(est2[,endo2]),
                           est$Ph, W)
  rtrue  <- fgs.results$r
  rtrue1 <- rtrue[1:nrow(W)]
  rtrue2 <- rtrue[(nrow(W)+1):(2*nrow(W))]
  # Wald Tests --------------------------------------------------------------
  source("3-wald-tests.R")
  
  deltas  <- se2eq_new(fgs.results$results, fgs.results$VAR)
  results <- deltas["beta1", ]
  return(results)
}