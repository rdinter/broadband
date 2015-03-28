sublast <- function(x, n = 1){
  substr(x, nchar(x) - n + 1, nchar(x))
}

latable <- function(resultsA, wald = NULL){
  n <- dim(resultsA)[1]
  rname <- vector("character", length = 2*n)
  rname[seq(1, 2*n, by = 2)] <- rownames(resultsA)
  
  DELTA <- as.character(resultsA[,1])
  SE    <- paste0("(", resultsA[,2], ")")
  
  # Significance
  if (is.null(wald)) {
    DELTA[resultsA[,4] < 0.1]  <- paste0(DELTA[resultsA[,4] < 0.1],"*")
    DELTA[resultsA[,4] < 0.05] <- paste0(DELTA[resultsA[,4] < 0.05],"*")
    DELTA[resultsA[,4] < 0.01] <- paste0(DELTA[resultsA[,4] < 0.01],"*")
  } else {
    DELTA[resultsA[,2] < 0.1]  <- paste0(DELTA[resultsA[,2] < 0.1],"*")
    DELTA[resultsA[,2] < 0.05] <- paste0(DELTA[resultsA[,2] < 0.05],"*")
    DELTA[resultsA[,2] < 0.01] <- paste0(DELTA[resultsA[,2] < 0.01],"*")
  }
  
  results <- vector("character", length = 2*n)
  results[seq(1, 2*n, by = 2)] <- DELTA
  results[seq(2, 2*n, by = 2)] <- SE
  results <- as.matrix(results)
  rownames(results) <- rname
  return(results)
}

wald <- function(r, R, VAR){
  if (!is.vector(r)) r <- as.vector(r)
  R  <- t(R)
  df <- dim(R)[1]
  
  stat <- t(r) %*% qr.solve(R %*% VAR %*% t(R)) %*% r
  if (df == 1){
    pval <- 1 - sqrt(pchisq(stat, df))
  } else {
    pval <- 1 - pchisq(stat, df)
  }
  
  return(cbind(stat, pval))
}

wald2eq <- function(results, VAR){
  
  nA1  = "BBS1"
  nA2  = "y2_lS1"
  nWA2 = "Wy2_lS1"
  nA2l = "y2"
  nWA2l= "WY2"
  nLP  = "y1_lS1"
  
  nB1  = "BBS2"
  nB2  = "y1_lS2"
  nWB2 = "Wy1_lS2"
  nB2l = "y1"
  nWB2l= "WY1"
  nLE  = "y2_lS2"
  
  A1   = results[nA1, "DELTA"]
  A2   = results[nA2, "DELTA"]
  WA2  = results[nWA2, "DELTA"]
  A2l  = results[nA2l, "DELTA"]
  WA2l = results[nWA2l, "DELTA"]
  LP   = -results[nLP, "DELTA"]
  
  B1   = results[nB1, "DELTA"]
  B2   = results[nB2, "DELTA"]
  WB2  = results[nWB2, "DELTA"]
  B2l  = results[nB2l, "DELTA"]
  WB2l = results[nWB2l, "DELTA"]
  LE   = -results[nLE, "DELTA"]
  
  #alpha 1 test
  r1       = A1 / LP
  R1       = vector("numeric", length = nrow(results))
  names(R1)= rownames(results)
  
  R1[nA1]   = 1 / LP
  R1[nLP]   = A1 / (LP^2)
  R1[nB2l]  = A1 / B2
  R1[nWB2l] = A1 / WB2
  
  R1 = t(as.matrix(R1))
  wald1 = t(r1) %*% qr.solve(R1 %*% VAR %*% t(R1)) %*% r1
  
  #alpha 2 test
  r2       = A2 / LP
  R2       = vector("numeric", length = nrow(results))
  names(R2)= rownames(results)
  
  R2[nA2]   = 1 / LP
  R2[nWA2]  = A2 / (LP*WA2)
  R2[nA2l]  = LE / LP
  R2[nWA2l] = (A2*LE) / (LP*WA2)
  R2[nLP]   = A2 / (LP^2)
  R2[nB2l]  = A2 / B2
  R2[nWB2l] = A2 / WB2
  
  R2 = t(as.matrix(R2))
  wald2 = t(r2) %*% qr.solve(R2 %*% VAR %*% t(R2)) %*% r2
  
  #beta 1 test
  r3       = B1 / LE
  R3       = vector("numeric", length = nrow(results))
  names(R3)= rownames(results)
  
  R3[nB1]   = 1 / LE
  R3[nLE]   = B1 / (LE^2)
  R3[nA2l]  = B1 / A2
  R3[nWA2l] = B1 / WA2
  
  R3 = t(as.matrix(R3))
  wald3 = t(r3) %*% qr.solve(R3 %*% VAR %*% t(R3)) %*% r3
  
  #beta 2 test
  r4       = B2 / LE
  R4       = vector("numeric", length = nrow(results))
  names(R4)= rownames(results)
  
  R4[nB2]   = 1 / LE
  R4[nWB2]  = B2 / (LE*WB2)
  R4[nB2l]  = LP / LE
  R4[nWB2l] = (B2*LP) / (LE*WB2)
  R4[nLE]   = B2 / (LE^2)
  R4[nA2l]  = B2 / A2
  R4[nWA2l] = B2 / WA2
  
  R4 = t(as.matrix(R4))
  wald4 = t(r4) %*% qr.solve(R4 %*% VAR %*% t(R4)) %*% r4
  
  # Joint a1 = b1 = 0 Test
  j1 = rbind(r1, r3)
  J1 = rbind(R1, R3)
  joint1 = t(j1) %*% qr.solve(J1 %*% VAR %*% t(J1)) %*% j1 #*(nrow(Z)/2)
  
  # Joint a2 = b2 = 0 Test
  j2 = rbind(r2, r4)
  J2 = rbind(R2, R4)
  joint2 = t(j2) %*% qr.solve(J2 %*% VAR %*% t(J2)) %*% j2 #*(nrow(Z)/2)

  waldtests <- list(alpha1 = wald1, alpha2 =  wald2,
                    beta1 =  wald3, beta2 = wald4,
                    a1b1 = joint1, a2b2 = joint2)
  return(waldtests)
}


wald3eq <- function(results, VAR){
  
  nA2   = "y2_l1" #employment t-1
  nWA2  = "Wy2_l1" #spatially lagged employment t-1
  nA2l  = "y21h" #employment change
  nWA2l = "WY21h" #spatially lagged employment change
  nA3   = "y3_l1" #broadband t-1
  nWA3  = "Wy3_l1" #spatially lagged broadband t-1
  nA3l  = "y31h" #broadband change
  nWA3l = "WY31h" #spatially lagged broadband change
  nLP   = "y1_l1" #population t-1
  nA    = c(nA2, nWA2, nA2l, nWA2l, nA3, nWA3, nA3l, nWA3l, nLP)
  
  nB2   = "y1_l2" #population t-1
  nWB2  = "Wy1_l2" #spatially lagged population t-1
  nB2l  = "y12h" #population change
  nWB2l = "WY12h" #spatially lagged population change
  nB3   = "y3_l2" #broadband t-1
  nWB3  = "Wy3_l2" #spatially lagged broadband t-1
  nB3l  = "y32h" #broadband change
  nWB3l = "WY32h" #spatially lagged broadband change
  nLE   = "y2_l2" #employment t-1
  nB    = c(nB2, nWB2, nB2l, nWB2l, nB3, nWB3, nB3l, nWB3l, nLE)
  
  nC2   = "y1_l3" #population t-1
  nWC2  = "Wy1_l3" #spatially lagged population t-1
  nC2l  = "y13h" #population change
  nWC2l = "WY13h" #spatially lagged population change
  nC3   = "y2_l3" #employment t-1
  nWC3  = "Wy2_l3" #spatially lagged employment t-1
  nC3l  = "y23h" #employment change
  nWC3l = "WY23h" #spatially lagged employment change
  nLB   = "y3_l3" #broadband t-1
  nC    = c(nC2, nWC2, nC2l, nWC2l, nC3, nWC3, nC3l, nWC3l, nLB)
  
  nC    = rownames(results)
  nA1   = nC[ (sublast(nC) == "1") & !(rownames(results) %in% c(nA, nB, nC))]
  nB1   = nC[ (sublast(nC) == "2") & !(rownames(results) %in% c(nA, nB, nC))]
  nC1   = nC[ (sublast(nC) == "3") & !(rownames(results) %in% c(nA, nB, nC))]
  
  A2    = results[nA2, "DELTA"]
  WA2   = results[nWA2, "DELTA"]
  A2l   = results[nA2l, "DELTA"]
  WA2l  = results[nWA2l, "DELTA"]
  A3    = results[nA3, "DELTA"]
  WA3   = results[nWA3, "DELTA"]
  A3l   = results[nA3l, "DELTA"]
  WA3l  = results[nWA3l, "DELTA"]
  LP    = -results[nLP, "DELTA"]
  
  B2    = results[nB2, "DELTA"]
  WB2   = results[nWB2, "DELTA"]
  B2l   = results[nB2l, "DELTA"]
  WB2l  = results[nWB2l, "DELTA"]
  B3    = results[nB3, "DELTA"]
  WB3   = results[nWB3, "DELTA"]
  B3l   = results[nB3l, "DELTA"]
  WB3l  = results[nWB3l, "DELTA"]
  LE    = -results[nLE, "DELTA"]
  
  C2    = results[nC2, "DELTA"]
  WC2   = results[nWC2, "DELTA"]
  C2l   = results[nC2l, "DELTA"]
  WC2l  = results[nWC2l, "DELTA"]
  C3    = results[nC3, "DELTA"]
  WC3   = results[nWC3, "DELTA"]
  C3l   = results[nC3l, "DELTA"]
  WC3l  = results[nWC3l, "DELTA"]
  LB    = -results[nLB, "DELTA"]
  
  A1    = results[nA1, "DELTA"]
  B1    = results[nB1, "DELTA"]
  C1    = results[nC1, "DELTA"]
  
  # Restrictions
  rA1       = A1 / LP # alpha_1 = 0
  rA2       = A2 / LP # alpha_2 = 0
  rWA2      = WA2 / A2# CANT RECALL
  rA3       = A3 / LP # alpha_3 = 0
  rWA3      = WA3 / A3# phi_ba = 0
  rLP       = LP      # lambda_p = 0
  rB1       = B1 / LE # beta_1 = 0
  rB2       = B2 / LE # beta_2 = 0
  rWB2      = WB2 / B2# CANT RECALL
  rB3       = B3 / LE # beta_3 = 0
  rWB3      = WB3 / B3# CANT RECALL
  rLE       = LE      # lambda_e = 0
  rG1       = C1 / LB # gamma_1 = 0
  rG2       = C2 / LB # gamma_2 = 0
  rWG2      = WC2 / C2# CANT RECALL
  rG3       = C3 / LB # gamma_3 = 0
  rWG3      = WC3 / C3# CANT RECALL
  rLB       = LB      # lambda_b = 0

  RA1          <- matrix(0, nrow = nrow(results), ncol = length(A1))
  rownames(RA1)<- rownames(results)
  RB1          <- matrix(0, nrow = nrow(results), ncol = length(B1))
  rownames(RB1)<- rownames(results)
  RG1          <- matrix(0, nrow = nrow(results), ncol = length(C1))
  rownames(RG1)<- rownames(results)
  
  RA2          <- vector("numeric", length = nrow(results))
  names(RA2)   <- rownames(results)
  RA2 -> RWA3 -> RA3 -> RWA3 -> RLP -> RB2 -> RWB3 -> RB3 -> RWB3 -> RLE
  RA2 -> RG2 -> RWG2 -> RG3 -> RWG3 -> RLB
  
  # alpha_1 = 0
  #   RA1[] =
  
  
  # alpha_2 = 0
  RA2[nA2]   = 1/LP
  RA2[nWA2]  = A2 / (LP*WA2)
  RA2[nA2l]  = LE / LP
  RA2[nWA2l] = (A2*LE) / (LP*WA2)
  RA2[nLP]   = A2 / (LP^2)
  RA2[nB2l]  = A2 / B2
  RA2[nWB2l] = A2 / WB2
  RA2[nC2l]  = A2 / C2
  RA2[nWC2l] = A2 / WC2
  
  # alpha_3 = 0
  RA3[nA3]   = 1/LP
  RA3[nWA3]  = A3 / (LP*WA3)
  RA3[nA3l]  = LB / LP
  RA3[nWA3l] = (A3*LB) / (LP*WA3)
  RA3[nLP]   = A3 / (LP^2)
  RA3[nB2l]  = A3 / B2
  RA3[nWB2l] = A3 / WB2
  RA3[nC2l]  = A3 / C2
  RA3[nWC2l] = A3 / WC2
  
  # beta_2 = 0
  RB2[nA2l]  = B2 / A2
  RB2[nWA2l] = B2 / WA2
  RB2[nB2]   = 1 / LE
  RB2[nWB2]  = B2 / (LE*WB2)
  RB2[nB2l]  = LP / LE
  RB2[nWB2l] = (B2*LP) / (LE*WB3)
  RB2[nLE]   = B2 / (LE^2)
  RB2[nC3l]  = B2 / C3
  RB2[nWC3l] = B2 / WC3
  
  # beta_ 3 = 0
  RB3[nA2l]  = B3 / A2
  RB3[nWA2l] = B3 / WA2
  RB3[nB3]   = 1 / LE
  RB3[nWB3]  = B3 / (LE*WB3)
  RB3[nB3l]  = LB / LE
  RB3[nWB3l] = (B3*LB) / (LE*WB3)
  RB3[nLE]   = B3 / (LE^2)
  RB3[nC3l]  = B3 / C3
  RB3[nWC3l] = B3 / WC3
  
  
  # gamma_1 = 0
  RG1[nA3l,]  = C1 / A3
  RG1[nWA3l,] = C1 / WA3
  RG1[nB3l,]  = C1 / B3
  RG1[nWB3l,] = C1 / WB3
  RG1[nC1,]   = 1 / LB
  RG1[nLB,]   = C1 / (LB^2)
  
  
  # gamma_2 = 0
  RG2[nA3l]  = C2 / A3
  RG2[nWA3l] = C2 / WA3
  RG2[nB3l]  = C2 / B3
  RG2[nWB3l] = C2 / WB3
  RG2[nC2]   = 1 / LB
  RG2[nWC2]  = C2 / (LB*WC2)
  RG2[nC2l]  = LP / LB
  RG2[nWC2l] = (C2*LP) / (LB*WC3)
  RG2[nLB]   = C2 / (LB^2)

  # gamma_3 = 0
  RG3[nA3l]  = C3 / A3
  RG3[nWA3l] = C3 / WA3
  RG3[nB3l]  = C3 / B3
  RG3[nWB3l] = C3 / WB3
  RG3[nC3]   = 1 / LB
  RG3[nWC3]  = C3 / (LB*WC3)
  RG3[nC3l]  = LE / LB
  RG3[nWC3l] = (C3*LE) / (LB*WC3)
  RG3[nLB]   = C3 / (LB^2)
  
  # lambda_b = 0
  RLB[nA3l]  = 1 / A3
  RLB[nWA3l] = 1 / WA3
  RLB[nB3l]  = 1 / B3
  RLB[nWB3l] = 1 / WB3
  RLB[nLB]   = 1
  
  # phi_ba = 0
  RWA3[nA3]   = - WA3 / (A3^2)
  RWA3[nA3l]  = 1 / A3
  RWA3[nWA3l] = - (WA3 * LB) / (A3^2)
  RWA3[nLP]   = LB / A3
  
  # WALD TESTING
  waldalpha2 = wald(rA2, RA2, VAR)
  waldalpha3 = wald(rA3, RA3, VAR)
  waldalpha  = wald(c(rA2, rA3), cbind(RA2, RA3), VAR)
  
  waldbeta2  = wald(rB2, RB2, VAR)
  waldbeta3  = wald(rB3, RB3, VAR)
  waldbeta   = wald(c(rB2, rB3), cbind(RB2, RB3), VAR)
  
  waldgamma2 = wald(rG2, RG2, VAR)
  waldgamma3 = wald(rG3, RG3, VAR)
  waldgamma  = wald(c(rG2, rG3), cbind(RG2, RG3), VAR)
  
  #   r = c(rG1, rG2, rG3, rLB, rWA3, rA3, rB3)
  r = c(rG2, rG3, rLB, rWA3, rA3, rB3)
  #   R = cbind(RG1, RG2, RG3, RLB, RWA3, RA3, RB3)
  R = cbind(RG2, RG3, RLB, RWA3, RA3, RB3)
  waldallG = wald(r, R, VAR)
  
  # INPUT THE ACTUAL LIST OF VALUES
  waldtests <- rbind(waldalpha2, waldalpha3, waldalpha,
                     waldbeta2, waldbeta3, waldbeta,
                     waldgamma2, waldgamma3, waldgamma,
                     waldallG)
  colnames(waldtests) <- c("Test Statistic", "P-Value")
  rownames(waldtests) <- c("waldalpha2", "waldalpha3", "waldalpha",
                           "waldbeta2", "waldbeta3", "waldbeta",
                           "waldgamma2", "waldgamma3", "waldgamma",
                           "waldallG")
#   waldtests <- list(alpha2 = waldalpha2, alpha3 =  waldalpha3,
#                     beta2 =  waldbeta2, beta3 = waldbeta3,
#                     gamma2 = waldgamma2, gamma3 = waldgamma3,
#                     alpha = waldalpha, beta = waldbeta, gamma = waldgamma,
#                     gammaALL = waldallG
#                     )
  return(waldtests)
}

names3eq <- function(results, k = 4){
  nA2   = "y2_l1" #employment t-1
  nWA2  = "Wy2_l1" #spatially lagged employment t-1
  nA2l  = "y21h" #employment change
  nWA2l = "WY21h" #spatially lagged employment change
  nA3   = "y3_l1" #broadband t-1
  nWA3  = "Wy3_l1" #spatially lagged broadband t-1
  nA3l  = "y31h" #broadband change
  nWA3l = "WY31h" #spatially lagged broadband change
  nLP   = "y1_l1" #population t-1
  nA    = c(nA2, nWA2, nA2l, nWA2l, nA3, nWA3, nA3l, nWA3l, nLP)
  resultsA <- results[nA,]
  rownames(resultsA) <- c("nA2", "nWA2", "nA2l", "nWA2l", "nA3",
                            "nWA3", "nA3l", "nWA3l", "nLP")
  tableA <- latable(round(resultsA, k))
  
  nB2   = "y1_l2" #population t-1
  nWB2  = "Wy1_l2" #spatially lagged population t-1
  nB2l  = "y12h" #population change
  nWB2l = "WY12h" #spatially lagged population change
  nB3   = "y3_l2" #broadband t-1
  nWB3  = "Wy3_l2" #spatially lagged broadband t-1
  nB3l  = "y32h" #broadband change
  nWB3l = "WY32h" #spatially lagged broadband change
  nLE   = "y2_l2" #employment t-1
  nB    = c(nB2, nWB2, nB2l, nWB2l, nB3, nWB3, nB3l, nWB3l, nLE)
  resultsB <- results[nB,]
  rownames(resultsB) <- c("nB2", "nWB2", "nB2l", "nWB2l", "nB3",
                          "nWB3", "nB3l", "nWB3l", "nLE")
  tableB <- latable(round(resultsB, k))
  
  nC2   = "y1_l3" #population t-1
  nWC2  = "Wy1_l3" #spatially lagged population t-1
  nC2l  = "y13h" #population change
  nWC2l = "WY13h" #spatially lagged population change
  nC3   = "y2_l3" #employment t-1
  nWC3  = "Wy2_l3" #spatially lagged employment t-1
  nC3l  = "y23h" #employment change
  nWC3l = "WY23h" #spatially lagged employment change
  nLB   = "y3_l3" #broadband t-1
  nC    = c(nC2, nWC2, nC2l, nWC2l, nC3, nWC3, nC3l, nWC3l, nLB)
  resultsC <- results[nC,]
  rownames(resultsC) <- c("nC2", "nWC2", "nC2l", "nWC2l", "nC3",
                          "nWC3", "nC3l", "nWC3l", "nLB")
  tableC <- latable(round(resultsC, k))
  
  results <- rbind(resultsA, resultsB, resultsC)
  output <- list(results = results, tableA = tableA,
                 tableB = tableB, tableC = tableC)
  return(output)
}
