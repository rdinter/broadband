#Robert Dinterman, NCSU Economics PhD Student

# Define Variables for Regression -----------------------------------------

setvars <- function(y1.1, y1.2, y1.l, y2.1, y2.2, y3.1, y3.2 = NULL,
                    scal = T, data, W){
  est  <- data.frame(data$FIPS)
  
  #Population
  est$y1   = data[, y1.2] + data[, y1.1]
  if (scal)  est$y1   = scale(est$y1)
  
  est$WY1  = W %*% est$y1
  #est$WY1  = scale(est$WY1)
  
  est$Wy1  = est$y1 + est$WY1
  
  est$y1_l = data[, y1.l]
  if (scal) est$y1_l = scale(est$y1_l)
  # est$y1_l = est$y1_l + W %*% est$y1_l
  est$Wy1_l = W %*% est$y1_l
  
  #Employment
  est$y2   = data[, y2.2] - data[, y2.1]
  if (scal) est$y2   = scale(est$y2)
  
  est$WY2  = W %*% est$y2
  #est$WY2  = scale(est$WY2)
  
  est$Wy2  = est$y2 + est$WY2
  
  est$y2_l = data[, y2.1]
  if (scal) est$y2_l = scale(est$y2_l)
  # est$y2_l = est$y2_l + W %*% est$y2_l
  est$Wy2_l= W %*% est$y2_l
  
  #Broadband
  est$dBB   = data$total_prov.2010B - data$total_prov.2008B
  est$BB    = data[, y3.1]
  
  if (!is.null(y3.2)){
    est$y3    = data[, y3.2] - data[, y3.1]
    if (scal) est$y3 = scale(est$y3)
    est$WY3   = W %*% est$y3
    
    est$Wy3   = est$y3 + est$WY3
    
    est$y3_l  = est$BB
    if (scal) est$y3_l = scale(est$y3_l)
    # est$y3_l  = est$y3_l + W %*% est$y3_l
    est$Wy3_l = W %*% est$y3_l
    
  }
  
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
  vars            <- c("BB", "UNrate", "MEDHOMVAL", "MEDHHINC", #"BLACK",
                       "Scale", "share", "tpi", "hwy", "EDUC", "wagesA.2008",
                       "taxwageA.2008", "permitunit", "share65", "rurala",
                       "ruraln", "poverty", "area")
  
  if (scal)  est[, vars]     <- scale(est[, vars])
  
  H      <- as.matrix(est[, vars[-1]])
  WH     <- W %*% H
  WWH    <- W %*% WH
  Ph     <- cbind(rep(1, nrow(W)), H, WH, WWH)
  est$Ph <- Ph
  
  return(est)
}

