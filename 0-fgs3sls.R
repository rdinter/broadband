fgs3sls_2 <- function(est1, est2,       #Data
                      endo1, endo2,     #endogenous vars
                      Ph, W, #instruments and weight matrix
                      robust = T
){
  r1 <- est1$residuals
  r2 <- est2$residuals
  
  model1 <- est1$model
  model2 <- est2$model
  
  rho1 <- est1$coefficients["rho",]
  rho2 <- est2$coefficients["rho",]
    
  y1    <- as.matrix(model1[,1])
  colnames(y1) <- "y11"
  x1    <- as.matrix(model1[,-1])
  y1s   <- y1 - rho1 * W %*% y1
  x1s   <- x1 - rho1 * W %*% x1
  end1s <- endo1 - rho1 * W %*% endo1
  b1    <- qr.solve(Ph, end1s)
  end1h <- Ph %*% b1
  
  y2    <- as.matrix(model2[,1])
  colnames(y2) <- "y22"
  x2    <- as.matrix(model2[,-1])
  y2s   <- y2 - rho2 * W %*% y2
  x2s   <- x2 - rho2 * W %*% x2
  end2s <- endo2 - rho2 * W %*% endo2
  b2    <- qr.solve(Ph, end2s)
  end2h <- Ph %*% b2
  
  e1e2   <- crossprod(r1, r2) / sqrt((nrow(model1) - nrow(est1$coefficients))*
                                          (nrow(model2)-nrow(est2$coefficients)))
  e1e1   <- crossprod(r1) / (nrow(model1) - nrow(est1$coefficients))
  e2e2   <- crossprod(r2) / (nrow(model2) - nrow(est2$coefficients))
  SIGMA  <- matrix(cbind(e1e1, e1e2, e1e2, e2e2), 2)
  ISIG   <- qr.solve(SIGMA)
  
  ADJ    <- kronecker(ISIG,diag(nrow(model1)))
  
  Z1H   <- cbind(end1h, x1s)
  Z2H   <- cbind(end2h, x2s)
  ZH    <- cbind(kronecker(c(1,0), Z1H), kronecker(c(0,1), Z2H))
  colnames(ZH) <- c(colnames(Z1H), colnames(Z2H))
  
  Z1    <- cbind(endo1, x1)
  Z2    <- cbind(endo2, x2)
  Z     <- cbind(kronecker(c(1,0), Z1), kronecker(c(0,1), Z2))
  colnames(Z) <- c(colnames(Z1), colnames(Z2))
  
  Z1S    <- cbind(end1s, x1s)
  Z2S    <- cbind(end2s, x2s)
  ZS     <- cbind(kronecker(c(1,0), Z1S), kronecker(c(0,1), Z2S))
  colnames(ZS) <- c(colnames(Z1S), colnames(Z2S))
  
  Y       <- c(y1, y2)
  YS      <- c(y1s, y2s)
  
  DELTA  <- (qr.solve(t(ZH) %*% ADJ %*% ZH)) %*% (t(ZH) %*% ADJ %*% YS)
  
  rtrue  <- YS - ZS %*% DELTA
  rother <- Y  - Z %*% DELTA
  
  bread  <- tryCatch(qr.solve(t(ZH) %*% ADJ %*% ZH),
                     error = function(e){
                       require(MASS, quietly = T)
                       ginv(t(ZH) %*% ADJ %*% ZH)
                     })
  
  if (robust) {
    meat   <- t(ZH) %*% ADJ %*% diag(diag(crossprod(t(rtrue)))) %*% ADJ %*% ZH
    VAR    <- bread %*% meat %*% bread
  } else {
    VAR    <- bread
  }
  
  SE     <- sqrt(diag(VAR))
  
  results<- table.results(cbind(DELTA, SE),
                          df = length(Y) / 2 - length(DELTA))
  return(list(results = results,
              VAR = VAR,
              r = rtrue,
              rother = rother
  ))
}


fgs3sls_3 <- function(est1, est2, est3,   #Data
                      endo1, endo2, endo3, #endogenous vars
                      Ph, W, #instruments and weight matrix
                      robust = T
){
  r1 <- est1$residuals
  r2 <- est2$residuals
  r3 <- est3$residuals
  
  model1 <- est1$model
  model2 <- est2$model
  model3 <- est3$model
  
  rho1 <- est1$coefficients["rho",]
  rho2 <- est2$coefficients["rho",]
  rho3 <- est3$coefficients["rho",]
  
  y1    <- as.matrix(model1[,1])
  colnames(y1) <- "y11"
  x1    <- as.matrix(model1[,-1])
  y1s    <- y1 - rho1 * W %*% y1
  x1s    <- x1 - rho1 * W %*% x1
  end1s  <- endo1 - rho1 * W %*% endo1
  b1    <- qr.solve(Ph, end1s)
  end1h <- Ph %*% b1
  
  y2    <- as.matrix(model2[,1])
  colnames(y2) <- "y22"
  x2    <- as.matrix(model2[,-1])
  y2s    <- y2 - rho2 * W %*% y2
  x2s    <- x2 - rho2 * W %*% x2
  end2s  <- endo2 - rho2 * W %*% endo2
  b2    <- qr.solve(Ph, end2s)
  end2h <- Ph %*% b2
  
  y3    <- as.matrix(model3[,1])
  colnames(y3) <- "y33"
  x3    <- as.matrix(model3[,-1])
  y3s    <- y3 - rho3 * W %*% y3
  x3s    <- x3 - rho3 * W %*% x3
  end3s  <- endo3 - rho3 * W %*% endo3
  b3    <- qr.solve(Ph, end3s)
  end3h <- Ph %*% b3
  
  e1e2  <- crossprod(r1, r2) / sqrt((nrow(model1) - nrow(est1$coefficients))*
                                      (nrow(model2)-nrow(est2$coefficients)))
  e1e3  <- crossprod(r1, r3) / sqrt((nrow(model1) - nrow(est1$coefficients))*
                                      (nrow(model3)-nrow(est3$coefficients)))
  e2e3  <- crossprod(r2, r3) / sqrt((nrow(model2) - nrow(est2$coefficients))*
                                      (nrow(model3)-nrow(est3$coefficients)))
  e1e1   <- crossprod(r1) / (nrow(model1) - nrow(est1$coefficients))
  e2e2   <- crossprod(r2) / (nrow(model2) - nrow(est2$coefficients))
  e3e3   <- crossprod(r3) / (nrow(model3) - nrow(est3$coefficients))
  SIGMA <- matrix(cbind(e1e1,e1e2,e1e3,
                        e1e2,e2e2,e2e3,
                        e1e3,e2e3,e3e3),3)
  ISIG  <- qr.solve(SIGMA)
  
  ADJ    <- kronecker(ISIG,diag(nrow(model1)))
  
  Z1H   <- cbind(end1h, x1s)
  Z2H   <- cbind(end2h, x2s)
  Z3H   <- cbind(end3h, x3s)
  ZH    <- cbind(kronecker(c(1,0,0), Z1H), kronecker(c(0,1,0), Z2H),
                 kronecker(c(0,0,1), Z3H))
  colnames(ZH) <- c(colnames(Z1H), colnames(Z2H), colnames(Z3H))
  
  Z1    <- cbind(endo1, x1)
  Z2    <- cbind(endo2, x2)
  Z3    <- cbind(endo3, x3)
  Z     <- cbind(kronecker(c(1,0,0), Z1), kronecker(c(0,1,0), Z2),
                 kronecker(c(0,0,1), Z3))
  colnames(Z) <- c(colnames(Z1), colnames(Z2), colnames(Z3))
  
  Z1S    <- cbind(end1s, x1s)
  Z2S    <- cbind(end2s, x2s)
  Z3S    <- cbind(end3s, x3s)
  ZS     <- cbind(kronecker(c(1,0,0), Z1S), kronecker(c(0,1,0), Z2S),
                 kronecker(c(0,0,1), Z3S))
  colnames(ZS) <- c(colnames(Z1S), colnames(Z2S), colnames(Z3S))
  
  Y      <- c(y1, y2, y3)
  YS     <- c(y1s, y2s, y3s)
  
  DELTA  <- (qr.solve(t(ZH) %*% ADJ %*% ZH)) %*% (t(ZH) %*% ADJ %*% YS)
  
  rtrue  <- YS - ZS %*% DELTA
  rother <- Y  - Z %*% DELTA
  
  bread  <- tryCatch(qr.solve(t(ZH) %*% ADJ %*% ZH),
                     error = function(e){
                       require(MASS, quietly = T)
                       ginv(t(ZH) %*% ADJ %*% ZH)
                     })
  
  if (robust) {
    meat   <- t(ZH) %*% ADJ %*% diag(diag(crossprod(t(rtrue)))) %*% ADJ %*% ZH
    VAR    <- bread %*% meat %*% bread
  } else {
    VAR    <- bread
  }
  
  SE     <- sqrt(diag(VAR))
  
  results<- table.results(cbind(DELTA, SE),
                          df = length(Y) / 2 - length(DELTA))
  return(list(results = results,
              VAR = VAR,
              r = rtrue,
              rother = rother
  ))
}
