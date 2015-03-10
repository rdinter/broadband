#Started: 10-17-2014
#Last Update: 2-25-2015
#Robert Dinterman, NCSU Economics PhD Student


#Functions
regress <- function(y,x){
  if (qr(x)$rank==ncol(x)) beta <- qr.solve(x,y)
  if (qr(x)$rank<ncol(x)) beta <- (qr(t(x)%*%x)$qr)%*%(t(x)%*%y)
  beta
}

varcov <- function(x, r){
  n = nrow(x)
  k = ncol(x)
  sanx = solve(crossprod(x))
  meat = t(x) %*% diag(diag(crossprod(t(r)))) %*% x
  
  (sanx %*% meat %*% sanx) * n / (n - k)
}

varcov2 <- function(x, r, s){
  n = nrow(x)
  k = ncol(x)
  sanx = solve(crossprod(x))
  meat = t(x) %*% diag(diag(crossprod(t(r)))) %*% x
  
  s * (sanx %*% meat %*% sanx) * n / (n - k)
}

table.results <- function(tab, df = 3000){
  DELTA  <- tab[, 1]
  SE     <- tab[, 2]
  
  TValue <- DELTA/SE
  PValue <- pt(-abs(TValue), df)*2
  
  results <- cbind(DELTA, SE, TValue, PValue)
  row.names(results) <- row.names(tab)
  results
}

gmproc <- function(r,W){
  #GM Function
  u = as.matrix(r)
  v = W %*% u
  w = W %*% v
  
  #Moment Matrix
  G = t(matrix(c(2*t(u) %*% v, -t(v) %*% v, length(u), #transposed for order
                 2*t(w) %*% v, -t(w) %*% w, sum(diag(t(W) %*% W)),
                 (t(u) %*% w + t(v) %*% v), -t(v) %*% w, 0),
               3)) / length(u)
  
  g = matrix(c(t(u) %*% u, t(v) %*% v, t(u) %*% v),
             3) / length(u)
  
  est = nls(g ~ G %*% c(p, p^2, s), start = list(p = 0.7, s = 1), 
            control = list(warnOnly = T))
  
  j5 = summary(est)[["coefficients"]]
  list(p = j5[1], s = j5[2], p.s = j5[3], s.s = j5[4])
}

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

# In 3 Equation -----------------------------------------------------------
library(sandwich)
ols.results <- function(model) {
  DELTA <- model$coefficients
  SE   <- sqrt(diag(vcovHC(model)))
  
  cbind(DELTA, SE)
}


# Estimation --------------------------------------------------------------

two.stage <- function(data, n, endo, xnames, y, Ph = Ph, xW = xW, W = W){
  
  if (!is.character(n)) (n <- as.character(n))
  if (!is.character(y)) (y <- as.character(y))
  y           <- paste0(y, n)
  
  work        <- data
  names(work) <- paste0(names(data), n)
  xnames      <- paste0(xnames, n) -> xnames1
  
  endo        <- paste0(endo, n)
  M           <- regress(work[, endo], Ph)
  ENDO        <- data.frame(Ph %*% M)
  names(ENDO) <- paste0(endo, "h") -> endoh
  work        <- cbind(work, ENDO)
  
  formula    <- reformulate(termlabels = c(endoh, xnames),
                            response = y, intercept = F)
  
  reg1        <- lm(formula, work)
  ols         <- ols.results(reg1)
  
  print("First Stage Tests")
  print(lm.LMtests(reg1, xW, test = "all")) #PRINT SPATIAL TESTS
  
  # moran.test(as.vector(reg1$residuals), xW)
  
  gmerror <- GMerrorsar(formula, work, xW)
  
  rtrue <- work[, y] - t(t(work[, c(endo, xnames)])) %*%
            t(t(reg1$coefficients))
  
  #Check for residual autocorrelation
  print("Residual Autocorrelation #1")
  print(moran.test(rtrue, xW, randomisation = F))
  gm            <- gmproc(rtrue,W)
#   sperr         <- c(gm$p, gm$p.s)
  
  #Transform due to spatial error, not needed for instruments
  work[, y]      <- work[, y] - gm$p * W %*% work[, y]
  work[, xnames] <- work[, xnames] - gm$p * W %*% as.matrix(work[, xnames])
  work[, endo]   <- work[, endo] - gm$p * W %*% as.matrix(work[, endo])
  M2             <- regress(work[, endo], Ph) #endogenous
  work[, endoh]  <- data.frame(Ph %*% M2)
  
  reg2           <- lm(formula, work)
  stage          <- ols.results(reg2)
  
  print("Second Stage Tests")
  print(lm.LMtests(reg2, xW, test = "all"))
  
  rtrue <- work[, y] - t(t(work[, c(endo, xnames)])) %*%
            t(t(reg2$coefficients))
  print("Residual Autocorrelation #2")
  print(moran.test(rtrue, xW, randomisation = F))
  
  work$r <- rtrue
  gm     <- data.frame(matrix(unlist(gm), nrow = 2),
                       row.names = c("p", "s"))
  
  return(list(work = work,
              ols  = ols, olsVAR = vcovHC(reg1), 
              stage = stage, stageVAR = vcovHC(reg2),
              sperr = gm
  ))
}

fgs3sls.3 <- function(est1, est2, est3,   #Data
                      endo1, endo2, endo3, #endogenous vars
                      xnames1, xnames2, xnames3, #x vars
                      bread = "yes"
){
  # FGS3SLS -----------------------------------------------------------------
  endo1  <- paste0(endo1, "1")
  endo2  <- paste0(endo2, "2")
  endo3  <- paste0(endo3, "3")
  
  xnames1<- paste0(xnames1, "1")
  xnames2<- paste0(xnames2, "2")
  xnames3<- paste0(xnames3, "3")
  
  endo1h <- paste0(endo1, "h")
  endo2h <- paste0(endo2, "h")
  endo3h <- paste0(endo3, "h")
  
  e1e2  <- t(est1$r) %*% est2$r / sqrt((nrow(est1) - ncol(est1))*
                                         (nrow(est2) - ncol(est2)))
  e1e3  <- t(est1$r) %*% est3$r / sqrt((nrow(est1) - ncol(est1))*
                                         (nrow(est3) - ncol(est3)))
  e2e3  <- t(est2$r) %*% est3$r / sqrt((nrow(est2) - ncol(est2))*
                                         (nrow(est3) - ncol(est3)))
  e1e1  <- t(est1$r) %*% est1$r / (nrow(est1) - ncol(est1))
  e2e2  <- t(est2$r) %*% est2$r / (nrow(est2) - ncol(est2))
  e3e3  <- t(est3$r) %*% est3$r / (nrow(est3) - ncol(est3))
  SIGMA <- matrix(cbind(e1e1,e1e2,e1e3,
                        e1e2,e2e2,e2e3,
                        e1e3,e2e3,e3e3),3)
  ISIG  <- qr.solve(SIGMA)
  
  ADJ   <- kronecker(ISIG,diag(nrow(est1)))
  
  #Need to finish final stage of estimation.
  Z1H   <- as.matrix(est1[, c(endo1h, xnames1)])
  Z2H   <- as.matrix(est2[, c(endo2h, xnames2)])
  Z3H   <- as.matrix(est3[, c(endo3h, xnames3)])
  
  Z1    <- as.matrix(est1[, c(endo1, xnames1)])
  Z2    <- as.matrix(est2[, c(endo2, xnames2)])
  Z3    <- as.matrix(est3[, c(endo3, xnames3)])
  
  ZH    <- cbind(kronecker(c(1,0,0), Z1H), kronecker(c(0,1,0), Z2H),
                 kronecker(c(0,0,1), Z3H))
  colnames(ZH) <- c(colnames(Z1H), colnames(Z2H), colnames(Z3H))
  
  Z     <- cbind(kronecker(c(1,0,0), Z1), kronecker(c(0,1,0), Z2),
                 kronecker(c(0,0,1), Z3))
  colnames(Z) <- c(colnames(Z1), colnames(Z2), colnames(Z3))
  
  Y      <- c(est1$y11, est2$y22, est3$y33)
  
  DELTA  <- (qr.solve(t(ZH) %*% ADJ %*% ZH)) %*% (t(ZH) %*% ADJ %*% Y)
  
  rtrue  <- Y - Z %*% DELTA
  
  #   resid  <- rtrue^2
  #   auxil  <- (qr.solve(t(ZH) %*% ADJ %*% Z)) %*% (t(ZH) %*% ADJ %*% resid)
  #   white  <- regress(resid, cbind(rep(1,length(Y)), Y, Y^2))
  
  if (bread == "yes") {
    bread  <- (qr.solve(t(ZH) %*% ADJ %*% ZH))
  } else {
    bread  <- (solve(t(ZH) %*% ADJ %*% ZH))
    bread  <- (qr(t(ZH) %*% ADJ %*% ZH)$qr)
    library(MASS)
    bread  <- ginv(t(ZH) %*% ADJ %*% ZH)
    
  }
  
  
  meat   <- t(ZH) %*% ADJ %*% diag(diag(crossprod(t(rtrue)))) %*% ADJ %*% ZH
  VAR    <- bread %*% meat %*% bread
  
  VAR    <- (qr.solve(t(ZH) %*% ADJ %*% ZH))
  SE     <- sqrt(diag(VAR))
  
  results<- table.results(cbind(DELTA, SE),
                          df = length(Y) / 2 - length(DELTA))
  return(list(results = results,
              VAR = VAR,
              r = rtrue
  ))
}