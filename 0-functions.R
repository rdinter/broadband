#Robert Dinterman, NCSU Economics PhD Student
require(Matrix, quietly = T)
require(sphet, quietly = T)

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

adiag <- function (..., pad = as.integer(0), do.dimnames = TRUE){
  args <- list(...)
  if (length(args) == 1) {
    return(args[[1]])
  }
  if (length(args) > 2) {
    jj <- do.call("Recall", c(args[-1], list(pad = pad)))
    return(do.call("Recall", c(list(args[[1]]), list(jj), 
                               list(pad = pad))))
  }
  a <- args[[1]]
  b <- args[[2]]
  if (is.null(b)) {
    return(a)
  }
  if (is.null(dim(a)) & is.null(dim(b))) {
    dim(a) <- rep(1, 2)
    dim(b) <- rep(1, 2)
  }
  if (is.null(dim(a)) & length(a) == 1) {
    dim(a) <- rep(1, length(dim(b)))
  }
  if (is.null(dim(b)) & length(b) == 1) {
    dim(b) <- rep(1, length(dim(a)))
  }
  if (length(dim.a <- dim(a)) != length(dim.b <- dim(b))) {
    stop("a and b must have identical number of dimensions")
  }
  s <- array(pad, dim.a + dim.b)
  s <- do.call("[<-", c(list(s), lapply(dim.a, seq_len), list(a)))
  ind <- lapply(seq(dim.b), function(i) seq_len(dim.b[[i]]) + 
                  dim.a[[i]])
  out <- do.call("[<-", c(list(s), ind, list(b)))
  n.a <- dimnames(a)
  n.b <- dimnames(b)
  if (do.dimnames & !is.null(n.a) & !is.null(n.b)) {
    dimnames(out) <- mapply(c, n.a, n.b, SIMPLIFY = FALSE)
    names(dimnames(out)) <- names(n.a)
  }
  return(out)
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

gmproc <- function(r,W, p = 0.7){
  s = var(r)
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
  
  est = nls(g ~ G %*% c(p, p^2, s), start = list(p = p, s = s), 
            control = list(warnOnly = T))
  
  j5 = summary(est)[["coefficients"]]
  list(p = j5[1], s = j5[2], p.s = j5[3], s.s = j5[4])
}

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

ols.results <- function(model) {
  require(sandwich, quietly = T)
  DELTA <- model$coefficients
  SE   <- sqrt(diag(vcovHC(model)))
  
  cbind(DELTA, SE)
}

#gg_het from sphet
#psirhorho_het

# stage.results <- function(model, Ph, gm, W) {
#   e  = model$residuals
#   df = nrow(model$model) - ncol(model$model)
#   s2 = crossprod(e) / df
#   omega = as.numeric(e^2)
#   
#   Zp <- Hmat %*% bz
#   ZpZp <- crossprod(Zp)
#   ZpZpi <- solve(ZpZp)
#   
#   step1 <- sphet:::gg_het(W, model$residuals, nrow(W))
#   #Hmat is the Ph
#   rhost <- sphet:::psirhorho_het(rho, residuals, Hmat = Ph,
#                                  Zmat, Ws, step1.c)
#   #http://econweb.umd.edu/~prucha/STATPROG/OLS/desols.pdf
#   R = diag(nrow(W)) - gm$p*W #LEFT OFF
# }

# Estimation --------------------------------------------------------------

two.stage <- function(data, n, endo, xnames, y, Ph = Ph, xW = xW, W = W){
  require(lmtest, quietly = T)
  require(sandwich, quietly = T)
  require(spdep, quietly = T)
  
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
  
  gmerror <- GMerrorsar(formula, work, xW, returnHcov = T)
  
  rtrue <- work[, y] - t(t(work[, c(endo, xnames)])) %*%
    t(t(reg1$coefficients))
  
  #Check for residual autocorrelation
  m1eq <-moran.test(rtrue, xW, randomisation = F, alternative = "two.sided")
  print("Residual Autocorrelation #1")
  print(m1eq)
  #   print(moran.mc(rtrue, xW, 100, alternative = "two.sided"))
  gm            <- gmproc(rtrue,W)
  
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
  m2eq  <- moran.test(rtrue, xW, randomisation = F, alternative = "two.sided")
  print("Residual Autocorrelation #2")
  print(m2eq)
  
  work$r <- rtrue
  gm     <- data.frame(matrix(unlist(gm), nrow = 2),
                       row.names = c("p", "s"))
  
  return(list(work = work,
              ols  = ols, olsVAR = vcovHC(reg1), 
              stage = stage, stageVAR = vcovHC(reg2),
              sperr = gm, moran1 = m1eq, moran2 = m2eq,
              GMerror = gmerror
  ))
}

fgs3sls.3 <- function(est1, est2, est3,   #Data
                      endo1, endo2, endo3, #endogenous vars
                      xnames1, xnames2, xnames3, #x vars
                      robust = F
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
              r = rtrue
  ))
}

fgs3sls.2 <- function(est1, est2,       #Data
                      endo1, endo2,     #endogenous vars
                      xnames1, xnames2, #x vars
                      robust = F
){
  # FGS3SLS -----------------------------------------------------------------
  endo1  <- paste0(endo1, "1")
  endo2  <- paste0(endo2, "2")
  
  xnames1<- paste0(xnames1, "1")
  xnames2<- paste0(xnames2, "2")
  
  endo1h <- paste0(endo1, "h")
  endo2h <- paste0(endo2, "h")
  
  e1e2   <- t(est1$r) %*% est2$r / sqrt((nrow(est1) - ncol(est1))*
                                          (nrow(est2) - ncol(est2)))
  e1e1   <- t(est1$r) %*% est1$r / (nrow(est1) - ncol(est1))
  e2e2   <- t(est2$r) %*% est2$r / (nrow(est2) - ncol(est2))
  SIGMA  <- matrix(cbind(e1e1, e1e2, e1e2, e2e2), 2)
  ISIG   <- qr.solve(SIGMA)
  
  ADJ    <- kronecker(ISIG,diag(nrow(est1)))
  
  #Need to finish final stage of estimation.
  Z1H   <- as.matrix(est1[, c(endo1h, xnames1)])
  Z2H   <- as.matrix(est2[, c(endo2h, xnames2)])
  
  Z1    <- as.matrix(est1[, c(endo1, xnames1)])
  Z2    <- as.matrix(est2[, c(endo2, xnames2)])
  
  ZH    <- cbind(kronecker(c(1,0), Z1H), kronecker(c(0,1), Z2H))
  colnames(ZH) <- c(colnames(Z1H), colnames(Z2H))
  
  Z     <- cbind(kronecker(c(1,0), Z1), kronecker(c(0,1), Z2))
  colnames(Z) <- c(colnames(Z1), colnames(Z2))
  
  Y      <- c(est1$y11, est2$y22)
  
  DELTA  <- (qr.solve(t(ZH) %*% ADJ %*% ZH)) %*% (t(ZH) %*% ADJ %*% Y)
  
  rtrue  <- Y - Z %*% DELTA
  
  #   resid  <- rtrue^2
  #   auxil  <- (qr.solve(t(ZH) %*% ADJ %*% Z)) %*% (t(ZH) %*% ADJ %*% resid)
  #   white  <- regress(resid, cbind(rep(1,length(Y)), Y, Y^2))
  
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
              r = rtrue
  ))
}

two.stage.simple <- function(data, n, endo, xnames, y,
                             Ph = Ph, xW = xW, W = W){
  require(lmtest, quietly = T)
  require(sandwich, quietly = T)
  require(spdep, quietly = T)
  
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
  rtrue <- work[, y] - t(t(work[, c(endo, xnames)])) %*%
    t(t(reg1$coefficients))
  gm            <- gmproc(rtrue,W)
  
  #Transform due to spatial error, not needed for instruments
  work[, y]      <- work[, y] - gm$p * W %*% work[, y]
  work[, xnames] <- work[, xnames] - gm$p * W %*% as.matrix(work[, xnames])
  work[, endo]   <- work[, endo] - gm$p * W %*% as.matrix(work[, endo])
  M2             <- regress(work[, endo], Ph) #endogenous
  work[, endoh]  <- data.frame(Ph %*% M2)
  reg2           <- lm(formula, work)
  
  rtrue <- work[, y] - t(t(work[, c(endo, xnames)])) %*%
    t(t(reg2$coefficients))
  
  work$r <- rtrue
  
  return(list(work = work))
}