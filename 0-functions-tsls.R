# Copyright 2004 by Luc Anselin
# two stage least squares
# Usage:
#    tsls(y,yend,X,Zinst,robust=FALSE)
# Arguments:
#    y: dependent variable as vector
#    yend: endogenous variables as vector or matrix (using cbind)
#    X: matrix of exogenous variables, including constant
#    Zinst: matrix of instruments (using cbind)
#    robust: flag for heteroskedastic robust estimator
# Details:
#    standard two stage least squares, using explicit two stages
#    uses degrees of freedom in computation of residual variance (N-K not N)
#    calls htsls when robust is TRUE
# Value:
# a list with results:
#   coefficients: coefficient estimates
#   se: (asymptotic) standard error of estimates
#   t:  value of asymptotic t-test statistic
#   p:  probability of t-test (tail, two-sided)
#   var: coefficient variance matrix
#   s2: residual variance (using degrees of freedom N-K)
#   residuals: observed y - predicted y, to be used in diagnostics

tsls <- function(y,yend,X,Zinst,robust=FALSE, HC="HC0", legacy=FALSE) {
  #  colnames(X) <- c("CONSTANT",colnames(X)[2:ncol(X)])
  Q <- cbind(X,Zinst)
  Z <- cbind(yend,X)
  df <- nrow(Z) - ncol(Z)
  #	QQ <- crossprod(Q,Q)
  Qye <- crossprod(Q,yend)
  Qr <- qr(Q)
  bz <- chol2inv(Qr$qr)%*% Qye
  #	bz <- solve(QQ,Qye)
  yendp <- Q %*% bz
  Zp <- cbind(yendp,X)
  Qr <- qr(Zp)
  #	ZpZp <- crossprod(Zp,Zp)
  #	ZpZpi <- solve(ZpZp)
  ZpZpi <- chol2inv(Qr$qr)
  Zpy <- crossprod(Zp,y)
  biv <- ZpZpi %*% Zpy
  #	biv <- crossprod(ZpZpi,Zpy)
  yp <- Z %*% biv
  biv <- biv[,1,drop=TRUE]
  names(biv) <- colnames(Zp)
  e <- y - yp
  if (robust) {
    if (legacy) {		
      result <- htsls(y,Z,Q,e)
    } else {
      sse <- c(crossprod(e,e))
      if (HC == "HC0") omega <- as.numeric(e^2)
      else if (HC == "HC1")
        omega <- (nrow(X)/df) * as.numeric(e^2)
      else stop("invalid HC choice")
      ZoZ<-crossprod(Zp,(Zp*omega))
      varb<-ZpZpi%*%ZoZ%*%ZpZpi
      
      result <- list(coefficients=biv,
                     var=varb,
                     sse=sse,
                     residuals=c(e),
                     df=df)
      
    }
  } else {	
    sse <- c(crossprod(e,e))
    s2 <- sse / df
    varb <- ZpZpi * s2
    #	    sebiv <- sqrt(diag(varb))
    #	    tbiv <- biv / sebiv
    #	    pbiv <- pnorm(abs(tbiv),lower.tail=FALSE) * 2
    result <- list(coefficients=biv,
                   #		  se=sebiv,t=tbiv,p=pbiv,
                   var=varb,
                   sse=sse,
                   residuals=c(e),
                   df=df)
  }
  result
}
