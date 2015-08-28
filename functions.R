################################################################################
### GOODNESS OF FIT FOR DOSE-RESPONSE META-ANALYSIS OF BINARY OUTCOMES
###       Andrea Discacciati, Alessio Crippa, Nicola Orsini
###                       andrea.discacciati@ki.se
###
###                    R functions used in the examples
################################################################################

## SUMMARY FUNCTION THAT FORCES MSE = 1 IN LINEAR REGRESSION
## (Similar to option mse1 of regress command in Stata)
summary.dr <- function (object, correlation = FALSE, symbolic.cor = FALSE, 
          ...) 
{
  z <- object
  p <- z$rank
  rdf <- z$df.residual
  if (p == 0) {
    r <- z$residuals
    n <- length(r)
    w <- z$weights
    if (is.null(w)) {
      rss <- sum(r^2)
    }
    else {
      rss <- sum(w * r^2)
      r <- sqrt(w) * r
    }
    resvar <- rss/rdf
    ans <- z[c("call", "terms", if (!is.null(z$weights)) "weights")]
    class(ans) <- "summary.lm"
    ans$aliased <- is.na(coef(object))
    ans$residuals <- r
    ans$df <- c(0L, n, length(ans$aliased))
    ans$coefficients <- matrix(NA, 0L, 4L)
    dimnames(ans$coefficients) <- list(NULL, c("Estimate", 
                                               "Std. Error", "t value", "Pr(>|t|)"))
    ans$sigma <- sqrt(resvar)
    ans$r.squared <- ans$adj.r.squared <- 0
    return(ans)
  }
  if (is.null(z$terms)) 
    stop("invalid 'lm' object:  no 'terms' component")
  if (!inherits(object, "lm")) 
    warning("calling summary.lm(<fake-lm-object>) ...")
  Qr <- stats:::qr.lm(object)
  n <- NROW(Qr$qr)
  if (is.na(z$df.residual) || n - p != z$df.residual) 
    warning("residual degrees of freedom in object suggest this is not an \"lm\" fit")
  r <- z$residuals
  f <- z$fitted.values
  w <- z$weights
  if (is.null(w)) {
    mss <- if (attr(z$terms, "intercept")) 
      sum((f - mean(f))^2)
    else sum(f^2)
    rss <- sum(r^2)
  }
  else {
    mss <- if (attr(z$terms, "intercept")) {
      m <- sum(w * f/sum(w))
      sum(w * (f - m)^2)
    }
    else sum(w * f^2)
    rss <- sum(w * r^2)
    r <- sqrt(w) * r
  }
  resvar <- rss/rdf
  if (is.finite(resvar) && resvar < (mean(f)^2 + var(f)) * 
        1e-30) 
    warning("essentially perfect fit: summary may be unreliable")
  p1 <- 1L:p
  R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
  se <- sqrt(diag(R))
  est <- z$coefficients[Qr$pivot[p1]]
  zval <- est/se
  ans <- z[c("call", "terms", if (!is.null(z$weights)) "weights")]
  ans$residuals <- r
  ans$coefficients <- cbind(est, se, zval, 2 * pnorm(abs(zval), 
                                                 lower.tail = FALSE))
  dimnames(ans$coefficients) <- list(names(z$coefficients)[Qr$pivot[p1]], 
                                     c("Estimate", "Std. Error", "z value", "Pr(>|z|)"))
  ans$aliased <- is.na(coef(object))
  ans$sigma <- sqrt(resvar)
  ans$df <- c(p, rdf, NCOL(Qr$qr))
  if (p != attr(z$terms, "intercept")) {
    df.int <- if (attr(z$terms, "intercept")) 
      1L
    else 0L
    ans$r.squared <- mss/(mss + rss)
    ans$adj.r.squared <- 1 - (1 - ans$r.squared) * ((n - 
                                                       df.int)/rdf)
    ans$fstatistic <- c(value = (mss/(p - df.int))/resvar, 
                        numdf = p - df.int, dendf = rdf)
  }
  else ans$r.squared <- ans$adj.r.squared <- 0
  ans$cov.unscaled <- R
  dimnames(ans$cov.unscaled) <- dimnames(ans$coefficients)[c(1, 
                                                             1)]
  if (correlation) {
    ans$correlation <- (R * resvar)/outer(se, se)
    dimnames(ans$correlation) <- dimnames(ans$cov.unscaled)
    ans$symbolic.cor <- symbolic.cor
  }
  if (!is.null(z$na.action)) 
    ans$na.action <- z$na.action
  class(ans) <- "summary.lm"
  ans
}

vcov.dr <- function (object, ...) 
{
  so <- summary.dr(object)
  so$cov.unscaled
}

## FUNCTION TO GET THE DATASET WITH DECORRELATED DATA, R^2 and R^2_adj
## (Decorrelated log(RR) and Design Matrix via Cholesky factorization)
gof <- function(object){
  mf <- model.frame(object)
  mfmX <- object$model
  y <- as.matrix(model.response(mfmX, "numeric"))
  id <- object$id
  nay <- is.na(y)
  X <- model.matrix(attr(mfmX, "terms"), data = mfmX)
  X <- X[, -grep("(Intercept)", colnames(X)), drop = FALSE]
  Slist <- object$Slist
  v <- object$v
  dim <- object$dim
  vlist <- lapply(unique(id), function(j) cbind(v[id == j]))
  ylist <- lapply(unique(id), function(j) 
    y[id == j, , drop = FALSE][!nay[j, ], , drop = FALSE])
  Xlist <- lapply(unique(id),
                  function(j) 
                    X[id == j, , drop = FALSE][!nay[j, ], , drop = FALSE])
  nalist <- split(nay, id)
  if (object$center){
    Xlist <- mapply(function(X, v){
      scale(X, X[v==0, ], scale = FALSE)
    }, Xlist, vlist, SIMPLIFY = FALSE)
    X <- do.call("rbind", Xlist)
  }
  Psi <- diag(0, dim$q)
  Xlist <- mapply(function(X, v) X[v!=0, , drop = FALSE], 
                  Xlist, vlist, SIMPLIFY = FALSE)
  ylist <- mapply(function(y, v) y[v!=0, , drop = FALSE], 
                  ylist, vlist, SIMPLIFY = FALSE)
  nalist <- mapply(function(na, v) na[v!=0], nalist, vlist, SIMPLIFY = FALSE)   
  gls <- dosresmeta:::glsfit(Xlist, Zlist = lapply(Xlist, function(z) z[, 1:dim$q, drop = FALSE]), 
                             ylist, Slist, nalist, Psi, onlycoef = FALSE)
  tdata <- data.frame(gls$invtUy, gls$invtUX)
  names(tdata)[1] <- names(mf)[1]
  tlm <- summary(lm(gls$invtUy ~ 0 + gls$invtUX))
  list(tdata = tdata, R2 = tlm$r.squared, R2adj = tlm$adj.r.squared)   
}
