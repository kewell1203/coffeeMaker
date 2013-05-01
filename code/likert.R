likertdist <- function(theta, beta, w = -3:3) {
  pp <- exp(w * theta + beta)
  p <- pp / sum(pp)
  names(p) <- w
  return(p)
}

likert <- function(x, theta, beta, w = -3:3) {
  xi <- match(x, w)
  if (any(is.na(xi)))
    stop("Response values do not match response scale.")
  likertdist(theta, beta, w)[xi]
}

likertexpect <- function(theta, beta, w = -3:3, values = w) {
  sum(likertdist(theta, beta, w) * values)
}

likertmode <- function(theta, beta, w = -3:3, values = w) {
  w[which.max(likertdist(theta, beta, w))]
}

dark <- function(col, factor = 0.7) {
  cc <- rgb2hsv(col2rgb(col))
  cc["v",] <- cc["v",] * factor
  h <- t(cc) 
  hsv(h[,"h"], h[,"s"], h[,"v"])
}

colorRampHighLow <- function(n) {
  pal <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", 
                            "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))
  col <- pal(n + 2)[(1:n) + 1]
  return(col)  
}

plotlikert <- function(beta, w = -3:3, xlim = c(-3,3), n = 101, col = NULL, type = "l", plot.it = TRUE, ...) {
  theta <- seq(xlim[[1]], xlim[[2]], length.out=n)
  r <- sapply(theta, function(thetai) likertdist(thetai, beta, w))
  result <- as.data.frame(t(r))
  rownames(result) <- theta
  if (missing(col) || is.null(col))
    col <- dark(colorRampHighLow(ncol(result)))
  if (plot.it) {
    plot(theta, result[[1]], xlim=xlim, ylim=c(0, 1), col=col[[1]], type=type, ylab="Probability", ...)
    for(i in 2:ncol(result))
      points(theta, result[[i]], col=col[[i]], type=type, ...)
    invisible(result)
  } else 
    return(result)
}

barplotlikert <- function(beta, w = -3:3, xlim = c(-3,3), n = 25, col = NULL, plot.it = TRUE, ...) {
  result <- plotlikert(beta, w, xlim, n, plot.it = F)
  if (missing(col) || is.null(col)) 
    col <- colorRampHighLow(ncol(result))
  if (plot.it) {
    barplot(t(as.matrix(result)), col=col, ...)
    invisible(result)
  } else
    return(result)
}

likertbeta <- function(alpha) {
  -diffinv(c(-rev(alpha), alpha))
}

likertlm <- function (formula, data, subset, na.action, 
                      method = c("symmetric", "full"), alpha = NULL, beta = NULL,
                      w = -3:3, model = TRUE, x = FALSE, y = FALSE, contrasts = NULL, 
                      optim.method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"), 
                      optim.control = list(), optim.start = NULL, ...) {
  method <- match.arg(method)
  optim.method <- match.arg(optim.method)
  ret.x <- x
  ret.y <- y
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action", "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  if (method == "model.frame") 
    return(mf)
  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")
  if (is.matrix(y))
    stop("Only one response variable allowed.")
  if (is.empty.model(mt)) {
    x <- NULL
    z <- likert.fit(y, method=method, alpha=alpha, beta=beta, w=w, 
                    optim.method=optim.method, optim.control=optim.control, optim.start=optim.start, ...)
    z$coefficients = numeric()
  } else {
    x <- model.matrix(mt, mf, contrasts)
    z <- likertlm.fit(x, y, method=method, alpha=alpha, beta=beta, w=w, 
                      optim.method=optim.method, optim.control=optim.control, optim.start=optim.start, ...)
  }
  class(z) <- "likertlm"
  z$na.action <- attr(mf, "na.action")
  z$contrasts <- attr(x, "contrasts")
  z$xlevels <- .getXlevels(mt, mf)
  z$call <- cl
  z$method <- method
  z$terms <- mt
  if (model) 
    z$model <- mf
  if (ret.x) 
    z$x <- x
  if (ret.y) 
    z$y <- y
  z$w <- w
  z
}

likertlm.fit <- function(x, y, method = c("symmetric", "full"), alpha = NULL, beta = NULL, w = -3:3, 
                         optim.method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"), 
                         optim.control = list(), optim.start = NULL, ...) {
  method <- match.arg(method)
  optim.method <- match.arg(optim.method)
  if (!is.null(alpha) && method != "symmetric")
    stop("A predefined alpha can only be used with method 'symmetric'.")
  if (!is.null(beta) && method != "full")
    stop("A predefined beta can only be used with method 'full'.")
  yi <- match(y, w)
  if (any(is.na(yi)))
    stop("Response values do not match response scale.")
  loglikelihood <- function(coefficients, beta) {
    theta <- as.vector(x %*% coefficients)
    v <- cbind(theta, yi)
    p <- apply(v, 1, function(v) likertdist(v[1], beta, w)[v[2]])
    sum(-log(p))
  }
  nc <- NCOL(x)
  nb <- length(w)
  na <- (nb - 1) %/% 2
  if (method == "symmetric" && 2 * na != nb-1)
    stop("Only odd number of levels allowed for 'symmetric' method.")
  if (!is.null(alpha) || !is.null(beta)) {
    if (!is.null(alpha))
      beta <- likertbeta(alpha)
    optf <- function(v) {
      coefficients <- v[1:nc]
      loglikelihood(coefficients, beta)
    }
    if (missing(optim.start) || is.null(optim.start))
      start <- c(rep(0, nc))
    else
      start <- optim.start
    o <- optim(start, optf, method=optim.method, control=optim.control)
    coefficients <- o$par[1:nc]
  } else if (method == "symmetric") {
    optf <- function(v) {
      coefficients <- v[1:nc]
      alpha <- v[(nc+1):(nc+na)]
      beta <- likertbeta(alpha)
      loglikelihood(coefficients, beta)
    }
    if (missing(optim.start) || is.null(optim.start))
      start <- c(rep(0, nc), rep(1, na))
    else
      start <- optim.start
    o <- optim(start, optf, method=optim.method, control=optim.control)
    coefficients <- o$par[1:nc]
    alpha <- o$par[(nc+1):(nc+na)]
    beta <- likertbeta(alpha)
  } else {
    optf <- function(v) {
      coefficients <- v[1:nc]
      beta <- c(0, v[(nc+1):(nc+nb-1)])
      loglikelihood(coefficients, beta)
    }
    if (missing(optim.start) || is.null(optim.start))
      start <- c(rep(0, nc), 1:(nb-1))
    else
      start <- optim.start
    o <- optim(start, optf, method=optim.method, control=optim.control)
    coefficients <- o$par[1:nc]
    beta <- c(0, o$par[(nc+1):(nc+nb-1)])
  }
  names(coefficients) <- colnames(x)
  names(beta) <- w
  if (is.null(alpha))
    list(coefficients = coefficients, beta = beta, likelihood = o$value)
  else {
    names(alpha) <- w[(na+2):nb]  
    list(coefficients = coefficients, alpha = alpha, beta = beta, likelihood = o$value)
  }
}

likert.fit <- function(y, method = c("symmetric", "full"), alpha = NULL, beta = NULL, w = -3:3, 
                       optim.method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"), 
                       optim.control = list(), optim.start = NULL, ...) {
  method <- match.arg(method)
  optim.method <- match.arg(optim.method)
  if (!is.null(alpha) && method != "symmetric")
    stop("A predefined alpha can only be used with method 'symmetric'.")
  if (!is.null(beta) && method != "full")
    stop("A predefined beta can only be used with method 'full'.")
  yi <- match(y, w)
  if (any(is.na(yi)))
    stop("Response values do not match response scale.")
  loglikelihood <- function(beta) {
    l <- likertdist(0, beta, w)
    p <- l[yi]
    sum(-log(p))
  }
  nb <- length(w)
  na <- (nb - 1) %/% 2
  if (method == "symmetric" && 2 * na != nb-1)
    stop("Only odd number of levels allowed for 'symmetric' method.")
  if (!is.null(alpha) || !is.null(beta)) {
    if (!is.null(alpha))
      beta <- likertbeta(alpha)
  } else if (method == "symmetric") {
    optf <- function(alpha) {
      beta <- likertbeta(alpha)
      loglikelihood(beta)
    }
    if (missing(optim.start) || is.null(optim.start))
      start <- rep(1, na)
    else
      start <- optim.start
    o <- optim(start, optf, method=optim.method, control=optim.control)
    alpha <- o$par
    beta <- likertbeta(alpha)
  } else {
    optf <- function(v) {
      beta <- c(0, v)
      loglikelihood(beta)
    }
    if (missing(optim.start) || is.null(optim.start))
      start <- 1:(nb-1)
    else
      start <- optim.start
    o <- optim(start, optf, method=optim.method, control=optim.control)
    beta <- c(0, o$par)
  }
  names(beta) <- w
  if (is.null(alpha))
    list(beta = beta, likelihood = o$value)
  else {
    names(alpha) <- w[(na+2):nb]  
    list(alpha = alpha, beta = beta, likelihood = o$value)
  }
}

print.likertlm <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  if (length(coef(x))) {
    cat("Coefficients:\n")
    print.default(format(coef(x), digits = digits), print.gap = 2, quote = FALSE)
  }
  else cat("No coefficients\n")
  cat("\n")
  if (is.null(x$alpha)) {
    cat("beta:\n")
    print.default(format(x$beta, digits = digits), print.gap = 2, quote = FALSE)
  } else {
    cat("alpha:\n")
    print.default(format(x$alpha, digits = digits), print.gap = 2, quote = FALSE)
  }
  cat("\n")
  invisible(x)
}

theta.likertlm <- function (object, newdata, na.action = na.pass, ...) {
  tt <- terms(object)
  if (!inherits(object, "likertlm")) 
    warning("calling theta.likertlm(<fake-likertlm-object>) ...")
  if (missing(newdata) || is.null(newdata)) 
    X <- model.matrix(object$terms, object$model)
  else {
    Terms <- delete.response(tt)
    m <- model.frame(Terms, newdata, na.action = na.action, xlev = object$xlevels)
    if (!is.null(cl <- attr(Terms, "dataClasses"))) 
      .checkMFClasses(cl, m)
    X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
  }
  coef <- object$coefficients
  drop(X %*% coef)
}

likertlmdist <- function(object, newdata, na.action = na.pass, ...) {
  theta <- theta.likertlm(object, newdata, na.action = na.action, ...)
  t(sapply(theta, function(theta) likertdist(theta, object$beta, object$w)))
}

likertlmexpect <- function(object, newdata, na.action = na.pass, values = NULL, ...) {
  theta <- theta.likertlm(object, newdata, na.action = na.action, ...)
  if (missing(values) || is.null(values))
    sapply(theta, function(theta) likertexpect(theta, object$beta, object$w))
  else
    sapply(theta, function(theta) likertexpect(theta, object$beta, object$w, values = values))
}

likertlmmode <- function(object, newdata, na.action = na.pass, ...) {
  theta <- theta.likertlm(object, newdata, na.action = na.action, ...)
  sapply(theta, function(theta) likertmode(theta, object$beta, object$w))
}

likertlmprob <- function(object, newdata, na.action = na.pass, ...) {
  if (missing(newdata) || is.null(newdata)) 
    resp <- model.response(object$model)
  else
    resp <- model.response(model.frame(object$terms, newdata, na.action = na.action, xlev = object$xlevels))
  theta <- theta.likertlm(object, newdata, na.action = na.action)
  p <- Vectorize(function(x, theta) if (is.na(x)) NA else likert(x, theta, object$beta, object$w))(resp, theta)
  names(p) <- names(theta)
  p
}

likelihood.likertlm <- function(object, newdata, na.action = na.pass, ...) {
  p <- likertlmprob(object, newdata, na.action = na.action, ...)
  -sum(log(p))
}

summary.likertlm <- function(object) {
  z <- object
  class(z) <- c("summary.likertlm", "likertlm")
  z
}

print.summary.likertlm <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  print.likertlm(x, digits, ...)
  cat("Log-Likelihood: ", format(x$likelihood, digits = digits), "\n")
  cat("\n")
  invisible(x)
}

comparelikertlm <- function(object, olddata, newdata, na.action = na.pass, ...) {
  if (missing(newdata) || is.null(newdata)) {
    newdata <- as.data.frame(matrix(, 1, 0))
  }
  m <- likertlmdist(object, newdata, na.action, ...)
  if (is.null(rownames(m))) 
    rownames(m) <- if (NROW(m) == 1) "dist" else paste0("dist", seq_len(NROW(m)))
  else
    rownames(m) <- paste0("dist", rownames(m))
  if (!missing(olddata) && !is.null(olddata)) {
    om <- model.frame(object$terms, olddata, na.action = na.action, xlev = object$xlevels)
    ore <- model.response(om)
    w <- object$w
    freq <- sapply(w, function(x) sum(ore == x, na.rm=T))
    freq <- freq / sum(freq)
    freq <- matrix(freq, 1, length(w))
    rownames(freq) <- "freq"
    m <- rbind(m, freq)
  }
  m  
}

barplotcomparelikertlm <- function(object, olddata, newdata, na.action = na.pass, 
                                   col = c("lavender", "cornsilk"), legend = T, ...) {
  m <- comparelikertlm(object, olddata, newdata, na.action)
  barplot(m, beside=T, col=col, ...)
  if (legend) 
    legend("topright", rownames(m), fill=col)
  invisible(m)
}
  
#####

plotkaffeelikertexpect <- function(x, data = NULL, legend = T, col = NULL, step = 100, ylab = NULL, values = NULL, ...) {
  require(plyr)
  if (is.formula(x))
    object <- likertlm(x, data)
  else
    object <- x
  func <- function(Mehlmenge, Mahlgrad, Nasspressen) 
    likertlmexpect(object, data.frame(Mehlmenge=Mehlmenge, Mahlgrad=Mahlgrad, Nasspressen=Nasspressen), values = values)
  if (missing(ylab))
    ylab <- names(object$model)[attr(object$terms, "response")]
  plotkaffeefunc(func, legend=legend, col=col, step=step, ylab=ylab, ...)
  invisible(object)
}

##### Beispiel:

# plotkaffeelikertexpect(Geschmack ~ Mehlmenge * Mahlgrad + I(Mehlmenge^2) + Nasspressen, kaffee.main)
# 
# #### Beispiel:
# 
# model.Suesse <- likertlm(Suesse ~ Mehlmenge + Mahlgrad + Nasspressen, kaffee.main0)
# model.Saeure <- likertlm(Saeure ~ Mehlmenge + Mahlgrad + Nasspressen, kaffee.main0)
# model.Bitterkeit <- likertlm(Bitterkeit ~ Mehlmenge + Mahlgrad + Nasspressen, kaffee.main0)
# model.Koerper <- likertlm(Koerper ~ Mehlmenge + Mahlgrad + Nasspressen, kaffee.main0)
# model.Geschmack <- likertlm(Geschmack ~ Mehlmenge + Mahlgrad + Nasspressen, kaffee.main0)
# 
# kaffeefunc <- function(Mehlmenge, Mahlgrad, Nasspressen) {
#   values <- c(0, 0, .5, 1, .5, 0, 0)
#   su <- likertlmexpect(model.Suesse, data.frame(Mehlmenge=Mehlmenge, Mahlgrad=Mahlgrad, Nasspressen=Nasspressen), values = values)  
#   sa <- likertlmexpect(model.Saeure, data.frame(Mehlmenge=Mehlmenge, Mahlgrad=Mahlgrad, Nasspressen=Nasspressen), values = values)  
#   bi <- likertlmexpect(model.Bitterkeit, data.frame(Mehlmenge=Mehlmenge, Mahlgrad=Mahlgrad, Nasspressen=Nasspressen), values = values)  
#   ko <- likertlmexpect(model.Koerper, data.frame(Mehlmenge=Mehlmenge, Mahlgrad=Mahlgrad, Nasspressen=Nasspressen), values = values)  
#   - log(su) - log(sa) - log(bi) - log(ko)
# }
# 
# plotkaffeefunc(kaffeefunc)

##### Beispiel:

# model.Suesse <- likertlm(Suesse ~ Mehlmenge + Mahlgrad + Nasspressen + Wassermenge, kaffee.all0, alpha = c(-.5, 1, 2), optim.method = "BFGS")
# model.Saeure <- likertlm(Saeure ~ Mehlmenge + Mahlgrad + Nasspressen + Wassermenge, kaffee.all0, alpha = c(-.5, 1, 2), optim.method = "BFGS")
# model.Bitterkeit <- likertlm(Bitterkeit ~ Mehlmenge + Mahlgrad + Nasspressen + Wassermenge, kaffee.all0, alpha = c(-.5, 1, 2), optim.method = "BFGS")
# model.Koerper <- likertlm(Koerper ~ Mehlmenge + Mahlgrad + Nasspressen + Wassermenge, kaffee.all0, alpha = c(-.5, 1, 2), optim.method = "BFGS")
# model.Geschmack <- likertlm(Geschmack ~ Mehlmenge + Mahlgrad + Nasspressen + Wassermenge, kaffee.all0, alpha = c(-.5, 1, 2), optim.method = "BFGS")
# 
# kaffeefunc <- function(Mehlmenge, Mahlgrad, Nasspressen, Wassermenge = 25, values = c(0, 0, 0, 1, 0, 0, 0)) {
#   su <- likertlmexpect(model.Suesse, data.frame(Mehlmenge=Mehlmenge, Mahlgrad=Mahlgrad, Nasspressen=Nasspressen, Wassermenge = Wassermenge), values = values)  
#   sa <- likertlmexpect(model.Saeure, data.frame(Mehlmenge=Mehlmenge, Mahlgrad=Mahlgrad, Nasspressen=Nasspressen, Wassermenge = Wassermenge), values = values)  
#   bi <- likertlmexpect(model.Bitterkeit, data.frame(Mehlmenge=Mehlmenge, Mahlgrad=Mahlgrad, Nasspressen=Nasspressen, Wassermenge = Wassermenge), values = values)  
#   ko <- likertlmexpect(model.Koerper, data.frame(Mehlmenge=Mehlmenge, Mahlgrad=Mahlgrad, Nasspressen=Nasspressen, Wassermenge = Wassermenge), values = values)  
#   - log(su) - log(sa) - log(bi) - log(ko)
# }
# 
# plotkaffeefunc(kaffeefunc)
