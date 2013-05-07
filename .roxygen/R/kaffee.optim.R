#'@export
kaffee.optim <- function (Wassermenge, Mahlgrad = NULL, Nasspressen = NULL, Mehlmenge.min = 40, Mehlmenge.max = 100, ...) {
  if (missing(Nasspressen) || is.null(Nasspressen)) 
    Nasspressen <- c(0, 20)
  f <- function(Nasspressen) kaffee.opt3(Wassermenge, Mahlgrad, Nasspressen, Mehlmenge.min, Mehlmenge.max, ...)
  return(maxval(Nasspressen, f, function(v) v[["Quality"]]))
}
kaffee.opt1 <- function (Wassermenge, Nasspressen, Mehlmenge.min = 40, Mehlmenge.max = 100, ...) {
  result.opt <- optim(c(50, 2), 
                      function(param) -kaffee.qual(param[2], param[1], Nasspressen, Wassermenge, ...), 
                      lower=c(Mehlmenge.min, 1), upper=c(Mehlmenge.max, 3.5), method="L-BFGS-B")
  result <- c(
    Mahlgrad = result.opt$par[2],
    Mehlmenge = result.opt$par[1],
    Nasspressen = Nasspressen,
    Wassermenge = Wassermenge,
    Quality = -result.opt$value)
  return(result)
}
kaffee.opt2 <- function (Wassermenge, Mahlgrad, Nasspressen, Mehlmenge.min = 40, Mehlmenge.max = 100, ...) {
  result.opt <- optim(50, 
                      function(param) -kaffee.qual(Mahlgrad, param[1], Nasspressen, Wassermenge, ...), 
                      lower=Mehlmenge.min, upper=Mehlmenge.max, method="L-BFGS-B")
  result <- c(
    Mahlgrad = Mahlgrad,
    Mehlmenge = result.opt$par,
    Nasspressen = Nasspressen,
    Wassermenge = Wassermenge,
    Quality = -result.opt$value)
  return(result)
}
kaffee.opt3 <- function (Wassermenge, Mahlgrad = NULL, Nasspressen, Mehlmenge.min = 40, Mehlmenge.max = 100, ...) {
  if (missing(Mahlgrad) || is.null(Mahlgrad)) 
    return(kaffee.opt1(Wassermenge, Nasspressen, Mehlmenge.min, Mehlmenge.max, ...))
  else {
    f <- function(Mahlgrad) kaffee.opt2(Wassermenge, Mahlgrad, Nasspressen, Mehlmenge.min, Mehlmenge.max, ...)
    return(maxval(Mahlgrad, f, function(v) v[["Quality"]]))
  }
}
