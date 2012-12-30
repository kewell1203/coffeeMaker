mse <- function(model, data) {
  if (missing(data) || is.null(data))
    data <- model$model
  p <- predict(model, data)
  o <- model.response(model.frame(model$terms, data))
  o <- o[names(p)]
  r <- na.omit(o - p)
  sum(r^2)/length(r)
}

recalculate <- function(model, data) {
  if (missing(data) || is.null(data))
    data <- model$model
  call <- model$call
  call[["data"]] <- as.name("data")
  eval(call)
}

crossvalid <- function (x, data = NULL, slices, random, newdata = NULL) {
  require(plyr)
  if (is.formula(x) && !is.null(data)) {
    call <- as.call(list(as.name("lm"), formula = deparse(substitute(x)), data = as.name("data")))
    model <- eval(call)
  }
  else
    model <- x
  data <- model$model
  if (missing(slices) || is.null(slices)) {
    slices <- NROW(data)
    random <- FALSE    
  } else if (missing(random) || is.null(random))
    random <- TRUE
  size <- nrow(data)
  isel <- if (random) sample.int(size) else seq(1:size)
  ssel <- (((seq(1:size) - 1) * slices) %/% size) + 1
  msevec <- sapply(1:slices, function(s) {
    i <- isel[ssel == s]
    modd <- data[-i,]
    vald <- data[i,]
    vmodel <- recalculate(model, modd)
    mse(vmodel, vald)
  })
  msemean <- mean(msevec, na.rm=T)
  result <- list(mse = msemean, rmse = sqrt(msemean), msevec = msevec, model = model)
  if (!is.null(newdata))
    result$newmse <- mse(model, newdata)
  class(result) <- "crossvalid"
  return(result)
}

print.crossvalid <- function (x, ...) {
  cat("Crossvalidation of model\n")
  cat("   formula: ", deparse(x$model$terms), "\n")
  cat("crossvalidation MSE : ", x$mse, "\n")
  cat("   square-root      : ", x$rmse, "\n")
  if (!is.null(x$newmse))
    cat("new data MSE        :", x$newmse, "\n")
  invisible(x)	
}
