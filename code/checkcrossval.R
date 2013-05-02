checkcrossval <- 
  function (formula, data, newdata = NULL, slices = nrow(data), 
            random = (slices != nrow(data)), details = TRUE, slices2 = NULL) 
  {
    msefunc <- function(model, localdata) {
      p <- predict(model, localdata)
      o <- model.response(model.frame(formula, localdata))
      o <- o[names(p)]
      r <- na.omit(o - p)
      mse <- sum(r^2)/length(r)
      return(mse)
    }
    
 #   data <- model.frame(formula, data)
    size <- nrow(data)
    
    nd <- !is.null(newdata)
    s2 <- !is.null(slices2)
    
    if (random) 
      isel <- sample.int(size)
    else 
      isel <- seq(1:size)
    
    ssel <- (((seq(1:size) - 1) * slices) %/% size) + 1
    
    mses <- vector()
    for (k in 1:slices) {
      i <- isel[ssel == k]
      d <- data[i, ]
      dd <- data[-i, ]
      model <- lm(formula, dd)
      
      mses[[k]] <- msefunc(model, d)
    }
    
    avmse <- mean(mses)
    
    model <- lm(formula, data)
    
    if (nd) 
      mse <- msefunc(model, newdata)
    
    if (!details & !nd & !s2) 
      return(avmse)
    
    result <- list()
    class(result) <- "crossval"
    result$cvmse = avmse
    if (nd) 
      result$newmse = mse
    if (details) {
      result$rmse = sqrt(avmse)
      result$mses = mses
    }
    
    if (s2) {
      result2 <- checkcrossval(formula, data, slices = slices2, details = details)
      if (details) {
        result$cvmse2 <- result2$cvmse
        result$rmse2 <- result2$rmse
        result$mses2 <- result2$mses 
      } else 
        result$cvmse2 <- result2
    }
    
    result$model = model
    result$formula = formula
    
    return(result)
  }
