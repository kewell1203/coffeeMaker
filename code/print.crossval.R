print.crossval <-
  function (x, ...) 
  {
    cat("Crossvalidation of model\n")
    cat("   formula: "); 
    print(x$formula); 
    cat("\n")
    
    cat("crossvalidation MSE : ", x$cvmse, "\n")
    if (!is.null(x$rmse))
      cat("   square-root      : ", x$rmse, "\n")
    if (!is.null(x$newmse))
      cat("new data MSE        :", x$newmse, "\n")
    
    if (!is.null(x$cvmse2)) {
      cat("\n")
      cat("2nd run  MSE        : ", x$cvmse2, "\n")
      if (!is.null(x$rmse2))
        cat("   square-root      : ", x$rmse2, "\n")
    }				
    
    invisible(x)	
  }