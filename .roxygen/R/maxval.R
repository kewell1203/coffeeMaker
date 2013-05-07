maxval <- function (vec, fun1, fun2 = NULL) {
  if (missing(fun2))
    fun2 <- function(x) x
  val <- fun1(vec[1])
  y <- fun2(val)
  if (length(vec) > 1) {
    for (i in 1:length(vec)) {
      val.temp <- fun1(vec[i])
      y.temp <- fun2(val.temp)
      if (y.temp > y) {
        val <- val.temp
        y <- y.temp
      }
    }
  }
  return(val)
}
