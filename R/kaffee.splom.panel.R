#'@importFrom lattice panel.xyplot panel.abline
#'@export
kaffee.splom.panel <- function(x, y, ...) {
  dd <- data.frame(x = x, y = y)
  dd <- na.omit(dd)
  if (nrow(dd) > 1) {
    panel.xyplot(jitter(x), jitter(y), ...)
    m <- NULL
    try(m <- lm(y~x, dd, singular.ok=F), silent=T)
    if (!is.null(m))
      panel.abline(m)
  }
}
