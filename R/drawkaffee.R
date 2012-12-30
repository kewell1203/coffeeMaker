#'@export
drawkaffee <- function (xlim, ylim, func, steps = 25, xlab = NULL, ylab = NULL, zlab = NULL, bewlim=T, ...) {
  x <- seq(xlim[1], xlim[2], length.out = steps)
  y <- seq(ylim[1], ylim[2], length.out = steps)
  m <- outer(x, y, Vectorize(func))
  if (bewlim) {
    c <- c(rep(cm.colors(2)[[1]], 20), cm.colors(21), rep(cm.colors(2)[[2]], 20))
    image(x, y, m, xlab=xlab, ylab=ylab, main=zlab, col=c, zlim=c(-3, 3), ...)
    contour(x, y, m, levels=seq(-3, 3, .5), drawlabels=T, add=T)
  } else {
    image(x, y, m, xlab=xlab, ylab=ylab, main=zlab, ...)
    contour(x, y, m, drawlabels=T, add=T)
  }
}
