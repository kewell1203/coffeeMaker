model.type.intercept = FALSE

checksrc <- function (x, data = NULL, level = 0.95, col = "lavender", ...) {
  f <- model.frame(x, data)
  for (n in names(f)) 
    f[[n]] <- (f[[n]] - mean(f[[n]])) / sd(f[[n]])
  x0 <- update(as.formula(x), ~ . + 0)
  m <- lm(x0, as.data.frame(f))
  c <- coef(m)
  ci <- confint(m, level=level)
  ci.l <- ci[,1]
  ci.u <- ci[,2]   
  xlim <- c( min(ci.l, -1), max(ci.u, 1) ) 
  require(gplots)
  barplot2(c, plot.ci=TRUE, ci.l=ci.l, ci.u=ci.u, horiz=T, xlim=xlim, col=col, xlab="Standardized Regression Coefficient", ...)
  return(m)
}

checklars <- function (x, data = NULL, type = c("lasso", "lar", "forward.stagewise", "stepwise"), intercept = T, max.steps = NULL, K = 10, plot.it = T, plot.cv = F, plot.data = F, ...) {
  require(lars)
  type <- match.arg(type)
  f <- model.frame(x, data)
  i <- model.matrix(x, data)
  o <- model.response(f)
  if (missing(max.steps))
    l <- lars(i, o, type=type, intercept=intercept)
  else
    l <- lars(i, o, type=type, intercept=intercept, max.steps=max.steps)
  if (plot.it) {
    def.par <- par(no.readonly=T)
    if (plot.data)  
      layout(matrix(1:4, nrow=2, ncol=2), respect=F)
    else {
      if (plot.cv)			
        layout(matrix(1:2, nrow=2, ncol=1), respect=F)
      else
        layout(matrix(1, nrow=1, ncol=1), respect=F)
    }
    plot(l)
  }
  if (missing(max.steps))
    c <- cv.lars(i, o, type=type, mode="fraction", K=K, intercept=intercept, plot.it=plot.it & plot.cv)
  else
    c <- cv.lars(i, o, type=type, mode="fraction", K=K, intercept=intercept, plot.it=plot.it & plot.cv, max.steps=max.steps)
  if (plot.it) {
    if (plot.data) {
      t <- terms(x)
      xlab <- names(attr(t, "factors")[,1])[attr(t, "response")]
      ylab <- "predict"
      p <- predict(l, i, s=1, mode="fraction")$fit
      plot(o, p, xlab=xlab, ylab=ylab, ...)
      ms <- c$index[c$cv == min(c$cv)][1]
      po <- predict(l, i, s=ms, mode="fraction")$fit
      plot(o, po, xlab=xlab, ylab=ylab, main="cv-opt solution", ...)
    }
    par(def.par)
  }
  return(l)
}

kaffee <- read.csv("~/IdeaProjects/Detact/Standalone/DETACT_STANDALONE/resources/kaffee.csv")

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

kaffee$Mahlgrad0 <- kaffee$Mahlgrad - 2
kaffee$Mehlmenge0 <- kaffee$Mehlmenge - 85
kaffee$Wassermenge0 <- kaffee$Wassermenge - 50
kaffee$NasspressenF <- factor(kaffee$Nasspressen > 0, labels=c("aus", "ein"))

kaffee0 <- subset(kaffee, Preinfusion.Menge == 30 & Pause.Preinfusion == 5 & Pressdruck == 50)
kaffee.main <- with(subset(kaffee0, !grepl("pre", Rezept.ID)), data.frame(P.ID, Mehlmenge, Mahlgrad, Nasspressen, Suesse, Saeure, Bitterkeit, Koerper, Geschmack, Crema..Farbe, Crema..Festigkeit, Crema..Hoehe, Crema..Tigerung, Mahlgrad0, Mehlmenge0, Wassermenge0, NasspressenF))
kaffee.all <- with(kaffee0, data.frame(P.ID, Mehlmenge, Wassermenge, Mahlgrad, Nasspressen, Suesse, Saeure, Bitterkeit, Koerper, Geschmack, Crema..Farbe, Crema..Festigkeit, Crema..Hoehe, Crema..Tigerung, Mahlgrad0, Mehlmenge0, Wassermenge0, NasspressenF))

kaffee.main0 <- subset(kaffee.main, P.ID %in% c(3, 5, 9, 14))
kaffee.all0 <- subset(kaffee.all, P.ID %in% c(3, 5, 9, 13, 14, 12, 8))

model.Suesse <- lm(Suesse ~ Mahlgrad * Mehlmenge + Nasspressen, kaffee.main0)
model.Saeure <- lm(Saeure ~ Mahlgrad + Mehlmenge * Nasspressen, kaffee.main0)
model.Bitterkeit <- lm(Bitterkeit ~ Mahlgrad + Mehlmenge + Nasspressen, kaffee.main0)
model.Koerper <- lm(Koerper ~ poly(Mahlgrad, 2) + Mehlmenge + Nasspressen, kaffee.main0)
model.Geschmack <- lm(Geschmack ~ Suesse * Koerper + Bitterkeit + Saeure + I(Koerper^2), kaffee.main0)

modela.Suesse <- lm(Suesse ~ I(Mahlgrad^2) + I(Mehlmenge^2) + Mahlgrad:Nasspressen:Wassermenge + I(Wassermenge^2) + Nasspressen:Wassermenge, kaffee.all0)
modela.Saeure <- lm(Saeure ~ Nasspressen:Wassermenge + Mahlgrad:Nasspressen:Wassermenge + I(Wassermenge^2) + Mehlmenge + Mahlgrad + Wassermenge + Mehlmenge:Nasspressen + Mahlgrad:Mehlmenge:Nasspressen + Mahlgrad:Mehlmenge, kaffee.all0)
modela.Bitterkeit <- lm(Bitterkeit ~ Mehlmenge + Mahlgrad:Nasspressen + I(Wassermenge^2) + Mahlgrad + Wassermenge, kaffee.all0)
modela.Koerper <- lm(Koerper ~ Mahlgrad:Nasspressen + I(Wassermenge^2) + Mehlmenge + Mahlgrad + Mahlgrad:Wassermenge, kaffee.all0)
modela.Geschmack <- lm(Geschmack ~ I(Wassermenge^2) + Mahlgrad + Mehlmenge:Nasspressen + I(Mehlmenge^2) + Mahlgrad:Mehlmenge:Wassermenge + Nasspressen:Wassermenge + Mehlmenge:Nasspressen:Wassermenge + Mahlgrad:Mehlmenge:Nasspressen + Wassermenge, kaffee.all0)

models.Suesse <- lm(Suesse ~ Mehlmenge0 + I(Mehlmenge0^2) + Mahlgrad0:Mehlmenge0 + Mahlgrad0:Mehlmenge0:Wassermenge0 + Nasspressen:Wassermenge0 + 0, kaffee.all0)
models.Saeure <- lm(Saeure ~ I(Mehlmenge0^2) + I(Wassermenge0^2) + Mehlmenge0:Nasspressen + Mehlmenge0:Nasspressen:Wassermenge0 + Mahlgrad0:Mehlmenge0 + Mahlgrad0:Mehlmenge0:Wassermenge0 + Wassermenge0 + I(Mahlgrad0^2) + 0, kaffee.all0)
models.Bitterkeit <- lm(Bitterkeit ~ I(Wassermenge0^2) + Mehlmenge0:Nasspressen + Mehlmenge0:Nasspressen:Wassermenge0 + I(Mehlmenge0^2) + I(Mahlgrad0^2) + Mahlgrad0:Wassermenge0 + Wassermenge0 + Mehlmenge0 + 0, kaffee.all0)
models.Koerper <- lm(Koerper ~ I(Wassermenge0^2) + Mehlmenge0:Nasspressen + Mehlmenge0:Nasspressen:Wassermenge0 + Wassermenge0 + I(Mehlmenge0^2) + Mahlgrad0 + 0, kaffee.all0)
models.Geschmack <- lm(Geschmack ~ I(Wassermenge0^2) + I(Mahlgrad0^2) + Wassermenge0 + Mahlgrad0:Mehlmenge0 + Mahlgrad0:Mehlmenge0:Wassermenge0 + Nasspressen:Wassermenge0 + Mahlgrad0:Nasspressen:Wassermenge0 + 0, kaffee.all0)

if (model.type.intercept) {
  modelf.Suesse <- modela.Suesse
  modelf.Saeure <- modela.Saeure
  modelf.Bitterkeit <- modela.Bitterkeit
  modelf.Koerper <- modela.Koerper
  modelf.Geschmack <- modela.Geschmack
} else {
  modelf.Suesse <- models.Suesse
  modelf.Saeure <- models.Saeure
  modelf.Bitterkeit <- models.Bitterkeit
  modelf.Koerper <- models.Koerper
  modelf.Geschmack <- models.Geschmack
}

pred.data <- function (Mahlgrad, Mehlmenge, Nasspressen, Wassermenge) {
  data.frame(Mahlgrad=Mahlgrad, Mehlmenge=Mehlmenge, Nasspressen=Nasspressen, Wassermenge=Wassermenge,
             Mahlgrad0=Mahlgrad - 2, Mehlmenge0=Mehlmenge - 85, Wassermenge0=Wassermenge - 50)
}

func.Suesse <- function (Mahlgrad, Mehlmenge, Nasspressen, Wassermenge) {
  d <- pred.data(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
  predict(modelf.Suesse, d)
}
func.Saeure <- function (Mahlgrad, Mehlmenge, Nasspressen, Wassermenge) {
  d <- pred.data(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
  predict(modelf.Saeure, d)
}
func.Bitterkeit <- function (Mahlgrad, Mehlmenge, Nasspressen, Wassermenge) {
  d <- pred.data(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
  predict(modelf.Bitterkeit, d)
}
func.Koerper <- function (Mahlgrad, Mehlmenge, Nasspressen, Wassermenge) {
  d <- pred.data(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
  predict(modelf.Koerper, d)
}
func.Geschmack <- function (Mahlgrad, Mehlmenge, Nasspressen, Wassermenge) {
  d <- pred.data(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
  predict(modelf.Geschmack, d)
}
# func.Geschmack <- function (Mahlgrad, Mehlmenge, Nasspressen, Wassermenge) {
#   d <- pred.data(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
#   d$Suesse <- func.Suesse(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
#   d$Saeure <- func.Saeure(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
#   d$Bitterkeit <- func.Bitterkeit(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
#   d$Koerper <- func.Koerper(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
#   predict(model.Geschmack, d)
# }

kaffee.qual <- function (Mahlgrad, Mehlmenge, Nasspressen, Wassermenge, factor.Geschmack = 0, Ziel.Suesse = 0, Ziel.Saeure = 0, Ziel.Bitterkeit = 0, Ziel.Koerper = 0) {
  Suesse <- func.Suesse(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
  Saeure <- func.Saeure(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
  Bitterkeit <- func.Bitterkeit(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
  Koerper <- func.Koerper(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
  QDiff <- (Suesse - Ziel.Suesse)^2 + (Saeure - Ziel.Saeure)^2 + (Bitterkeit - Ziel.Bitterkeit)^2 + (Koerper - Ziel.Koerper)^2
  Quality <- -QDiff
  if (factor.Geschmack != 0) {
    Geschmack <- func.Geschmack(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
    Quality <- Quality + factor.Geschmack * Geschmack
  }
  return(Quality)
}

compliance.qual <- function (quality) {
  -log(-quality + 1) + 1
}

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
                      function(param) -detact.v.te9jzt.qual(Mahlgrad, param[1], Nasspressen, Wassermenge, ...), 
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
kaffee.optim <- function (Wassermenge, Mahlgrad = NULL, Nasspressen = NULL, Mehlmenge.min = 40, Mehlmenge.max = 100, ...) {
  if (missing(Nasspressen) || is.null(Nasspressen)) 
    Nasspressen <- c(0, 20)
  f <- function(Nasspressen) kaffee.opt3(Wassermenge, Mahlgrad, Nasspressen, Mehlmenge.min, Mehlmenge.max, ...)
  return(maxval(Nasspressen, f, function(v) v[["Quality"]]))
}

kaffee.optim.plot <- function (Mahlgrad = NULL, Nasspressen = NULL, Mehlmenge.min = 40, Mehlmenge.max = 100, steps = 26, main = NULL, ...) {
  Wassermenge <- seq(25, 50, length.out=steps)
  result <- apply(as.array(Wassermenge), 1, 
                  function(x) kaffee.optim(x, Mahlgrad, Nasspressen, Mehlmenge.min, Mehlmenge.max, ...))
  data <- as.data.frame(t(as.array(result)))
  data$Compliance <- compliance.qual(data$Quality)
  if (missing(main))
    main <- "Ergebnis der Optimierung"
  require(zoo)
  z <- zoo(data[,c("Mehlmenge", "Mahlgrad", "Compliance")], order.by=data$Wassermenge)
  col = data$Nasspressen/20 + 2
  plot(z, col=list(Mehlmenge=col, Mahlgrad=col, Compliance=1), type="p",
       xlab="Wassermenge", main=main)
}

# require(lattice)
# print(splom(~ data.frame(Mehlmenge, Wassermenge, Mahlgrad, Nasspressen, Suesse, Saeure, Bitterkeit, Koerper, Geschmack), kaffee.all, panel=kaffee.splom.panel))
# print(splom(~ data.frame(Mehlmenge, Wassermenge, Mahlgrad, Nasspressen, Crema..Farbe, Crema..Festigkeit, Crema..Hoehe, Crema..Tigerung), kaffee.all, panel=kaffee.splom.panel))

# require(corrplot)
# par.def <- par()
# corrplot(cor(kaffee.all[, 2:14], use="comp"))
# suppressWarnings(par(par.def))

# require(effects)
# checksrc(Suesse ~ Mahlgrad * Mehlmenge + Nasspressen, kaffee.main0, main="Sensitivitaetsanalyse Suesse")
# plot(allEffects(model.Suesse), ask=F)
# checksrc(Saeure ~ Mahlgrad + Mehlmenge * Nasspressen, kaffee.main0, main="Sensitivitaetsanalyse Saeure")
# plot(allEffects(model.Saeure), ask=F)
# checksrc(Bitterkeit ~ Mahlgrad + Mehlmenge + Nasspressen, kaffee.main0, main="Sensitivitaetsanalyse Bitterkeit")
# plot(allEffects(model.Bitterkeit), ask=F)
# checksrc(Koerper ~ Mahlgrad + Mehlmenge + Nasspressen + I(Mahlgrad^2), kaffee.main0, main="Sensitivitaetsanalyse Koerper")
# plot(allEffects(model.Koerper), ask=F)
# checksrc(Geschmack ~ Suesse * Koerper + Bitterkeit + Saeure + I(Koerper^2), kaffee.main0, main="Sensitivitaetsanalyse Geschmack")
# plot(allEffects(model.Geschmack), ask=F)

# drawkaffee(c(1, 3.5), c(40, 60), function(x, y) func.Suesse(x, y, 0, 25), steps=40, xlab="Mahlgrad", ylab="Mehlmenge", zlab="Suesse ohne Nasspressen, einfacher Espresso")
# drawkaffee(c(1, 3.5), c(40, 60), function(x, y) func.Suesse(x, y, 20, 25), steps=40, xlab="Mahlgrad", ylab="Mehlmenge", zlab="Suesse mit Nasspressen, einfacher Espresso")
# drawkaffee(c(1, 3.5), c(40, 60), function(x, y) func.Saeure(x, y, 0, 25), steps=40, xlab="Mahlgrad", ylab="Mehlmenge", zlab="Saeure ohne Nasspressen, einfacher Espresso")
# drawkaffee(c(1, 3.5), c(40, 60), function(x, y) func.Saeure(x, y, 20, 25), steps=40, xlab="Mahlgrad", ylab="Mehlmenge", zlab="Saeure mit Nasspressen, einfacher Espresso")
# drawkaffee(c(1, 3.5), c(40, 60), function(x, y) func.Bitterkeit(x, y, 0, 25), steps=40, xlab="Mahlgrad", ylab="Mehlmenge", zlab="Bitterkeit ohne Nasspressen, einfacher Espresso")
# drawkaffee(c(1, 3.5), c(40, 60), function(x, y) func.Bitterkeit(x, y, 20, 25), steps=40, xlab="Mahlgrad", ylab="Mehlmenge", zlab="Bitterkeit mit Nasspressen, einfacher Espresso")
# drawkaffee(c(1, 3.5), c(40, 60), function(x, y) func.Koerper(x, y, 0, 25), steps=40, xlab="Mahlgrad", ylab="Mehlmenge", zlab="Koerper ohne Nasspressen, einfacher Espresso")
# drawkaffee(c(1, 3.5), c(40, 60), function(x, y) func.Koerper(x, y, 20, 25), steps=40, xlab="Mahlgrad", ylab="Mehlmenge", zlab="Koerper mit Nasspressen, einfacher Espresso")
# drawkaffee(c(1, 3.5), c(40, 60), function(x, y) func.Geschmack(x, y, 0, 25), steps=40, xlab="Mahlgrad", ylab="Mehlmenge", zlab="Geschmack ohne Nasspressen, einfacher Espresso")
# drawkaffee(c(1, 3.5), c(40, 60), function(x, y) func.Geschmack(x, y, 20, 25), steps=40, xlab="Mahlgrad", ylab="Mehlmenge", zlab="Geschmack mit Nasspressen, einfacher Espresso")

# print(checklars(Suesse ~ Mahlgrad * Mehlmenge * Nasspressen * Wassermenge + I(Mahlgrad^2) + I(Mehlmenge^2) + I(Wassermenge^2), kaffee.all, max.steps=10, plot.cv=T))
# print(checklars(Saeure ~ Mahlgrad * Mehlmenge * Nasspressen * Wassermenge + I(Mahlgrad^2) + I(Mehlmenge^2) + I(Wassermenge^2), kaffee.all, max.steps=10, plot.cv=T))
# print(checklars(Bitterkeit ~ Mahlgrad * Mehlmenge * Nasspressen * Wassermenge + I(Mahlgrad^2) + I(Mehlmenge^2) + I(Wassermenge^2), kaffee.all, max.steps=10, plot.cv=T))
# print(checklars(Koerper ~ Mahlgrad * Mehlmenge * Nasspressen * Wassermenge + I(Mahlgrad^2) + I(Mehlmenge^2) + I(Wassermenge^2), kaffee.all, max.steps=10, plot.cv=T))
# print(checklars(Geschmack ~ Mahlgrad * Mehlmenge * Nasspressen * Wassermenge + I(Mahlgrad^2) + I(Mehlmenge^2) + I(Wassermenge^2), kaffee.all, max.steps=10, plot.cv=T))

# drawkaffee(c(1, 3.5), c(40, 60), function(x, y) compliance.qual(kaffee.qual(x, y, 0, 25)), xlab="Mahlgrad", ylab="Mehlmenge", zlab="Compliance ohne Nasspressen, einfacher Espresso", bewlim=F, zlim=c(-2, 1), col=heat.colors(21))
# drawkaffee(c(1, 3.5), c(40, 60), function(x, y) compliance.qual(kaffee.qual(x, y, 20, 25)), xlab="Mahlgrad", ylab="Mehlmenge", zlab="Compliance mit Nasspressen, einfacher Espresso", bewlim=F, zlim=c(-2, 1), col=heat.colors(21))
# drawkaffee(c(1, 3.5), c(60, 90), function(x, y) compliance.qual(kaffee.qual(x, y, 0, 50)), xlab="Mahlgrad", ylab="Mehlmenge", zlab="Compliance ohne Nasspressen, doppelter Espresso", bewlim=F, zlim=c(-2, 1), col=heat.colors(21))

# kaffee.optim.plot()

require(sensitivity)

sobol.n <- 5000
sobol.nboot <- 100

sobol.X1 <- data.frame(Mahlgrad=runif(sobol.n, 1, 3.5), Mehlmenge=runif(sobol.n, 20, 30), Nasspressen=ifelse(runif(sobol.n)<.5, 0, 20), Wassermenge=runif(sobol.n, 50, 70))
sobol.X2 <- data.frame(Mahlgrad=runif(sobol.n, 1, 3.5), Mehlmenge=runif(sobol.n, 20, 30), Nasspressen=ifelse(runif(sobol.n)<.5, 0, 20), Wassermenge=runif(sobol.n, 50, 70))

if (!model.type.intercept) {
  sobol.X1$Mahlgrad0 <- sobol.X1$Mahlgrad - 2
  sobol.X1$Mehlmenge0 <- sobol.X1$Mehlmenge - 85
  sobol.X1$Wassermenge0 <- sobol.X1$Wassermenge - 50
  sobol.X1$Mahlgrad <- NULL
  sobol.X1$Mehlmenge <- NULL
  sobol.X1$Wassermenge <- NULL
  sobol.X1 <- sobol.X1[, c("Mahlgrad0", "Mehlmenge0", "Nasspressen", "Wassermenge0")]
  sobol.X2$Mahlgrad0 <- sobol.X2$Mahlgrad - 2
  sobol.X2$Mehlmenge0 <- sobol.X2$Mehlmenge - 85
  sobol.X2$Wassermenge0 <- sobol.X2$Wassermenge - 50
  sobol.X2$Mahlgrad <- NULL
  sobol.X2$Mehlmenge <- NULL
  sobol.X2$Wassermenge <- NULL
  sobol.X2 <- sobol.X2[, c("Mahlgrad0", "Mehlmenge0", "Nasspressen", "Wassermenge0")]
}

sobol.Suesse <- sobol2002(modelf.Suesse, sobol.X1, sobol.X2, nboot=sobol.nboot)
sobol.Saeure <- sobol2002(modelf.Saeure, sobol.X1, sobol.X2, nboot=sobol.nboot)
sobol.Bitterkeit <- sobol2002(modelf.Bitterkeit, sobol.X1, sobol.X2, nboot=sobol.nboot)
sobol.Koerper <- sobol2002(modelf.Koerper, sobol.X1, sobol.X2, nboot=sobol.nboot)
sobol.Geschmack <- sobol2002(modelf.Geschmack, sobol.X1, sobol.X2, nboot=sobol.nboot)

plot(sobol.Suesse); title("Suesse")
plot(sobol.Saeure); title("Saeure")
plot(sobol.Bitterkeit); title("Bitterkeit")
plot(sobol.Koerper); title("Koerper")
plot(sobol.Geschmack); title("Geschmack")
