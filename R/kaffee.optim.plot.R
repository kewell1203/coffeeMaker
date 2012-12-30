#'@export
#'@importFrom zoo zoo
kaffee.optim.plot <- function (Mahlgrad = NULL, Nasspressen = NULL, Mehlmenge.min = 40, Mehlmenge.max = 100, steps = 26, main = NULL, ...) {
  Wassermenge <- seq(25, 50, length.out=steps)
  result <- apply(as.array(Wassermenge), 1, 
                  function(x) kaffee.optim(x, Mahlgrad, Nasspressen, Mehlmenge.min, Mehlmenge.max, ...))
  data <- as.data.frame(t(as.array(result)))
  data$Compliance <- compliance.qual(data$Quality)
  if (missing(main))
    main <- "Ergebnis der Optimierung"
  z <- zoo(data[,c("Mehlmenge", "Mahlgrad", "Compliance")], order.by=data$Wassermenge)
  col = data$Nasspressen/20 + 2
  plot(z, col=list(Mehlmenge=col, Mahlgrad=col, Compliance=1), type="p",
       xlab="Wassermenge", main=main)
}
