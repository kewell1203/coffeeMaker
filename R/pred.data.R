#'@export
pred.data <- function (Mahlgrad, Mehlmenge, Nasspressen, Wassermenge) {
  data.frame(Mahlgrad=Mahlgrad, Mehlmenge=Mehlmenge, Nasspressen=Nasspressen, Wassermenge=Wassermenge,
             Mahlgrad0=Mahlgrad - 2, Mehlmenge0=Mehlmenge - 85, Wassermenge0=Wassermenge - 50)
}