#'@export
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
