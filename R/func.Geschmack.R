#'@export 
func.Geschmack <- function (Mahlgrad, Mehlmenge, Nasspressen, Wassermenge) {
  d <- pred.data(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
  predict(modelf.Geschmack, d)
}
