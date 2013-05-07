#'@export
func.Suesse <- function (Mahlgrad, Mehlmenge, Nasspressen, Wassermenge) {
  d <- pred.data(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
  predict(modelf.Suesse, d)
}
