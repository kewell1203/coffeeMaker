#' @export
func.Saeure <- function (Mahlgrad, Mehlmenge, Nasspressen, Wassermenge) {
  d <- pred.data(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
  predict(modelf.Saeure, d)
}
