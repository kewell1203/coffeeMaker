#' @export
func.Koerper <- function (Mahlgrad, Mehlmenge, Nasspressen, Wassermenge) {
  d <- pred.data(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
  predict(modelf.Koerper, d)
}