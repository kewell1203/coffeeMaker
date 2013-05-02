#' @export
func.Bitterkeit <- function (Mahlgrad, Mehlmenge, Nasspressen, Wassermenge) {
  d <- pred.data(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
  predict(modelf.Bitterkeit, d)
}
