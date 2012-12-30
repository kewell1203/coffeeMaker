compliance.qual <- function (quality) {
  -log(-quality + 1) + 1
}
