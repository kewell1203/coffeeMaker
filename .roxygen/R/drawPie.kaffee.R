#'@import ggplot2
#'@import reshape
#'@export
drawPie.kaffee <- function(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
{
  Suesse <- round(func.Suesse(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge))+3
  Saeure <- round(func.Saeure(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge))+3
  Koerper <- round(func.Koerper(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge))+3
  Bitterkeit <- round(func.Bitterkeit(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge))+3
  Geschmack <- round(func.Geschmack(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge))+3
  df <- data.frame(Suesse=Suesse, Saeure=Saeure, Koerper=Koerper, Bitterkeit=Bitterkeit, Geschmack=Geschmack)
  g <- ggplot(melt.data.frame(df), aes(x=variable,
                                       y=value,fill=variable))
  g <- g + geom_bar() + geom_text(aes(y=value+0.5, label=value-3)) + coord_polar()
  print(g)
}