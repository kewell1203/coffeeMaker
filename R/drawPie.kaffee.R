#'@import ggplot2
#'@importFrom reshape melt.data.frame
#'@export
drawPie.kaffee <- function(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
{
  Suesse <- min(2,max(round(func.Suesse(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)),-2))+3
  Saeure <- min(2,max(round(func.Saeure(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)),-2))+3
  Koerper <- min(2,max(round(func.Koerper(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)),-2))+3
  Bitterkeit <- min(2,max(round(func.Bitterkeit(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)),-2))+3
  Geschmack <- min(2,max(round(func.Geschmack(Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)),-2))+3
  df <- data.frame(Suesse=Suesse, Saeure=Saeure, Koerper=Koerper, Bitterkeit=Bitterkeit, Geschmack=Geschmack)
  g <- ggplot(melt.data.frame(df), aes(x=variable,
                                       y=value,fill=variable))
  g <- g + geom_bar() + geom_text(aes(y=value+0.5, label=value-3)) + coord_polar()
  print(g)
}