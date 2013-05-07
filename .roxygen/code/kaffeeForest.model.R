library(randomForest)
library(coffeemachine)
data(kaffee)
kaffee$Mahlgrad0 <- kaffee$Mahlgrad - 2
kaffee$Mehlmenge0 <- kaffee$Mehlmenge - 85
kaffee$Wassermenge0 <- kaffee$Wassermenge - 50
kaffee$NasspressenF <- factor(kaffee$Nasspressen > 0, labels=c("aus", "ein"))

kaffee0 <- subset(kaffee, Preinfusion.Menge == 30 & Pause.Preinfusion == 5 & Pressdruck == 50)
kaffee.main <- with(subset(kaffee0, !grepl("pre", Rezept.ID)), data.frame(P.ID, Mehlmenge, Mahlgrad, Nasspressen, Suesse, Saeure, Bitterkeit, Koerper, Geschmack, Crema..Farbe, Crema..Festigkeit, Crema..Hoehe, Crema..Tigerung, Mahlgrad0, Mehlmenge0, Wassermenge0, NasspressenF))
kaffee.all <- with(kaffee0, data.frame(P.ID, Mehlmenge, Wassermenge, Mahlgrad, Nasspressen, Suesse, Saeure, Bitterkeit, Koerper, Geschmack, Crema..Farbe, Crema..Festigkeit, Crema..Hoehe, Crema..Tigerung, Mahlgrad0, Mehlmenge0, Wassermenge0, NasspressenF))

kaffee.main0 <- subset(kaffee.main, P.ID %in% c(3, 5, 9, 14))
kaffee.all0 <- subset(kaffee.all, P.ID %in% c(3, 5, 9, 13, 14, 12, 8))


modelf.Suesse<-randomForest(factor(Suesse)~Mahlgrad0+Mehlmenge0+Nasspressen+Wassermenge0, na.omit(kaffee.main0), 
             mtry=2, importance=TRUE, do.trace=100)

func.Suesse <- function (Mahlgrad, Mehlmenge, Nasspressen, Wassermenge) {
  d <- pred.data (Mahlgrad, Mehlmenge, Nasspressen, Wassermenge)
  p<-as.numeric(predict(modelf.Suesse, d))-3
}

drawkaffee(c(1,3.5), c(30,70), function(x,y) func.Suesse(x,y,20,50), xlab="Mahlgrad", ylab="Mehlmenge", zlab="Suesse")
