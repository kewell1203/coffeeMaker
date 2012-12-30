model.type.intercept = FALSE
#criteria of index coffee to be made
kaffee$Mahlgrad0 <- kaffee$Mahlgrad - 2
kaffee$Mehlmenge0 <- kaffee$Mehlmenge - 85
kaffee$Wassermenge0 <- kaffee$Wassermenge - 50
kaffee$NasspressenF <- factor(kaffee$Nasspressen > 0, labels=c("aus", "ein"))

kaffee0 <- subset(kaffee, Preinfusion.Menge == 30 & Pause.Preinfusion == 5 & Pressdruck == 50)
kaffee.main <- with(subset(kaffee0, !grepl("pre", Rezept.ID)), data.frame(P.ID, Mehlmenge, Mahlgrad, Nasspressen, Suesse, Saeure, Bitterkeit, Koerper, Geschmack, Crema..Farbe, Crema..Festigkeit, Crema..Hoehe, Crema..Tigerung, Mahlgrad0, Mehlmenge0, Wassermenge0, NasspressenF))
kaffee.all <- with(kaffee0, data.frame(P.ID, Mehlmenge, Wassermenge, Mahlgrad, Nasspressen, Suesse, Saeure, Bitterkeit, Koerper, Geschmack, Crema..Farbe, Crema..Festigkeit, Crema..Hoehe, Crema..Tigerung, Mahlgrad0, Mehlmenge0, Wassermenge0, NasspressenF))

kaffee.main0 <- subset(kaffee.main, P.ID %in% c(3, 5, 9, 14))
kaffee.all0 <- subset(kaffee.all, P.ID %in% c(3, 5, 9, 13, 14, 12, 8))

## using LM
model.Suesse <- lm(Suesse ~ Mahlgrad * Mehlmenge + Nasspressen, kaffee.main0)
model.Saeure <- lm(Saeure ~ Mahlgrad + Mehlmenge * Nasspressen, kaffee.main0)
model.Bitterkeit <- lm(Bitterkeit ~ Mahlgrad + Mehlmenge + Nasspressen, kaffee.main0)
model.Koerper <- lm(Koerper ~ poly(Mahlgrad, 2) + Mehlmenge + Nasspressen, kaffee.main0)
model.Geschmack <- lm(Geschmack ~ Suesse * Koerper + Bitterkeit + Saeure + I(Koerper^2), kaffee.main0)

modela.Suesse <- lm(Suesse ~ I(Mahlgrad^2) + I(Mehlmenge^2) + Mahlgrad:Nasspressen:Wassermenge + I(Wassermenge^2) + Nasspressen:Wassermenge, kaffee.all0)
modela.Saeure <- lm(Saeure ~ Nasspressen:Wassermenge + Mahlgrad:Nasspressen:Wassermenge + I(Wassermenge^2) + Mehlmenge + Mahlgrad + Wassermenge + Mehlmenge:Nasspressen + Mahlgrad:Mehlmenge:Nasspressen + Mahlgrad:Mehlmenge, kaffee.all0)
modela.Bitterkeit <- lm(Bitterkeit ~ Mehlmenge + Mahlgrad:Nasspressen + I(Wassermenge^2) + Mahlgrad + Wassermenge, kaffee.all0)
modela.Koerper <- lm(Koerper ~ Mahlgrad:Nasspressen + I(Wassermenge^2) + Mehlmenge + Mahlgrad + Mahlgrad:Wassermenge, kaffee.all0)
modela.Geschmack <- lm(Geschmack ~ I(Wassermenge^2) + Mahlgrad + Mehlmenge:Nasspressen + I(Mehlmenge^2) + Mahlgrad:Mehlmenge:Wassermenge + Nasspressen:Wassermenge + Mehlmenge:Nasspressen:Wassermenge + Mahlgrad:Mehlmenge:Nasspressen + Wassermenge, kaffee.all0)

models.Suesse <- lm(Suesse ~ Mehlmenge0 + I(Mehlmenge0^2) + Mahlgrad0:Mehlmenge0 + Mahlgrad0:Mehlmenge0:Wassermenge0 + Nasspressen:Wassermenge0 + 0, kaffee.all0)
models.Saeure <- lm(Saeure ~ I(Mehlmenge0^2) + I(Wassermenge0^2) + Mehlmenge0:Nasspressen + Mehlmenge0:Nasspressen:Wassermenge0 + Mahlgrad0:Mehlmenge0 + Mahlgrad0:Mehlmenge0:Wassermenge0 + Wassermenge0 + I(Mahlgrad0^2) + 0, kaffee.all0)
models.Bitterkeit <- lm(Bitterkeit ~ I(Wassermenge0^2) + Mehlmenge0:Nasspressen + Mehlmenge0:Nasspressen:Wassermenge0 + I(Mehlmenge0^2) + I(Mahlgrad0^2) + Mahlgrad0:Wassermenge0 + Wassermenge0 + Mehlmenge0 + 0, kaffee.all0)
models.Koerper <- lm(Koerper ~ I(Wassermenge0^2) + Mehlmenge0:Nasspressen + Mehlmenge0:Nasspressen:Wassermenge0 + Wassermenge0 + I(Mehlmenge0^2) + Mahlgrad0 + 0, kaffee.all0)
models.Geschmack <- lm(Geschmack ~ I(Wassermenge0^2) + I(Mahlgrad0^2) + Wassermenge0 + Mahlgrad0:Mehlmenge0 + Mahlgrad0:Mehlmenge0:Wassermenge0 + Nasspressen:Wassermenge0 + Mahlgrad0:Nasspressen:Wassermenge0 + 0, kaffee.all0)

if (model.type.intercept) {
  modelf.Suesse <- modela.Suesse
  modelf.Saeure <- modela.Saeure
  modelf.Bitterkeit <- modela.Bitterkeit
  modelf.Koerper <- modela.Koerper
  modelf.Geschmack <- modela.Geschmack
} else {
  modelf.Suesse <- models.Suesse
  modelf.Saeure <- models.Saeure
  modelf.Bitterkeit <- models.Bitterkeit
  modelf.Koerper <- models.Koerper
  modelf.Geschmack <- models.Geschmack
}
