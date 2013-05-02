kaffee$Mahlgrad0 <- kaffee$Mahlgrad - 2
kaffee$Mehlmenge0 <- kaffee$Mehlmenge - 85
kaffee$Wassermenge0 <- kaffee$Wassermenge - 50
kaffee$NasspressenF <- factor(kaffee$Nasspressen > 0, labels=c("aus", "ein"))

kaffee0 <- subset(kaffee, Preinfusion.Menge == 30 & Pause.Preinfusion == 5 & Pressdruck == 50)
kaffee.main <- with(subset(kaffee0, !grepl("pre", Rezept.ID)), data.frame(P.ID, Mehlmenge, Mahlgrad, Nasspressen, Suesse, Saeure, Bitterkeit, Koerper, Geschmack, Crema..Farbe, Crema..Festigkeit, Crema..Hoehe, Crema..Tigerung, Mahlgrad0, Mehlmenge0, Wassermenge0, NasspressenF))
kaffee.all <- with(kaffee0, data.frame(P.ID, Mehlmenge, Wassermenge, Mahlgrad, Nasspressen, Suesse, Saeure, Bitterkeit, Koerper, Geschmack, Crema..Farbe, Crema..Festigkeit, Crema..Hoehe, Crema..Tigerung, Mahlgrad0, Mehlmenge0, Wassermenge0, NasspressenF))

kaffee.main0 <- subset(kaffee.main, P.ID %in% c(3, 5, 9, 14))
kaffee.all0 <- subset(kaffee.all, P.ID %in% c(3, 5, 9, 13, 14, 12, 8))

require(caret)
require(e1071)


svmGrid <- expand.grid(.gamma = 10 ^ (-3:1), .cost=10^(-1:1))
modelFun <- function (data, parameter, levels, last, ...)
{
  library(e1071)
  list(fit = svm(.outcome~.,data=data, gamma=parameter$.gamma,
                 cost=parameter$.cost,
                 kernel='radial'
                 # ,type='C-classification'
  ))
}

preFunc <- function(object, newdata)
{
  library(e1071)
  p<-predict(object$fit, newdata)
  # p<-as.numeric(p)-3
}

sortFunc <- function(x) x[order(x$gamma, x$cost),]
controlFit<-trainControl(custom=list(parameters=svmGrid, model=modelFun, prediction=preFunc, 
                                     probability=NULL, sort=sortFunc, method="cv"),
                         repeats=4)
modelf.Suesse<-train(Suesse~Mahlgrad+Mehlmenge+Nasspressen, data=kaffee.main0,"custom",trControl=controlFit)
drawkaffee(c(1,3.5), c(30,70), function(x,y) func.Suesse(x,y,20,50), xlab="Mahlgrad", ylab="Mehlmenge")
# controlFit <- trainControl(number=50,repeats=4)
# model.Suesse<-train(Suesse~Mahlgrad+Mehlmenge+Nasspressen, data=kaffee.main0,trControl=controlFit)
# modelFun <- function (data, parameter, levels, last, ...)
#   {
#     library(kernlab)
#     list(fit = ksvm(.outcome~.,data=data, kpar=list(sigma=parameter$.sigma, degree=parameter$.degree)
#                    kernel='anovadot',
#                    ,type='spoc-svc'
#     ))
#   }