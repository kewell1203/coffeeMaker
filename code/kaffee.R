#  preseting model type
library(coffeemachine)
data(kaffee)
require(lattice)
source('kaffee.lin.model.R')

splom(~data.frame(Mehlmenge, Mahlgrad, Nasspressen, Wassermenge0, Suesse, 
                  Saeure, Bitterkeit, Koerper, Geschmack), kaffee.main0, panel = kaffee.splom.panel )

drawkaffee (c(1, 3.5), c(40, 60), function(x,y) 
   func.Suesse(x,y,20,70))

kaffee.optim.plot(50,Mahlgrad=1:3)


