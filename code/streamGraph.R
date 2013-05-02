
model.Suesse <- likertlm(Suesse ~ Mehlmenge + Mahlgrad + Nasspressen, kaffee.main0)
model.Saeure <- likertlm(Saeure ~ Mehlmenge + Mahlgrad + Nasspressen, kaffee.main0)
model.Bitterkeit <- likertlm(Bitterkeit ~ Mehlmenge + Mahlgrad + Nasspressen, kaffee.main0)
model.Koerper <- likertlm(Koerper ~ Mehlmenge + Mahlgrad + Nasspressen, kaffee.main0)
model.Geschmack <- likertlm(Geschmack ~ Mehlmenge + Mahlgrad + Nasspressen, kaffee.main0)


# ssbkgVec <- function(form, data){
#    model.Suesse <- likertlm(paste("Suesse ~ " , form), kaffee.main0)
#    model.Saeure <- likertlm(paste("Saeure ~ " , form), kaffee.main0)
#    model.Bitterkeit <- likertlm(paste("Bitterkeit ~ " , form), kaffee.main0)
#    model.Koerper <- likertlm(paste("Koerper ~ " , form), kaffee.main0)
#    model.Geschmack <- likertlm(paste("Geschmack ~ " , form), kaffee.main0)
#    
#    df<-data.frame(Mahlgrad=2, Mehlmenge=60, Nasspressen=20, Suesse=0, Saeure=0,Bitterkeit=0,Koerper=0,Geschmack=0)
#    ssbkg<-c(likertlmprob(model.Suesse,df),likertlmprob(model.Saeure,df),likertlmprob(model.Bitterkeit,df),likertlmprob(model.Koerper,df),likertlmprob(model.Geschmack,df))
#    return(ssbkg)
# }

m<-matrix(1:25,5,5)
m[1,]<-sapply(-2:2,function(x) likertlmprob(model.Suesse, data.frame(Mahlgrad=1, Mehlmenge=80, Nasspressen=20, Suesse=x)))
m[2,]<-sapply(-2:2,function(x) likertlmprob(model.Saeure, data.frame(Mahlgrad=1, Mehlmenge=80, Nasspressen=20, Saeure=x)))
m[3,]<-sapply(-2:2,function(x) likertlmprob(model.Bitterkeit, data.frame(Mahlgrad=1, Mehlmenge=80, Nasspressen=20, Bitterkeit=x)))
m[4,]<-sapply(-2:2,function(x) likertlmprob(model.Koerper, data.frame(Mahlgrad=1, Mehlmenge=80, Nasspressen=20, Koerper=x)))
m[5,]<-sapply(-2:2,function(x) likertlmprob(model.Geschmack, data.frame(Mahlgrad=2, Mehlmenge=80, Nasspressen=20, Geschmack=x)))
rownames(m)<-c("Suesse", "Saeure", "Bitterkeit", "Koerper", "Geschmack")

streamMatrix<-function(Mahlgrad=x,Mehlmenge=y){
  m<-matrix(1:25,5,5)
  m[1,]<-sapply(-2:2,function(z) likertlmprob(model.Suesse, data.frame(Mahlgrad, Mehlmenge, Nasspressen=20, Suesse=z)))
  m[2,]<-sapply(-2:2,function(z) likertlmprob(model.Saeure, data.frame(Mahlgrad, Mehlmenge, Nasspressen=20, Saeure=z)))
  m[3,]<-sapply(-2:2,function(z) likertlmprob(model.Bitterkeit, data.frame(Mahlgrad, Mehlmenge, Nasspressen=20, Bitterkeit=z)))
  m[4,]<-sapply(-2:2,function(z) likertlmprob(model.Koerper, data.frame(Mahlgrad, Mehlmenge, Nasspressen=20, Koerper=z)))
  m[5,]<-sapply(-2:2,function(z) likertlmprob(model.Geschmack, data.frame(Mahlgrad, Mehlmenge, Nasspressen=20, Geschmack=z)))
  return(m)
}

mameVcl<-Vectorize(streamMatrix)
resMatrix<-mameVcl(rep(seq(0,3,by=0.5),times=5),rep(seq(50,90,by=10),each=7))

streamPlot <- function(data) {
  # Color palette
#   nColors <- 10
#   pal <- colorRampPalette(c("#0f7fb4", "white"))
#   colors <- pal(nColors)
 # colors<-c("red", "green", "blue", "brown", "purple")
#   colorRampSg <- function(n) {
#     pal <- colorRampPalette(c("#1C86EE", "#FF8247", "#008B45", "#FF0000", "#7B68EE", 
#                               "#8B4513"))
#     col <- pal(n + 2)[(1:n) + 1]
#     return(col)  
#   }
#   colors<-colorRampSg(10)
  colors<-c("#1C86EE", "#FF8247", "#458B00", "#FF0000", 
    "#8B5742")
  sortedData <- data[1,]
  weights <- rowSums(data)
  topWeight <- weights[1]
  bottomWeight <- weights[1]
  
  colorindex<-c(1)
  if (length(data[,1]) > 1) {
    for (i in 2:length(data[,1])) {
      
      if (topWeight > bottomWeight) {
        sortedData <- rbind(sortedData, data[i,])
        topWeight <- topWeight + weights[i]
        colorindex<-c(colorindex,i)
      } else {
        sortedData <- rbind(data[i,], sortedData)
        bottomWeight <- bottomWeight + weights[i]
        colorindex<-c(i,colorindex)
      }
    }
  }
  
  
  
  # Convert sorted data to splines.
  
  # Helper function to change negative valus to zero
  zeroNegatives <- function(x) { 
    if (x < 0) { return(0) }
    else { return(x) }
  }
  
  # Initialize smoothed data
  firstRow <- spline(1:length(sortedData[1,]), sortedData[1,], 200)$y
  firstRow <- sapply(firstRow, zeroNegatives)
  
  smoothData <- data.frame( rbind(firstRow, rep(0, length(firstRow))) )
  smoothData <- smoothData[1,]
  
  if (length(sortedData[,1]) > 1) {
    for (i in 2:length(sortedData[,1])) {
      
      splinerow <- spline(1:length(sortedData[i,]), sortedData[i,], 200)$y
      splinerow <- sapply(splinerow, zeroNegatives)
      smoothData <- rbind(smoothData, splinerow)
    }
  }
  
  # Optimize vertical offset
  n <- length(smoothData[,1])
  i <- 1:length(smoothData[,1])
  parts <- (n - i + 1) * smoothData
  theSums <- colSums(parts)
  yOffset <- -theSums / (n + 1)
  
  # Totals at each index for axis upper and lower bounds
  totals <- colSums(smoothData)
  yLower <- min(yOffset)
  yUpper <- max(yOffset + totals)
  
  # Max, min, and span of weights for each layer
  maxRow <- max(rowSums(smoothData))
  minRow <- min(rowSums(smoothData))
  rowSpan <- if ( (maxRow - minRow) > 0 ) { maxRow - minRow } else { 1 }
  
  # Make the graph.
  #plot(1:10, 1:10, type="n", xlim=c(1, length(smoothData[1,])), ylim=c(yLower, yUpper), xlab=NA, ylab=NA)
  plot(1:10, 1:10, type="n", xlim=c(1, length(smoothData[1,])), ylim=c(yLower, yUpper), axes=FALSE, xlab=NA, ylab=NA)
  for (i in 1:length(smoothData[,1])) {
    #colindex <- floor( (nColors-2) * ( (maxRow - sum(smoothData[i,])) / rowSpan ) ) + 1
    
    # Wireframe debugging	
    # points(1:length(smoothData[i,]), smoothData[i,] + yOffset, col=i)
    # lines(1:length(smoothData[i,]), smoothData[i,] + yOffset, col=i)
    
    polygon(c(1:length(smoothData[i,]), length(smoothData[i,]):1), c(smoothData[i,] + yOffset, rev(yOffset)), col=colors[colorindex[i]], border="#ffffff", lwd=0.2)
    
    yOffset <- yOffset + smoothData[i,]
  }
  
}


par(mfrow=c(5,7),mar=c(1,1,1,1))
for (i in 1 : ncol(resMatrix) ) {
  streamPlot(matrix(resMatrix[,i],5,5))
}
