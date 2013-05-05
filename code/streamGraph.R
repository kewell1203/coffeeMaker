streamPlot <- function(data, pi, spline.steps) {
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
  names(colors) <- c("Suesse", "Saeure", "Bitterkeit", "Koerper", "Geschmack")
  colors <- colors[pi]
  sortedData <- data[1,]
  sortedData<-matrix(sortedData,ncol=5)
 
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
  
 
  # Helper function to change negative values to zero
  zeroNegatives <- function(x) { 
    if (x < 0) { return(0) }
    else { return(x) }
  }
  
  # Initialize smoothed data
  firstRow <- spline(1:length(sortedData[1,]), sortedData[1,], spline.steps)$y
  firstRow <- sapply(firstRow, zeroNegatives)
  
  smoothData <- data.frame( rbind(firstRow, rep(0, length(firstRow))) )
  smoothData <- smoothData[1,]
  
  if (length(sortedData[,1]) > 1) {
    for (i in 2:length(sortedData[,1])) {
      
      splinerow <- spline(1:length(sortedData[i,]), sortedData[i,], spline.steps)$y
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
    polygon(c(1:length(smoothData[i,]), length(smoothData[i,]):1), c(smoothData[i,] + yOffset, rev(yOffset)), col=colors[colorindex[i]], border="#ffffff", lwd=0.2)
    yOffset <- yOffset + smoothData[i,]
  }
  
}

streamMatrix<-function(Mahlgrad=x,Mehlmenge=y, pi=pi){
  n <- length(pi)
  m<-matrix(0,n,5)
  for(i in 1 : n)
  m[i,]<-sapply(-2:2, function(z) do.call('likertlmprob', list(as.name(paste("model.",pi[i],sep="")),data.frame(Mahlgrad, Mehlmenge, Nasspressen=20, Suesse=z, Bitterkeit=z, Saeure=z, Koerper=z, Geschmack=z)),envir=parent.frame(4)))
  return(m)
}

streamGraphplot <- function(form.postfix, data, plot.all=FALSE, plotitem=c("Suesse", "Saeure", "Bitterkeit", "Koerper", "Geschmack"), steps.x=7, steps.y=5,  from.x=0, 
                              to.x=3, from.y = 50, to.y =90, spline.steps=100, plot.margin=c(1,1,1,1),...)

  {
  model.Suesse <- do.call('likertlm', list(paste("Suesse~",form.postfix,sep=""), data))
  model.Saeure <- do.call('likertlm', list(paste("Saeure~",form.postfix,sep=""), data))
  model.Bitterkeit <- do.call('likertlm', list(paste("Bitterkeit~",form.postfix,sep=""), data))
  model.Koerper <- do.call('likertlm', list(paste("Koerper~",form.postfix,sep=""), data))
  model.Geschmack <- do.call('likertlm', list(paste("Geschmack~",form.postfix,sep=""), data))

#   model.Suesse <- likertlm(Suesse ~ Mehlmenge^2 + Mahlgrad^2 + Nasspressen, kaffee.main0)
#   model.Saeure <- likertlm(Saeure~Mahlgrad^2 + Mehlmenge^2 + Nasspressen, kaffee.main0)
#   model.Bitterkeit<-likertlm(Bitterkeit~Mahlgrad^2 + Mehlmenge^2 + Nasspressen, kaffee.main0)
#   model.Koerper <- likertlm(Koerper~Mahlgrad^2 + Mehlmenge^2 + Nasspressen, kaffee.main0)
#   model.Geschmack <- likertlm(Geschmack~Mahlgrad^2 + Mehlmenge^2 + Nasspressen, kaffee.main0)
  if(plot.all)
    pi<-c("Suesse","Saeure","Bitterkeit","Koerper","Geschmack")
  else{
    pi <- match.arg(plotitem, several.ok=TRUE)
  }
  mameGrid<-expand.grid(seq(from.x,to.x,length.out=steps.x),seq(from.y,to.y,length.out=steps.y))
  resMatrix<-apply(mameGrid,1,function(x) streamMatrix(x[1],x[2],pi=pi))
  par(mfrow=c(steps.y, steps.x),mar=plot.margin)
  for (i in 1 : ncol(resMatrix) ) {
    streamPlot(matrix(resMatrix[,i],ncol=5), pi, spline.steps)
  }
  
}