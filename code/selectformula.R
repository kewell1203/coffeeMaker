selectMod<-function(data, fm, newdata = NULL, slices = nrow(data), 
                    random = (slices != nrow(data)), details = FALSE, slices2 = NULL,
                    grad, partGrads, ...){
    if(any(grad<partGrads)){
      stop("partGrads must less than grad ")
    }
    
    fm <- as.formula(fm)
    independents <- attr(terms(fm), "term.labels")
    dependent <- as.character(attr(terms(fm), "variables")[[2]])
    data<-na.omit(data[,c(dependent,independents)])
    
    t<-as.matrix(expand.grid(lapply(partGrads,function(x){seq(0,x)})))
    t<-t[rowSums(t)>0&rowSums(t)<=grad,]
    
    formset<- function(args ,vec){
      res<-NULL
      res<-args[vec==1]
      if(any(vec>1)){
        agt1<-args[vec>1]
        ggt1<-vec[vec>1]
        pre<-paste("I(",agt1,"^",ggt1,")",sep="")
        res<-c(res,pre)
      }
      if(length(res)==1){
        return(res)
      }else{
        res<-paste(res,collapse=":")
        return(res)
      }
    }
    
    require(rje)
    fset<-powerSet(apply(t,1,function(x){formset(independents,x)}))
    fset[[1]]<-NULL
    forms<-lapply(fset,function(x){paste(dependent,"~",paste(x,collapse="+",sep=""))})
    forms<-unlist(forms)
    
    res <- sapply(forms, 
                  function(f) {mse<-checkcrossval(f, data, newdata, slices, random, details, slices2)    
                               c(f,mse)
                               })
    
    res<-t(as.matrix(res))
    rownames(res)<-NULL
    colnames(res)<-c("formula","crossvalidated mse")
    res<-res[order(res[,c("crossvalidate mse")]),]
    return(res)
}