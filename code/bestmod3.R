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

set<-apply(t,1,function(vec){formset(independents,vec)})