var.mw <-
function(x1,x2) {
  n1<-length(x1)
  n2<-length(x2)

  #Pr(x1<=x2) 
  p12<-as.numeric(1-wilcox.test(x1,x2)$stat/(n1*n2))

  # Pr(max(x11,x12)<x2)
  tmp<-expand.grid(x1,x1)
  mx<-pmax(tmp[,1],tmp[,2])
  remove("tmp")
  tmp<-expand.grid(mx,x2)
  p112<-mean(tmp[,1]<=tmp[,2])
  remove("tmp") 
  # Pr(max(x21,x22)<x1)
  tmp<-expand.grid(x2,x2)
  mx<-pmax(tmp[,1],tmp[,2]) 
  remove("tmp") 
  tmp<-expand.grid(mx,x1)
  p221<-mean(tmp[,1]<=tmp[,2])
  remove("tmp") 

  v<-((n1-1)*(p112-p12^2)+(n2-1)*(p221-(1-p12)^2)+p12*(1-p12))/(n1*n2)
  return(list(var=v,mw=p12))
}

