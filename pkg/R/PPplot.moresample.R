PPplot.moresample <-
function(x,y,m=max(length(x),length(y)),ref=T,add=F,xlab=NULL,ylab=NULL,...) {
  # x is reference distribution
  F1<-ecdf(x)
  F2<-ecdf(y)
  p1<-c()
  p2<-c()
  for(i in 1:m) {
    F1.inv<-inv.cdf(F1,(i-0.5)/m,x.range=sort(x))
    p1<-c(p1,(i-0.5)/m)
    p2<-c(p2,F2(F1.inv))
  }
  if(add) {
    points(p1,p2,...)
  } 
  else {
    plot(p1,p2,xlab=xlab,ylab=ylab,ylim=c(0,1),xlim=c(0,1),...)
  }
  if(ref) abline(c(0,1))
}

