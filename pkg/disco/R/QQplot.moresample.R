QQplot.moresample <-
function(x,y,m=max(length(x),length(y)),ref=T,add=F,xlab=NULL,ylab=NULL,...) {
  # x is reference distribution
  F1<-ecdf(x)
  F2<-ecdf(y)
  p1<-c()
  p2<-c()
  for(i in 1:m) {
    F1.inv<-inv.cdf(F1,(i-0.5)/m,x.range=sort(x))
    F2.inv<-inv.cdf(F2,(i-0.5)/m,x.range=sort(y))
    p1<-c(p1,F1.inv)
    p2<-c(p2,F2.inv)
  }
  if(add) {
    points(p1,p2,...)
  } 
  else {
  	XL<-xlab
  	YL<-ylab
  	if(is.null(xlab)) {XL<-deparse(substitute(x))}
  	if(is.null(ylab)) {YL<-deparse(substitute(y))}
  	plot(p1,p2,xlab=XL,ylab=YL,...)
  }
  if(ref) abline(c(0,1))
}

