gajek.d <-
function(f,g,lower=0,upper=1,N=100) {
 # gajek correction for discrete distributions
 sx<-lower:upper
 f.sx<-f(sx)
 eps.s<-seq(0,max(f.sx),1/N)
 integrals<-c()
 for(eps in eps.s) {
   fc<-function(x) {
     d<-f(x)-eps*g(x)
     d[d<0]<-0
     return(d)  
   }
   i<-sum(fc(lower:upper))
   integrals<-c(integrals,i)
 }
 errors<-abs(integrals-1)
 error<-min(errors)

 int<-integrals[errors==error]
 epsilon<-eps.s[errors==error]
 fc<-function(x) {
     d<-f(x)-epsilon*g(x)
     d[d<0]<-0
     return(d)  
 }
 r<-list(error=error,integral=int,epsilon=epsilon,fc=fc)
 return(r)
}

