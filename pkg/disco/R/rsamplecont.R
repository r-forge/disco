rsamplecont <-
function(n,f) { 
  U<-runif(n)
  T<-rep(0,n)
  cdf<-function(x){
    temp<-try(integrate(f,-Inf,x)$value,silent=TRUE)
    if (inherits(temp,"try-error")) {temp<-100}
    return(temp)
  }
  optfn<-function(x,y) {
    (cdf(x)-y)
  }
  for (i in 1:n) {
    tmp<-nls.lm(0,optfn,y=U[i])
    T[i]<-tmp$par
  }
  return(T)
}

