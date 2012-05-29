rsampledisc <-
function(n,f) { 
  U<-runif(n)
  T<-rep(0,n)
  U<-sort(U)
  quant<-0
  cdf<-f(quant)
  for (i in 1:n) {
    while (cdf<U[i]) {
	quant<-quant+1
	cdf<-cdf+f(quant) #cdf is on every moment cdf(quant)
    }
    T[i]<-quant
  }
  return(T)
}

