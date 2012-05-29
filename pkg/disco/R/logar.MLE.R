logar.MLE <-
function(sample){
  x.bar<-mean(sample)
  tmp<-optimize(logar.optfn,x.bar=x.bar,interval=c(0,1),tol=.Machine$double.eps^10)
  pars<-tmp$minimum
  return(pars)
}

