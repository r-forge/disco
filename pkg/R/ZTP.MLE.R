ZTP.MLE <-
function(sample){
  x.bar<-mean(sample)
  tmp<-nls.lm(x.bar,ZTP.optfn,x.bar=x.bar)
  pars<-tmp$par
  return(pars)
}

