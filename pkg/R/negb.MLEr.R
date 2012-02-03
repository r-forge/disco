negb.MLEr <-
function(sample){
  r<-negb.MME(sample)[2]
  tmp<-optimize(negb.optfn,x=sample,interval=c(max(r-10,0),r+10),tol=.Machine$double.eps^10)
  pars<-tmp$minimum  
  return(pars)
}

