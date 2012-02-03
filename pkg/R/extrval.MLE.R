extrval.MLE <-
function(sample){
 n<-length(sample)
 b<-sqrt(6)*sqrt(sum((sample-mean(sample))^2)/n)/pi
 tmp<-nls.lm(c(mean(sample)+digamma(1)*b,b),extrval.optfn,x=sample,n=n)
 pars<-tmp$par
 return(pars)
}

