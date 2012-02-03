logis.MLE <-
function(sample){
 n<-length(sample)
 tmp<-nls.lm(c(mean(sample),sqrt(3)*sqrt(sum((sample-mean(sample))^2)/n)/pi),logis.optfn,x=sample,n=n)
 pars<-tmp$par
 return(pars)
}

