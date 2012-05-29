extrval.optfn <-
function(pars,x,n){
 t1<-n-sum(exp((pars[1]-x)/pars[2]))
 t2<-sum((pars[1]-x)*exp((pars[1]-x)/pars[2]))+sum(x)-n*pars[1]-n*pars[2]
 return(c(t1,t2))
}

