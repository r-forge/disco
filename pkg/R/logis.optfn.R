logis.optfn <-
function(pars,x,n){
 t2<-(pars[2]-(1/n)*sum((x-pars[1])*(1-exp(-(x-pars[1])/pars[2]))/(1+exp(-(x-pars[1])/pars[2]))))
 t1<- (n/2-sum(exp(-(x-pars[1])/pars[2])/(1+exp(-(x-pars[1])/pars[2]))))
 return(c(t1,t2))
}

