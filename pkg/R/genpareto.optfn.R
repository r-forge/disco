genpareto.optfn <-
function(pars,sample,n) { #pars is theta
	if (pars<(1/max(sample))) {
		k = -1/n*sum(log(1-pars*sample))	
		temp<-n/pars-(1/k-1)*sum(sample/(1-pars*sample))
	}
	else {temp<-Inf}
	return(temp)
}

