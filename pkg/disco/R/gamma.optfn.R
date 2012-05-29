gamma.optfn <-
function(pars,sample) { #pars is shape a
	if (pars>0) {
	temp<-log(pars)-digamma(pars)-log(mean(sample))+mean(log(sample))
	}
	else {temp<-Inf}
	return(temp)
}

