gamma.MLE <-
function(sample) {
	xbar<-mean(sample)
	if (min(sample)<=0) {stop("There are negative sample observations so the MLE does not exist. ")}
	s<-log(xbar)-mean(log(sample))
	startpar<-(3-s+sqrt((s-3)^2+24*s))/(12*s) #alternatief is via gamma.MME startwaarde voor a bekomen
	tmp<-nls.lm(startpar, gamma.optfn, sample = sample)
	a <- tmp$par
	b <- xbar/a
	return(c(a,b)) # shape a and scale b
}

