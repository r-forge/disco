betab.MLE <-
function(sample,ntrials) {
	n<-length(sample)
	tmp<-nls.lm(betab.MME(sample,ntrials),betab.optfn,x=sample,ntrials=ntrials,n=n)	
	pars<-tmp$par
	return(pars) #these pars are the estimations of alpha and beta
}

