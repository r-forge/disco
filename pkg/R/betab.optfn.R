betab.optfn <-
function(pars,x,ntrials,n){
	a<-pars[1] #this is alpha
	b<-pars[2] #this is beta
	t1<-sum(digamma(x+a))-n*digamma(a+ntrials+b)-n*digamma(a)+n*digamma(a+b)
	t2<-sum(digamma(ntrials-x+b))-n*digamma(a+ntrials+b)-n*digamma(b)+n*digamma(a+b)
	return(c(t1,t2))
}

