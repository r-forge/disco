genpareto.MLE <-
function(sample) {
	n<-length(sample)
	par_start<-genpareto.MME(sample)
	tmp <- try(nls.lm(par_start[2]/par_start[1], genpareto.optfn, sample = sample, n = n),silent=TRUE)
	if (!inherits(tmp,"try-error")) {
	  theta <- tmp$par
	  k <- -sum(log(1-theta*sample))/n
	  s <- k/theta
	  pars <- c(s, k)
	  if (is.nan(pars[1])||is.nan(pars[2])||is.na(pars[1])||is.na(pars[2])||is.infinite(pars[1])||is.infinite(pars[2])) {
		pars<-genpareto.MLE2(sample)
	  }
	  else {
		W1<-(1-k)/s^2*sum(sample/(1-k*sample/s)) - n/s
		W2<- -sum(log(1-k*sample/s))/k^2 - (1/k-1)/s*sum(sample/(1-k*sample/s))
		if (!is.na((W1^2+W2^2)>1)) {
			if ((W1^2+W2^2)>1) {pars<-genpareto.MLE2(sample)}
		}
	      else {pars<-genpareto.MLE2(sample)}
	  }
	}
	else {pars<-genpareto.MLE2(sample)}
	s <- pars[1]
	k <- pars[2]
	return(c(s, k))
}

