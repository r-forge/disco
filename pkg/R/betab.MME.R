betab.MME <-
function(sample,ntrials) { #the number of trials is known (fixed)
	n<-ntrials
	MEAN<-mean(sample)
	VAR<-sum((sample-MEAN)^2)/length(sample)
	alpha<-(n-MEAN-VAR/MEAN)*MEAN/(n*VAR/MEAN+MEAN-n)
	beta<-(n-MEAN-VAR/MEAN)*(n-MEAN)/(n*VAR/MEAN+MEAN-n)
	return(c(alpha,beta))
}

