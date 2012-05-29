genpareto.MME <-
function(sample) { #only valid if shape=k > -1/2
	MEAN<-mean(sample)
	VAR<-sum((sample-MEAN)^2)/length(sample)
	k = (MEAN^2/VAR - 1)/2
	s = MEAN*(MEAN^2/VAR + 1)/2
	return(c(s,k))
}

