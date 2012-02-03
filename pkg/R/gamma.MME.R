gamma.MME <-
function(sample) {
	MEAN <- mean(sample)
	VAR <- sum((sample - MEAN)^2)/length(sample)
	a<-MEAN^2/VAR
	b<-VAR/MEAN
	return(c(a,b)) # shape a and scale b
}

