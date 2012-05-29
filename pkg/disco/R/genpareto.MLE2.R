genpareto.MLE2 <-
function(sample) { #methode van Choulakian (met herschaling)
	n<-length(sample)
	xbar<-mean(sample)
	x<-sample/xbar
	thetapos<-seq(-4*xbar,1/max(x),length.out=4000)
	start<- -n-sum(log(1-thetapos[1]*x))-n*log(-sum(log(1-thetapos[1]*x))/(n*thetapos[1]))
	thetadef<- thetapos[1]
	thetapos<-thetapos[-c(1,4000)]
	for (theta in thetapos) {
		tmp<--n-sum(log(1-theta*x))-n*log(-sum(log(1-theta*x))/(n*theta))
		if (tmp>start) {	
			start<-tmp
			thetadef<-theta
		}
	}
	theta<-thetadef
	k <- -sum(log(1-theta*x))/n
	s <- k/theta*xbar
	return(c(s,k))
}

