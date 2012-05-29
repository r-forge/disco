logis.covarMLE <-
function() { # independent of nuisance parameters
	C<-matrix(c(1-9/pi^2,0,sqrt(21)/(2*pi^2),0,
                  0,1-45/(12+4*pi^2),0,3*sqrt(5)/(6+2*pi^2),
                  sqrt(21)/(2*pi^2),0,1-7/(12*pi^2),0,
                  0,3*sqrt(5)/(6+2*pi^2),0,1-1/(3+pi^2)),ncol=4,byrow=TRUE)
	return(C)
}

