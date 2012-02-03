gamma.covarMME <-
function(a) { # shape a (independent of scale b)
	C<-matrix(0,ncol=2,nrow=2)
	C[1,1]<-1/3*(3*a+10)/(a+2)
	C[1,2]<--2/(a+2)/(a+3)^(1/2)
	C[2,1]<-C[1,2]
	C[2,2]<-(a^2+5*a+9)/(a+2)/(a+3)
	return(C)
}

