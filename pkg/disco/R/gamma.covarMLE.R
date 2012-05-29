gamma.covarMLE <-
function(a) { # shape a (independent of scale b)
	C<-matrix(0,ncol=3,nrow=3)
	C[1,1]<-1/2*(2*trigamma(a)*a^2-2*a+2*trigamma(a)*a-3)/(a+1)/(trigamma(a)*a-1)
	C[1,2]<-1/3*1/(a+1)/(trigamma(a)*a-1)*3^(1/2)/(a+2)^(1/2)
	C[2,1]<-C[1,2]
	C[1,3]<--1/2*1/(a+1)/(trigamma(a)*a-1)*3^(1/2)/(a+2)^(1/2)/(a+3)^(1/2)
	C[3,1]<-C[1,3]
	C[2,2]<-1/3*(9*trigamma(a)*a^2-9*a+6*trigamma(a)*a-8+3*a^3*trigamma(a)-3*a^2)/(a+1)/(a+2)/(trigamma(a)*a-1)
	C[2,3]<-1/((a+1)*(a+2)*(trigamma(a)*a-1)*(a+3)^(1/2))
	C[3,2]<-C[2,3]
	C[3,3]<-1/2*(2*a^4*trigamma(a)-2*a^3+12*a^3*trigamma(a)-12*a^2+22*trigamma(a)*a^2-22*a+12*trigamma(a)*a-15)/(a+3)/(a+2)/(a+1)/(trigamma(a)*a-1)
	return(C)
}

