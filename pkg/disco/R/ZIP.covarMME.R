ZIP.covarMME <-
function(lambda,p) { # rate lambda and prob p
	C<-matrix(0,ncol=2,nrow=2)
	C[1,1]<-(lambda^3*p+5*lambda^2*p+6*lambda*p+6+2*lambda)/(lambda^3*p+3*lambda^2*p+6*lambda*p+6)
	C[1,2]<--2*lambda^(3/2)*(lambda^2*p+2*lambda*p+2)*(lambda*p+1)/((lambda^2*p+2*lambda*p+2)*(lambda^3*p+3*lambda^2*p+6*lambda*p+6))^(1/2)*3^(1/2)/((lambda^4*p+24*lambda*p+4*lambda^3*p+12*lambda^2*p+24)*(lambda^3*p+3*lambda^2*p+6*lambda*p+6))^(1/2)
	C[2,1]<-C[1,2]
	C[2,2]<-(lambda^7*p^2+7*lambda^6*p^2+36*lambda^5*p^2+12*lambda^4*p+96*lambda^4*p^2+144*lambda^3*p^2+144*lambda^2*p^2+288*lambda*p+72*lambda^3*p+144*lambda^2*p+144+12*lambda^2)/(lambda^4*p+24*lambda*p+4*lambda^3*p+12*lambda^2*p+24)/(lambda^3*p+3*lambda^2*p+6*lambda*p+6)
	return(C)
}

