unif.covarMME <-
function() { # independent of nuisance parameters
	C<-matrix(0,ncol=2,nrow=2)
	C[1,1]<-10/3
	C[2,2]<-2.8
	return(C)
}

