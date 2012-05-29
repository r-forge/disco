laplace.covarMME <-
function() { # independent of nuisance parameters
	C<-matrix(c(7/6, 0,
		 	0, 165/149),nrow=2,byrow=TRUE)
	return(C)
}

