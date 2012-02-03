laplace.covarMLE <-
function() { # independent of nuisance parameters
	C<-matrix(c(1/2, 0, 1/12*2^(1/2)*3^(1/2), 0, 
			0, 1/5, 0, 16/3725*5^(1/2)*745^(1/2), 
			1/12*2^(1/2)*3^(1/2), 0, 11/12, 0,
			0, 16/3725*5^(1/2)*745^(1/2), 0, 681/745), ncol=4, byrow=TRUE)
	return(C)
}

