logis.covarMME <-
function() { # independent of nuisance parameters
	I <- diag(rep(1,2))
 	S_t1b<-matrix(c(sqrt(3)/pi,0,
                  0,2/sqrt(3.2)),nrow=2,byrow=TRUE)
 	S_t2b<-matrix(c(-sqrt(7)/(6*pi),0,
                  0,-1/3),nrow=2,byrow=TRUE)
      K <- S_t2b %*% solve(S_t1b)
      C <- I + K %*% t(K)
	# a little less precise: 
	#C<-matrix(c(1.064815,0,
      #            0,1.088889),ncol=2,byrow=TRUE)
	return(C)
}

