partialsum.sequence <-
function(sample,S,min.order,...) {
  # returns all score statistics within the horizon S
  # min.order depends on method of estimation and distribution; 
  #   it is determined in ddsmooth.test
  max.order<-max(unlist(S))
  #JM - HAS TO CHANGE - infinite recursion
  st<-smooth.test(sample,order=max.order,B=NULL,output=F,...)
  Sigma<-st$Sigma
  U<-matrix(st$comp,ncol=1) #$comp are now the unscaled components
  theta<-U/sqrt(length(sample))
  stats<-rep(NA,length(S))
  for(i in 1:length(S)) {
    ind<-S[[i]]-min.order+1
    temp<-try(t(U[ind])%*%solve(Sigma[ind,ind])%*%U[ind],silent=TRUE)
    if (!inherits(temp,"try-error")) { #det(Sigma[ind,ind]) \neq 0
	stats[i]<-temp
    }
    else {
	stats[i]<-NA
    }
  }
  return(list(stats=stats,theta=theta))
}

