MISE.sequence <-
function(sample,S,min.order,...) {
  # returns all theta, and the MISEs within the horizon S
  # min.order depends on method of estimation and distribution; 
  #   it is determined in ddsmooth.test
  max.order<-max(unlist(S))
  st<-smooth.test(sample,rescale=T,order=max.order,B=NULL,output=F,...) 
  Sigma<-st$Sigma
  EVar<-st$EVar/length(sample)
  theta<-st$comp/sqrt(length(sample))
  theta2<-theta^2
  U<-matrix(st$comp,ncol=1)
  stats<-rep(NA,length(S))
  mises<-stats
  for(i in 1:length(S)) {
    ind<-S[[i]]-min.order+1 # indices corresponding to S
    nind<-!(1:(max.order-min.order+1))%in%ind # indices corresponding to \bar{S}
    temp<-try(t(U[ind])%*%solve(Sigma[ind,ind])%*%U[ind],silent=TRUE)
    if (!inherits(temp,"try-error")) { #det(Sigma[ind,ind]) \neq 0
	stats[i]<-temp
	mises[i]<-sum(EVar[ind])+sum(theta2[nind])-sum(EVar[nind])
    }
    else {
	stats[i]<-NA
	mises[i]<-NA # to ensure that this model is removed from the set of models so it is surely not selected 
    }
  }
  return(list(mises=mises,stats=stats,theta=theta))
}

