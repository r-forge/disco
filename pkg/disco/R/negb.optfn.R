negb.optfn <-
function(r,x){
  n<-length(x)
  x.bar<-mean(x)
  k<-max(x)
  tmp<-rep(0,k)
  N<-rep(0,k)
  for (i in 1:k){
	h<-0
	for (j in 0:(i-1)){
	  h<-h+j/(1+j/r)
	}
	tmp[i]<-h
	N[i]<-sum(x==i)
  }
  (n*r^2*log(1+x.bar/r)-x.bar*n*(x.bar+r)/(1+x.bar/r)+sum(N*tmp))^2
}

