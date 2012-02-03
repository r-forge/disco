make.coeff.disc <-
function(f, k, moments = NA, norm = T)
{
#function for (normalised) coefficients up to k-th order
a<-matrix(0,nrow=k+1,ncol=k+1) # rows=order of x, cols=order of poly
cte<-rep(0,k+1)
  a[1,1]<-1
  cte[1]<-1
  if(k>0) {
        if (is.na(moments[1])) {
		mus <- prepare.poly.disc(f, k)
   	  }
	  else {
	  	mus <- moments
	  }
	  for(r in 1:k) {
          a.cte<-coeff.pol(i=r,a=a,cte=cte,mus=mus)
          cte<-a.cte$cte
          a<-a.cte$a
        }
  }
  if (norm == T) {
    for (r in 0:k) {
        a[,r+1]<-a[,r+1]/sqrt(cte[r+1]) 
    }
  }
  return(a)
}

