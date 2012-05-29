make.polynomial.disc <-
function (f, k, moments = NA) 
{
# makes orthonormal polynomials from order 0 up to k
h.all<-list()
a<-matrix(0,nrow=k+1,ncol=k+1) # rows=order of x, cols=order of poly
cte<-rep(0,k+1)
    h <- function(x) {
        y <- 1
        return(y)
    }
    a[1,1]<-1
    cte[1]<-1
    h.all[[1]]<-h
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
          h <- function(x) {
            y <- 0
            for (j in 0:r) {
                y <- y + a[j+1,r+1]*x^j
            }
            y <- y/sqrt(cte[r+1])
            return(y)
          }
          h.all[[r+1]]<-h
        }
    }
    return(h.all)
}

