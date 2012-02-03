prepare.poly.disc <-
function (f, k) 
# computes moments
{
    mu <- rep(0, 2 * k )
    for (i in 1:(2 * k )) {
        x<-0:500
	  mu[i]<-sum(x^i*f(x))  
    }
    return(mu)
}

