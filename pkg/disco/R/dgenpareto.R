dgenpareto <-
function(x,s,k){
	  if (!is.numeric(s) || !(s>0)) stop("\"scale\" must be positive.")
        f = x
        f[x<0] = 0
        f[x>=0] = (1/s)*(1-k*x[x>=0]/s)^((1-k)/k)
        if (k>0) { f[x>=s/k] <-0 }
	  if (k==0) { f<- dexp(x,1/s) }
        return(f)
}

