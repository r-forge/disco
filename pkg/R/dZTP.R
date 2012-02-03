dZTP <-
function(x,lambda) {
		if (!is.numeric(lambda) || !(lambda>0)) stop("\"lambda\" must be positive.")
            y<-lambda^x/(factorial(x) * (exp(lambda) - 1))
            y[x == 0]<-0
            return(y)
}

