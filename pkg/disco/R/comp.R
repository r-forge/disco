comp <-
function(x1,x2,fn) {
	n1<-length(x1)
	n2<-length(x2)
	x<-c(x1,x2)
	r<-(rank(x)-0.5)/(n1+n2)
	cmp<-(mean(fn(r[1:n1]))-mean(fn(r[(n1+1):(n1+n2)])))*sqrt(n1*n2/(n1+n2))
	return(cmp)
}

