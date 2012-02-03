approx.perm <-
function(x,y,stat,B) {
	n1<-length(x)
	z<-c(x,y)
	d<-c()
	for(i in 1:B) {
		ind<-sample(1:length(z),n1)
		x1<-z[ind]
		y1<-z[-ind]
		d<-c(d,stat(x1,y1))
	}
	p<-mean(d>=stat(x,y))
	invisible(p)
}

