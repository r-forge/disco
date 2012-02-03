approx.perm.k <-
function(data,stat,B) {
	k<-length(data)
	z<-data[[1]]
	n<-c(0,length(data[[1]]))
	for(s in 2:k) {
		z<-c(z,data[[s]])
		n<-c(n,n[s]+length(data[[s]]))
	}
	d<-c()
	data2<-data
	for(i in 1:B) {
		ind<-sample(1:length(z))
		data2<-lapply(1:k,function(j,ind2,z2,n2) {
			 z2[ind2[(n2[j]+1):n2[j+1]]]}, ind2=ind,z2=z,n2=n)
		d<-c(d,stat(data2))
	}
	p<-mean(d>=stat(data))
	invisible(p)
}

