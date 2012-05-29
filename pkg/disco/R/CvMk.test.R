CvMk.test <-
function(data) {
	# CHECK! No ref to literature found
	k<-length(data)
	z<-data[[1]]
	for(s in 2:k) {
		z<-c(z,data[[s]])
	}
	z<-sort(z)
	n<-length(z)
	tmp<-0
	for(s in 1:k) {
		n1<-length(data[[s]])
		x1<-data[[s]]
		A<-0
  		for(i in 1:(n-1)) {
  			Mi<-sum(x1<=z[i])
  			A<-A+(n*Mi-n1*i)^2
  		}
  		tmp<-tmp+A/n1
	}
	tmp<-tmp/n^3
	invisible(tmp)
}

