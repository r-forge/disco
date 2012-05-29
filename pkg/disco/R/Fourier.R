Fourier <-
function(k) {
	even<-(k/2)==round(k/2,0)
	n<-k
	if(even) {
		h<-function(x) {
			y<-sqrt(2)*cos(2*pi*n*x)
			return(y)
		}
	}
	if(!even) {
		h<-function(x) {
			y<-sqrt(2)*sin(2*pi*n*x)
			return(y)
		}
	}
	if(k==0) {
		h<-function(x) {
			y<-1
			return(y)
		}
	}
	return(h)
}

