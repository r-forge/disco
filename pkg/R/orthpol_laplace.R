orthpol_laplace <-
function(k,alpha,beta) {
	#location-scale family!
	if (k==0) {
        h<-function(x){
          y<-1
          return(y)
        }
      }
	else if (k==1) {
        h<-function(x){
          z<-(x-alpha)/beta
          y<-z/sqrt(2)
          return(y)
        }
      }
	else if (k==2) {
        h<-function(x){
          z<-(x-alpha)/beta
          y<-(z^2-2)/(2*sqrt(5))
          return(y)
        }
      }
	else if (k==3) {
        h<-function(x){
          z<-(x-alpha)/beta
          y<-(z^3-12*z)/(12*sqrt(3))
          return(y)
        }
      }
	else if (k==4) {
        h<-function(x){
          z<-(x-alpha)/beta
          y<-1/3576*sqrt(745)*(z^4-168/5*z^2+216/5)
          return(y)
        }
      }
	return(h)
}

