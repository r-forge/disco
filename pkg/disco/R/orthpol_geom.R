orthpol_geom <-
function(k,p) {
	if (k==0) {
        h<-function(x){
          y<-1
          return(y)
        }
      }
	else if (k==1) {
        h<-function(x){
          y<-(x*p-1+p)/(-(-1+p))^(1/2)
          return(y)
        }
      }
	else if (k==2) {
        h<-function(x){
          y<-1/2*(3*p^2*x+p^2*x^2+2*p^2-4*p*x-4*p+2)/((-1+p)^2)^(1/2)
          return(y)
        }
      }
	else if (k==3) {
        h<-function(x){
          y<--1/6/(-(-1+p)^9)^(1/2)*(6-36*p+90*p^2-120*p^3+90*p^4+9*x^2*p^2-36*p^5+6*p^6-3*p^5*x^3+45*p^4*x^2-60*p^5*x+11*p^6*x+6*p^6*x^2+132*p^4*x-27*p^5*x^2+p^6*x^3-33*x^2*p^3+81*x*p^2-146*x*p^3-x^3*p^3+3*x^3*p^4-18*x*p)
          return(y)
        }
      }
	else if (k==4) {
        h<-function(x){
          y<-1/24*(p^4*x^4+10*p^4*x^3-16*p^3*x^3-96*p^3*x^2+35*p^4*x^2+72*p^2*x^2+216*p^2*x+50*p^4*x-176*p^3*x-96*p*x+24*p^4+144*p^2-96*p^3-96*p+24)/((-1+p)^4)^(1/2)
          return(y)
        }
      }
	return(h)
}

