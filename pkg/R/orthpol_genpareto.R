orthpol_genpareto <-
function(k.degree,s,k) { #s is scale and k is shape
	if (k.degree==0) {
        h<-function(x){
          y<-1
          return(y)
        }
      }
	else if (k.degree==1) {
        h<-function(x){
          y<- -(-x-x*k+s)/(1+k)/(s^2/(1+k)^2/(1+2*k))^(1/2)
          return(y)
        }
      }
	else if (k.degree==2) {
        h<-function(x){
          y<- 1/2*(6*x^2*k^2+5*x^2*k-8*x*k*s+x^2+2*s^2-4*x*s)/(1+2*k)/(1+3*k)/(s^4/(1+4*k)/(1+3*k)^2/(1+2*k)^2)^(1/2)    
          return(y)
        }
      }
	else if (k.degree==3) {
        h<-function(x){
          y<--1/6/(s^6/(1+6*k)/(1+5*k)^2/(1+4*k)^4/(1+3*k)^6/(1+2*k)^6/(1+k)^4)^(1/2)*(-60*x^3*k^3+108*x^2*k^2*s-47*x^3*k^2-12*x^3*k-54*x*k*s^2+63*x^2*k*s+9*x^2*s-x^3+6*s^3-18*x*s^2)/(1+5*k)/(1+4*k)^2/(1+3*k)^3/(1+2*k)^3/(1+k)^2  
          return(y)
        }
      }
	else if (k.degree==4) {
        h<-function(x){
          y<- 1/24*(840*k^4*x^4+x^4+638*k^3*x^4+179*k^2*x^4+22*k*x^4-1184*k^2*s*x^3-1920*k^3*s*x^3-240*k*s*x^3-16*s*x^3+1440*k^2*s^2*x^2+72*s^2*x^2+648*k*s^2*x^2-384*k*s^3*x-96*s^3*x+24*s^4)/(1+7*k)/(1+6*k)/(1+5*k)/(1+4*k)/(s^8/(1+8*k)/(1+7*k)^2/(1+6*k)^2/(1+5*k)^2/(1+4*k)^2)^(1/2)
          return(y)
        }
      }
	return(h)
}

