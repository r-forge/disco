orthpol_ZIP <-
function(k,p,lambda) {
	if (k==0) {
        h<-function(x){
          y<-1
          return(y)
        }
      }
	else if (k==1) {
        h<-function(x){
          y<-(x+(-1+p)*lambda)/(-lambda^2*p^2+lambda-lambda*p+lambda^2*p)^(1/2)
          return(y)
        }
      }
	else if (k==2) {
        h<-function(x){
          y<-(x^2*(lambda*p+1)-x*(lambda^2*p+lambda*p+2*lambda+1)-lambda^2*p+lambda^2)/((1-p)*(2*lambda*p+lambda^2*p+2)*lambda^2*(lambda*p+1))^(1/2)
          return(y)
        }
      }
	else if (k==3) {
        h<-function(x){
          y<-1/2*lambda^3*2^(1/2)*((2*lambda*p+2+2*p^2+lambda^2*p+2*lambda*p^3-2*lambda^2*p^2-4*p-4*lambda*p^2+lambda^2*p^3)*x^3+(6*lambda*p+12*p+4*lambda^3*p^2-6*p^2-9*lambda^2*p-6+18*lambda^2*p^2+6*lambda*p^2-6*lambda-9*lambda^2*p^3-2*lambda^3*p-2*lambda^3*p^3-6*lambda*p^3)*x^2+(p*lambda^4-12*lambda^3*p^2-2*lambda^4*p^2+6*lambda+6*lambda^3*p+4+4*p^2-2*lambda*p^2+8*lambda^2*p^3+lambda^4*p^3+6*lambda^2+4*lambda*p^3+6*lambda^3*p^3-4*lambda^2*p-10*lambda^2*p^2-8*p-8*lambda*p)*x-2*lambda^3-6*lambda^3*p^2+6*lambda^3*p+2*lambda^3*p^3)/(-(-1+p)^5*lambda^9*(2*lambda*p+lambda^2*p+2)*(3*lambda^2*p+lambda^3*p+6*lambda*p+6))^(1/2)
          return(y)
        }
      }
	else if (k==4) {
        h<-function(x){
          y<-1/6*((3*lambda^2*p+lambda^3*p+6*lambda*p+6)*x^4+(-42*lambda^2*p-24*lambda-3*p*lambda^4-18*lambda^3*p-36*lambda*p-36)*x^3+(72*lambda+66+83*lambda^3*p+66*lambda*p+36*lambda^2+3*p*lambda^5+24*p*lambda^4+105*lambda^2*p)*x^2+(-66*lambda^2*p-24*lambda^3-p*lambda^6-36*lambda*p-9*p*lambda^5-48*lambda-36*lambda^2-36-39*p*lambda^4-66*lambda^3*p)*x-6*p*lambda^4+6*lambda^4)*6^(1/2)/((1-p)*lambda^4*(4*lambda^3*p+24*lambda*p+p*lambda^4+12*lambda^2*p+24)*(3*lambda^2*p+lambda^3*p+6*lambda*p+6))^(1/2)
          return(y)
        }
      }
	return(h)
}

