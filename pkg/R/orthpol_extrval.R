orthpol_extrval <-
function(k,a,b) {
	#location-scale family!
	if (k==0) {
        h<-function(x){
          y<-1
          return(y)
        }
      }
	else if (k==1) {
        h<-function(x){
          z<-(x-a)/b
          y<-0.7796968012*z-0.4500532075
          return(y)
        }
      }
	else if (k==2) {
        h<-function(x){
          z<-(x-a)/b
          y<-.3451996482*(z-.5772156649)^2-.5045182395*z-.2766148303
          return(y)
        }
      }
	else if (k==3) {
        h<-function(x){
          z<-(x-a)/b
          y<-.1060499473*(z-.5772156649)^3-.4944037009*(z-.5772156649)^2-.2194200910*z+.6849580626
          return(y)
        }
      }
	else if (k==4) {
        h<-function(x){
          z<-(x-a)/b
          y<-.2493263979e-1*(z-.5772156649)^4-.2416834756*(z-.5772156649)^3+.2690771450*(z-.5772156649)^2+.7769092062*z-.6743236152
          return(y)
        }
      }
	return(h)
}

