print.componentscdk <-
function(obj) {
	squared<-obj$squared
	order<-obj$order
	mo<-obj$x$min.order
	cat("\n")
	if(squared&(obj$x$K<=2)) {
		cat("squared ")
	}
	cat("components\n")
	if((obj$x$K==2)|(obj$x$K==1)) {
	  if(squared) {
	  	cmp<-obj$x$comp
	  }
	  if(!squared) {
	  	cmp<-obj$x$coefficients
	  }
  	  for(i in 1:order) {
		if(i>=mo) {
			cat("    ",i,ist(i),"component =",sgnspc(cmp[i-mo+1]),cmp[i-mo+1]," p-value =",round(obj$x$p.value[i-mo+1],4),"\n")
		}
      }
    }
    if(obj$x$K>2) {
      n.sample<-nrow(obj$m)
      for(i in 1:n.sample) {
        cat("sample ",i,": ",sep="")
        for(j in 1:order) {
        	cat(sgnspc(obj$m[i,j]),round(obj$m[i,j],4),"  ",sep="")
        }
        if(obj$contrast[1]=="sum") cat(" * ",round(sum(obj$m[i,1:order]^2),4),sep="")
        cat("\n")
      }	
      cat("\n")
      if(obj$contrast[1]=="sum") cat("comp    : ",round(obj$x$comp[1:order],4),"\n",sep="  ")
    } 
	invisible(obj)	
}

