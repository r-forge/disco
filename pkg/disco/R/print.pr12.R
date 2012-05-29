print.pr12 <-
function(x,...) {
  cat("\n",x$method,"\n\n")
  if(!is.matrix(x$estimate)) {
  	cat("X1: ",x$xname," X2: ",x$yname,"\n\n")
  	cat("Estimate = ",round(x$estimate,4),"\n")
  	cat("The ",1-x$alpha," confindence interval:\n")
  	cat(round(x$ci.bounds[1],4),round(x$ci.bounds[2],4),"\n")
  }
  if(is.matrix(x$estimate)) {
  	cat("X1<=X2\t","estimate\t","[lower\t\t","upper] at ",1-x$alpha,"conf. level \n")
  	n<-nrow(x$estimate)
  	for(i in 1:(n-1)) {
  		for(j in (i+1):n) {
  			cat(x$xname[i],"<=",x$yname[j],"\t",formatC(round(x$estimate[i,j],4),format="f"),"\t",formatC(round(x$ci.bounds[j,i],4),format="f"),"\t",formatC(round(x$ci.bounds[i,j],4),format="f"),"\n")
  		}
  	}
  }
  invisible(x)
}

