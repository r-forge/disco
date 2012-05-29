print.cdk <-
function(x,...) {
	if(is.null(x$criterion)) {
     	if(x$K>1) {
     		cat("K-sample smooth goodness-of-fit test (K=",x$K,")\n\n",sep="")
     		cat("Data: ",x$DNAME,"\n\n",sep="")
     	}
     	if(x$K==1) {
     		cat("One-sample smooth goodness-of-fit test\n\n")
     		cat("Data: ",x$DNAME,"\n\n",sep="")
     		cat("Null hypothesis:",x$distr,"against",x$order,ist(x$order),"order alternative\n",sep=" ")
     		cat("Nuisance parameter estimation:",x$method.est,"\n",sep=" ")
     		cat("Parameter estimates:",x$par.est,"(",x$names.par,")\n\n",sep=" ")
     	}
     	cat("Smooth test statistic T_k =",round(x$stat,4)," p-value =",round(x$p.value[length(x$p.value)],4),"\n\n")
    }
    if(!is.null(x$criterion)) {
    	if(x$K>1) {
    		cat("Adaptive K-sample smooth goodness-of-fit test (K=",x$K,")\n\n",sep="")
    		cat("Data: ",x$DNAME,"\n\n",sep="")
    	}
    	if(x$K==1) {
    		cat("Adaptive One-sample smooth goodness-of-fit test\n\n",sep="")
    		cat("Data: ",x$DNAME,"\n\n",sep="")
    		cat("Null hypothesis:",x$distr,"against",x$order,ist(x$order),"order alternative\n",sep=" ")
     		cat("Nuisance parameter estimation:",x$method.est,"\n",sep=" ")
     		cat("Parameter estimates:",x$par.est,"(",x$names.par,")\n\n",sep=" ")
    	}	
    	cat("Horizon: ",x$horizon," selection (max. order = ",x$order,")\n",sep="")
    	cat("Selection rule:",x$criterion,"\n\n")
    	cat("Adaptive smooth test statistic T_k =",round(x$stat,4)," p-value =",round(x$p.value[length(x$p.value)],4),"\n")
    	cat("Selected components: ",paste(as.character(x$order.select),collapse=","),"\n\n")
    	#cat("Squared components\n")
    }
#    if(x$K>1) {
#    	seq.order<-1:x$order
#    }
#    if(x$K==1) {
#    	seq.order<-(length(x$par.est)+1):(length(x$par.est)+length(x$comp))
#    }
	cat("Squared components\n")
    for(i in x$order.select) {
   	  	cat("    ",i,ist(i),"component =",sgnspc(x$comp[i-x$min.order+1]),round(x$comp[i-x$min.order+1],4)," p-value =",round(x$p.value[i-x$min.order+1],4),"\n")
   	}
   	cat("\n")
   	cat("All p-values are obtained by",x$method.p.value,"\n\n")
   	
   	if(x$K==2) {
   	  cat("Estimation of likely orderings\n")
   	  cat("  Pr(X1<=X2) =",round(x$diagnose$pr12$estimate,4),"\n")
   	  cat("  Pr(max(X11,X12)<=X2) =",round(x$diagnose$order2$p112,4),"\n")
   	  cat("  Pr(max(X11,X12)<=X2) =",round(x$diagnose$order2$p221,4),"\n")  
   	}
   	invisible(x)
}

