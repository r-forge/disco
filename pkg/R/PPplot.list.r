PPplot.list <- function(x,...){

  nx <- length(x)
  namesx <- names(x)
  
  
  dots <- list(...)
  
  if(nx < 2){
    dots$x <- x[[1]]
    warning('Only one level, so one-sample PP plot is used.')
    do.call('PPplot.onesample',dots)
  } else if(nx == 2){

    if(is.null(dots$xlab)){
      dots$xlab = paste('level:',namesx[1])
      dots$ylab = paste('level:',namesx[2])
    }
    dots$x <- x[[1]]
    dots$y <- x[[2]]
    do.call('PPplot.moresample',dots)
  
  } else if(nx >= 2) {
      dots$xlab <- dots$ylab <- ''
      dots$main <- ''
      
    	op <- par(mfrow=c(nx,nx))
    	on.exit(par(op))
    	for(i in 1:nx) {
    		for(j in 1:nx) {
    			if(i!=j) {
      			
            dots$x <- x[[i]]
            dots$y <- x[[j]]
            
            do.call('PPplot.moresample',dots)
    			}
    			else {
   			    hist(x[[i]],main=paste('level:',namesx[i]),xlab='')
    			}
    		}
    	}    
  } 
  invisible(NULL)

}