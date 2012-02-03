ZTP.optfn <-
function(lambda,x.bar){
  y<-Inf
  if (lambda>0) {
   y<-x.bar-lambda/(1-exp(-lambda))
  }
  return(y)
}

