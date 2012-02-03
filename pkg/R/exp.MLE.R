exp.MLE <-
function(sample){
  LAMBDA<-1/mean(sample)
  return(LAMBDA)
}

