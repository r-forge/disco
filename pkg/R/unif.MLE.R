unif.MLE <-
function(sample){
  MIN<-min(sample)
  MAX<-max(sample)
  return(c(MIN,MAX))
}

