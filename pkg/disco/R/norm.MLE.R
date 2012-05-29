norm.MLE <-
function(sample){
  MEAN<-mean(sample)
  SD<-sqrt(sum((sample-MEAN)^2)/length(sample))
  return(c(MEAN,SD))
}

