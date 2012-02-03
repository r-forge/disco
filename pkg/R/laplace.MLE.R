laplace.MLE <-
function(sample){
  a<-median(sample)
  b<-sum(abs(sample-a))/length(sample)
  return(c(a,b))
}

