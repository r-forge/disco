laplace.MME <-
function(sample){
  a<-mean(sample)
  b<-sqrt(sum((sample-a)^2)/(2*length(sample)))
  return(c(a,b))
}

