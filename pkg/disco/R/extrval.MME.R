extrval.MME <-
function(sample){
  b<-sqrt(6)*sqrt(sum((sample-mean(sample))^2)/length(sample))/pi
  a<-mean(sample)+digamma(1)*b
  return(c(a,b))
}

