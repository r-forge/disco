logis.MME <-
function(sample){
  MU<-mean(sample)
  SIGMA<-sqrt(3)*sqrt(sum((sample-MU)^2)/length(sample))/pi
  return(c(MU,SIGMA))
}

