negb.MME <-
function(sample){
  m<-mean(sample)
  s<-sum((sample-m)^2)/length(sample)
  p<-m/s
  r<-m*p/(1-p)
  return(c(r,p))
}

