negb.MLE <-
function(sample){
  r<-negb.MLEr(sample)
  p<-r/(r+mean(sample))
  return(c(r,p))
}

