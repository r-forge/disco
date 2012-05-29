geom.MLE <-
function(sample){
  p<-1/(1+mean(sample))
  return(p)
}

