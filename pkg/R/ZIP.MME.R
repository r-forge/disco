ZIP.MME <-
function(sample) {
  m<-mean(sample)
  s<-sum((sample-m)^2)/length(sample)
  pars<-c((s-m)/(s-m+m^2),(s-m+m^2)/m)
  # names(pars)<-c("p","lambda")
  pars<-pars[2:1]
  return(pars)
  # returns c(lambda,p)
}

