EDF.teststat <-
function(U,type,composite,distr) {
  # aux function for the EDF.test function
  n<-length(U)
  if(type=="AD") {
    ## CHECK!!!
    h <- (2 * seq(1:n) - 1) * (log(U) + log(1 - rev(U)))
    stat <- -n - mean(h)
    if(composite&distr=="norm") stat <- (1 + 0.75/n + 2.25/n^2) * stat
  }
  else if(type=="CvM") {
    stat<-sum((U-(2 * seq(1:n) - 1)/(2*n))^2)+1/(12*n)
    if(composite&distr=="norm") stat <- stat*(1+0.5/n)
    if(!composite&distr=="norm") stat<-(stat-0.4/n+0.6/n^2)*(1+1/n)
  }
  return(stat)
}

