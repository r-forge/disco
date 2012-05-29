NVE.unif <-
function(sample,order,parest=NA){
  #choose order between 3 and 8#
 if (order<9 && order>2) {
  n<-length(sample)
  if (is.na(parest[1])) {
  tmp<-unif.MME(sample)
  a<-tmp[1]
  b<-tmp[2]
  }
  else {
  a<-parest[1]
  b<-parest[2]
  }
  sigma<-b-a
  z<-(sample-a)/sigma # also location-scale family but scale sigma depends on location a!!
  var_est<-var.unif2(order,z,sigma)
 }
 else {var_est<-NA}
 return(var_est)
}

