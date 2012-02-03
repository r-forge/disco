NVE.laplace <-
function(sample,order,parest=NA){
  #choose order between 3 and 8#
 if (order<9 && order>2) {
  n<-length(sample)
  if (is.na(parest[1])) {
  tmp<-laplace.MME(sample)
  a<-tmp[1]
  b<-tmp[2]
  }
  else {
  a<-parest[1]
  b<-parest[2]
  }
  z<-(sample-a)/b
  var_est<-var.laplace2(order,z,b)
 }
 else {var_est<-NA}
 return(var_est)
}

