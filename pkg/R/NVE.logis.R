NVE.logis <-
function(sample,order,parest=NA){
  #choose order between 3 and 8#
 if (order<9 && order>2) {
  n<-length(sample)
  if (is.na(parest[1])) {
  tmp<-logis.MME(sample)
  beta<-tmp[1]
  sigma<-tmp[2]
  }
  else {
  beta<-parest[1]
  sigma<-parest[2]
  }
  z<-(sample-beta)/sigma
  var_est<-var.logis2(order,z,sigma)
 }
 else {var_est<-NA}
 return(var_est)
}

