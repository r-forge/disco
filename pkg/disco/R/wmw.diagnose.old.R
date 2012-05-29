wmw.diagnose.old <-
function(x1,x2) {
  # function to diagnose some of the assumptions underlying the WMW test
  n1<-length(x1)
  n2<-length(x2)
  cnt1<-0
  cnt2<-0
  for(i in 1:10000) {
    if(max(sample(x1,2))<=sample(x2,1)) cnt1<-cnt1+1 
    if(max(sample(x2,2))<=sample(x1,1)) cnt2<-cnt2+1 
  }
  p112<-cnt1/10000
  p221<-cnt2/10000
  vmw<-var.mw(x1,x2)$var

  tmp<-list(p112=p112,p221=p221,var=vmw,n1=n1,n2=n2)
  class(tmp)<-"wmwdiagnose"

  tmp
 }

