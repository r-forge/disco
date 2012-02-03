diagnose.formula <-
function(obj,...) {
   m<-model.frame(obj,...)
   lv<-levels(m[,2])
   if(length(lv)!=2) stop("more than two group levels")
   x1<-m[m[,2]==lv[1],1]
   x2<-m[m[,2]==lv[2],1]
   RES<-wmw.diagnose.old(x1,x2)
   class(RES)<-"wmwdiagnose"
   RES
}

