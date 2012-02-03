diagnose.numeric <-
function(x1,x2=NULL) {
   if(is.null(x2)) stop("second vector is missing")
   RES<-wmw.diagnose.old(x1,x2)
   class(RES)<-"wmwdiagnose"
   RES
}

