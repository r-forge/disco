diagnose.cdk <-
function(x,...) {
  if(x$K!=2) {
  	stop("The diagnostics are only implemented for the 2-sample smooth test")
  }
  else {
  	RES<-x$diagnose$order2
  }
  class(RES)<-"wmwdiagnose"
  RES
}

