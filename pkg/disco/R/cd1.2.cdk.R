cd1.2.cdk <-
function(obj) {
#	S<-list()
	S<-obj
	S$statistic<-obj$allstatistics[length(obj$allstatistics)]
	#S$comp<-obj$statistics[-length(obj$statistics)]
	#S$p.values<-obj$p.value
	S$par.est<-obj$par.est
	S$names.par<-names(obj$par.est)
	S$EVar<-obj$EVar
	S$Sigma<-obj$Sigma
	S$K<-1
#	S$order<-obj$order
#	S$order.select<-obj$order.select
#	S$distr<-obj$distr

	class(S)<-"cdk"
	return(S)
}

