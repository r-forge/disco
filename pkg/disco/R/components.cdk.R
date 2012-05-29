components.cdk <-
function(x,order=NULL,contrast=c("sum",NULL),squared=T,...) {
	if(is.null(order)) order<-x$order
	m<-NULL
    if(x$K>2) {
      m<-x$coefficients
      n.sample<-nrow(m)
      if(contrast[1]=="control") {
        ctr<-diag(x$K)
        ctr[,as.numeric(contrast[2])]<--1
        ctr[as.numeric(contrast[2]),]<-0
        m<-ctr%*%m
      }
    }
    RES<-list(x=x,order=order,contrast=contrast,m=m,squared=squared)
    class(RES)<-"componentscdk"
    return(RES) 
}

