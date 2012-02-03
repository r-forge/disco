PPplot.onesample <-
function(x,d,pars=NA,blom=0,name=NULL,...) {
  # x: data vector
  # d: specifief the quantile function that has to be used; now only qnorm and qunif are defined 
  # pars: vector with parameters -- correspods to parameters used by q-function
  # blom: parameter used for the plotting positions (according to Blom)
  # name: name to be used on graph instead of x
  dots<-as.list(substitute(list(...)))[-1]
  ndots<-names(dots)
  DNAME<-ifelse(is.null(name),deparse(substitute(x)),name)
  n<-length(x)
  x<-sort(x)
  p<-ppoints(n,a=blom)
  # parameter estimates should be given in pars
  #d<-deparse(substitute(distr))

  Fhat<-ecdf(x)  

  q<-switch(d,
            qnorm=Fhat(qnorm(p,mean=pars[1],sd=pars[2])),
            qunif=Fhat(qunif(p,min=pars[1],max=pars[2])),
		Fhat(distr(p,pars=pars)))
  i.main<-sum(ndots=="main")
  i.ylab<-sum(ndots=="ylab")
  i.xlab<-sum(ndots=="xlab")
  if(i.main==0) main<-paste("PP plot of",DNAME)
	else main<-dots$main
  if(i.xlab==0) xlab<-"p"
	else xlab<-dots$xlab
  if(i.ylab==0) ylab<-"empirical probability"
	else ylab<-dots$ylab
  plot(p,q,main=main,
       ylab=ylab,xlab=xlab)
  abline(c(0,1))
}

