QQplot.onesample <-
function(x,d,pars=c(0,1),blom=0,PIT=F,name=NULL,distr=NULL,...) {
  # x: data vector
  # distr: specifies the quantile function that has to be used; now only qnorm and qunif are defined 
  # pars: vector with parameters -- correspods to parameters used by q-function
  # blom: parameter used for the plotting positions (according to Blom)
  # if PIT=T then distr should be pDISTR which is user-defined and has "pars" as only parameter argument. (PIT=probability integral transformation)
  #    Expample
  #    > pEXP<-function(x,pars) pexp(x,rate=pars)
  #    > QQplot(PCB,distr=pEXP,pars=1/mean(PCB),blom=0.375,PIT=T)
  #
  # name: name to be used on graph instead of x
  dots<-as.list(substitute(list(...)))[-1]
  ndots<-names(dots)
  DNAME<-ifelse(is.null(name),deparse(substitute(x)),name)
  n<-length(x)
  x<-sort(x)
  p<-ppoints(n,a=blom)
  # parameter estimates should be given in pars
  #d<-deparse(substitute(distr))
  
  if(PIT) {
    q<-switch(d,
            qnorm=pnorm(x,mean=pars[1],sd=pars[2]),
            qunif=punif(x,min=pars[1],max=pars[2]),
		distr(x,pars=pars))
  } 
  else {
    q<-switch(d,
            qnorm=qnorm(p,mean=pars[1],sd=pars[2]),
            qunif=qunif(p,min=pars[1],max=pars[2]),
		distr(p,pars=pars))
  }

  i.main<-sum(ndots=="main")
  i.ylab<-sum(ndots=="ylab")
  i.xlab<-sum(ndots=="xlab")
  if(PIT) {
    if(i.main==0) main<-paste("QQ plot of",DNAME,"after PIT")
	else main<-dots$main
    if(i.xlab==0) xlab<-"probability"
	else xlab<-dots$xlab
    if(i.ylab==0) ylab<-paste("uniform order statistic of",DNAME)
	else ylab<-dots$ylab
    plot(p,q,main=main,
       ylab=ylab,
       xlab=xlab)
  }
  else {
    if(i.main==0) main<-paste("QQ plot of",DNAME)
	else main<-dots$main
    if(i.xlab==0) xlab<-"expected quantile"
	else xlab<-dots$xlab
    if(i.ylab==0) ylab<-DNAME
	else ylab<-dots$ylab
    plot(q,x,main=main,
       ylab=ylab,xlab=xlab)
  }
  abline(c(0,1))
}

