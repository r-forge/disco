ksboot.test <-
function(x,distr="pnorm",B=100) {
  # Kolmogorov-Smirnov test based on the bootstrap null distribution
  # x: data vector
  # distr: distribution to test for (normal, exponential, lognormal)
  # B: number of bootstrap runs
  
  if((is.null(B))|(is.na(B))) {
  	stop("B should be a positive number (number of bootstrap runs)")
  }
  if(B<100) {
  	warning("B should be at least 100 to obtain reliable p-values\n")
  }
  
  DNAME<-deparse(substitute(x))
  n<-length(x)
  METHOD<-"Bootstrap One-sample Kolomogorov Smirnov Test for the"
  METHOD<-paste(METHOD,switch(distr,
                pnorm="normal",
                pexp="exponential",
                plnorm="log normal",
                ppois="Poisson"),"distribution")
  

  if(distr=="pnorm") {
  	kt<-function(x,pars) suppressWarnings(ks.test(x,y="pnorm",mean=pars[1],sd=pars[2])$statistic)
  	simdistr<-function(n,pars) rnorm(n,mean=pars[1],sd=pars[2])
  	pars.est<-function(x) c(mean(x),sd(x))
  	pars.obs<-pars.est(x)
  }
  if(distr=="pexp") {
  	kt<-function(x,pars) suppressWarnings(ks.test(x,y="pexp",rate=pars)$statistic)
  	simdistr<-function(n,pars) rexp(n,rate=pars)
  	pars.est<-function(x) mean(x)
  	pars.obs<-pars.est(x)
  }
  if(distr=="plnorm") {
  	kt<-function(x,pars) suppressWarnings(ks.test(x,y="plnorm",mean=pars[1],sd=pars[2])$statistic)
  	simdistr<-function(n,pars) rlnorm(n,meanlog=pars[1],sdlog=pars[2])
  	pars.est<-function(x) c(mean(log(x)),sd(log(x)))
  	pars.obs<-pars.est(x)
  }
  if(distr=="ppois") {
  	kt<-function(x,pars) suppressWarnings(ks.test(x,y="ppois",lambda=pars)$statistic)
  	simdistr<-function(n,pars) rpois(n,lambda=pars)
  	pars.est<-function(x) mean(x)
  	pars.obs<-pars.est(x)
  }

  t.obs<-kt(x,pars.obs)
  d<-rep(NA,B)
  for(i in 1:B) {
    x.star<-simdistr(n,pars.obs)
    d[i]<-kt(x.star,pars=pars.est(x.star))  
  }
  p.value<-length(d[d>=t.obs])/B

  PARAMETER<-B
  names(PARAMETER)<-"number of bootstrap runs"

  RVAL<-list(statistic=c(t.obs), p.value=p.value,
        method=METHOD,
        data.name=DNAME,parameter=PARAMETER)
  class(RVAL)<-"htest"
  return(RVAL)  
}

