orth.poly <-
function(degree,distr="unif",pars=c(0,1),sample=NA,method="NONE",f=NA,moments=NA,typedistr="cont",ntrials=NA){
  if (distr=="logis"||distr=="exp"||distr=="norm"||distr=="unif"||distr=="laplace"
	||distr=="extrval"||distr=="genpareto"||distr=="pois"||distr=="ZIP"||distr=="negb"
	||distr=="logar"||distr=="geom"||distr=="ZTP"||distr=="betab"||distr=="gamma") {
	if (method!="NONE"&&!is.numeric(sample)) stop("If nuisance parameter estimation has to be performed, 
									the given i.i.d. sample has to be a numeric vector.")
	if (method=="NONE"&&is.na(pars[1])) stop("The nuisance parameter(s) have to be given,
								 since no estimation method is chosen.")
	if (distr=="betab") {
		if (is.na(ntrials)) {stop("The number of trials has to be given through \"ntrials\".")}
		else {n<-ntrials}
  	}
  }
  else if (distr=="otherwise") {
	if (method != "NONE") stop("The orthonormal polynomials for a density given by \"f\" or \"moments\",   
					   can only be construced without nuisance parameter estimation.")
	if (is.na(moments[1])) {
		if (!is.function(f)) stop("The density is not (correctly) specified.") 
					#i.e. not through distr (1st choice), nor moments (2nd choice), nor f (last choice)
		if (!(typedistr=="cont"||typedistr=="disc")) stop("The type of the density \"f\" is not correctly specified.")
	}
	else { #in this case "moments" are used (and not "f")
		if (!is.numeric(moments)||(length(moments)!=2*degree)) stop("The density is not (correctly) specified: 
												bad input for argument \"moments\".") 
	}
  }
  else {
	stop("The density is not correctly specified through \"distr\".")
  }
  if ( ((trunc(degree)-degree)!=0) || (degree<0) ) stop("The degree of an orthonormal polynomial has to be a natural number.")
  if (!(method=="MLE"||method=="MME"||method=="NONE")) stop("The nuisance parameter estimation method is not 1 of 3 given possibilities.")
  if (method=="MLE") {
	pars<-switch(distr,
  	    norm=norm.MLE(sample),
	    exp=exp.MLE(sample),
	    logar=logar.MLE(sample),
	    pois=pois.MLE(sample),
	    geom=geom.MLE(sample),
	    ZTP=ZTP.MLE(sample),
   	    logis=logis.MLE(sample),
    	    negb=negb.MLE(sample),
     	    laplace=laplace.MLE(sample),
	    ZIP=ZIP.MLE(sample),
  	    extrval=extrval.MLE(sample),
	    genpareto=genpareto.MLE(sample),
	    betab=betab.MLE(sample,ntrials=n),
	    gamma=gamma.MLE(sample),
	    unif=unif.MLE(sample)) # the given pars are only used when method=="NONE"
  }
  else if (method=="MME") {
	pars<-switch(distr,
  	    norm=norm.MLE(sample),
	    exp=exp.MLE(sample),
	    logar=logar.MLE(sample),
	    pois=pois.MLE(sample),
	    geom=geom.MLE(sample),
	    ZTP=ZTP.MLE(sample),
   	    logis=logis.MME(sample),
    	    negb=negb.MME(sample),
     	    laplace=laplace.MME(sample),
	    ZIP=ZIP.MME(sample),
  	    extrval=extrval.MME(sample),
	    genpareto=genpareto.MME(sample),
	    betab=betab.MME(sample,ntrials=n),
	    gamma=gamma.MME(sample),
	    unif=unif.MME(sample)) # the given pars are only used when method=="NONE"
  }
  if (distr=="unif"){
	MIN<-pars[1]  
	MAX<-pars[2]
	if (!is.numeric(MIN) || !is.numeric(MAX) || is.na(MIN) || is.na(MAX)) stop("\"min\" and \"max\" are not correctly specified or estimated.")
	if (!(MIN<MAX)) stop("\"min\" has to be smaller than \"max\".")
	f<-function(x){
	  y<-dunif(x,min=MIN,max=MAX)
	  return(y)
	}
  }
  else if (distr=="norm"){
	MEAN<-pars[1]  
	SD<-pars[2]
	if (!is.numeric(MEAN) || !is.numeric(SD) || is.na(MEAN) || is.na(SD)) stop("\"mean\" and \"sd\" are not correctly specified or estimated.")
	if (!(SD>0)) stop("\"sd\" has to be strictly positive.") 
	f<-function(x){
	  y<-dnorm(x,mean=0,sd=1) # MEAN and SD are loc scale (used later in standard orthonormal polynomial)
	  return(y)
	}
  }
  else if (distr=="exp"){
	LAMBDA<-pars  
	if (!is.numeric(LAMBDA) || is.na(LAMBDA)) stop("\"rate\" is not correctly specified or estimated.")
	if (!(LAMBDA>0)) stop("\"rate\" has to be strictly positive.")
	f<-function(x){
	  y<-dexp(x,rate=1) # 1/LAMBDA is scale
	  return(y)
	}
  }
  else if (distr=="logis"){
	MU<-pars[1]  
	SIGMA<-pars[2]
	if (!is.numeric(MU) || !is.numeric(SIGMA) || is.na(MU) || is.na(SIGMA)) stop("\"location\" and \"scale\" are not correctly specified or estimated.")
	if (!(SIGMA>0)) stop("\"scale\" has to be strictly positive.")
	f<-function(x){
	  y<-dlogis(x,location=0,scale=1) # MU and SIGMA are loc scale
	  return(y)
	}
  }
  else if (distr=="ZIP"){ 
      lambda<-pars[1]  
	p<-pars[2]
	if (!is.numeric(lambda) || !is.numeric(p) || is.na(lambda) || is.na(p)) stop("\"lambda\" and \"phi\" are not correctly specified or estimated.")
	if (!(lambda>0)) stop("\"lambda\" has to be strictly positive.")
	if (!(p>=0) || !(p<1)) stop("\"phi\" must be positive and strictly smaller than 1.")
	f<-function(x){
	  y<-dpois(x,lambda)*(1-p)
	  y[x==0]<-p+(1-p)*exp(-lambda)
	  return(y)
	}
  }
  else if (distr=="logar"){
	Beta<-pars
	if (!is.numeric(Beta) || is.na(Beta)) stop("\"prob\" is not correctly specified or estimated.") 
	if (!(Beta>0) || !(Beta<1)) stop("\"prob\" must be strictly between 0 and 1.")
	f<-function(x){
	  y<--Beta^x/(x*log(1-Beta))
	  y[x==0]<-0
	  return(y)
	}
  }
  else if (distr=="pois"){
	lambda<-pars
	if (!is.numeric(lambda) || is.na(lambda)) stop("\"lambda\" is not correctly specified or estimated.") 
	if (!(lambda>=0)) stop("\"lambda\" has to be positive.")
	f<-function(x){
	  y<-dpois(x,lambda=lambda)
	  return(y)
	}
  }
  else if (distr=="negb"){ 
      r<-pars[1]  
	p<-pars[2]
	if (!is.numeric(r) || !is.numeric(p) || is.na(r) || is.na(p)) stop("\"size\" and \"prob\" are not correctly specified or estimated.")
	if (!(r>0)) stop("\"size\" has to be strictly positive.")
	if (!(p>0) || !(p<1)) stop("\"prob\" must be strictly between 0 and 1.")
	f<-function(x){
	  y<-dnbinom(x,size=r,prob=p)
	  return(y)
	}
  }
  else if (distr=="laplace"){
	a<-pars[1]
	b<-pars[2]
	if (!is.numeric(a) || !is.numeric(b) || is.na(a) || is.na(b)) stop("\"location\" and \"scale\" are not correctly specified or estimated.")
	if (!(b>0)) stop("\"scale\" has to be strictly positive.")
	f<-function(x){
	  y<-exp(-abs(x))/(2) # a and b are loc scale
	  return(y)
	}
  }
  else if (distr=="extrval"){
	a<-pars[1]
	b<-pars[2]
	if (!is.numeric(a) || !is.numeric(b) || is.na(a) || is.na(b)) stop("\"location\" and \"scale\" are not correctly specified or estimated.")
	if (!(b>0)) stop("\"scale\" has to be strictly positive.")
	f<-function(x){
	  y<-exp(-x-exp(-x)) # a and b are loc scale
	  return(y)
	}
  }
  else if (distr=="geom"){
	p<-pars
	if (!is.numeric(p) || is.na(p)) stop("\"prob\" is not correctly specified or estimated.")
	if (!(p>0) || !(p<=1)) stop("\"prob\" must be strictly positive and smaller than or equal to 1.")
	f<-function(x){
	  y<-dgeom(x,prob=p) 
	  return(y)
	}
  }
  else if (distr=="ZTP"){
	lambda<-pars
	if (!is.numeric(lambda) || is.na(lambda)) stop("\"lambda\" is not correctly specified or estimated.")
	if (!(lambda>=0)) stop("\"lambda\" has to be positive.")
	f<-function(x){
	  y<-dZTP(x,lambda=lambda)
	  return(y)
	}
  }
  else if (distr=="genpareto"){
	s<-pars[1]
	k<-pars[2]
	if (!is.numeric(s) || !is.numeric(k) || is.na(s) || is.na(k)) stop("\"scale\" and \"shape\" are not correctly specified or estimated.")
	if (!(s>0)) stop("\"scale\" has to be strictly positive.")
      if (!(k>-1/(2*degree))) stop("The orthonormal polynomials cannot be constructed. 
					Reason: The estimated shape parameter is too small, 
					so the estimated moments are not finite.")
	f<-function(x){
	  y<-dgenpareto(x,s=1,k=k) # s is scale 
	  return(y)
	}
  }
  else if (distr=="betab"){
	alpha<-pars[1]
	beta<-pars[2]
	if (!is.numeric(alpha) || !is.numeric(beta) || is.na(alpha) || is.na(beta)) stop("\"shape1\" and \"shape2\" are not correctly specified or estimated.")
	if (!(alpha>0)) stop("\"shape1\" has to be strictly positive.")
	if (!(beta>0)) stop("\"shape2\" has to be strictly positive.")
	f<-function(x){
	  y<-rep(0,length(x))
	  y[(x>=0)&(x<=n)]<-choose(n,x[(x>=0)&(x<=n)])*beta(alpha+x[(x>=0)&(x<=n)],beta+n-x[(x>=0)&(x<=n)])/beta(alpha,beta)
	  return(y)
	}
  }
  else if (distr=="gamma"){
	a<-pars[1] # shape
	b<-pars[2] # scale
	if (!is.numeric(a) || !is.numeric(b) || is.na(a) || is.na(b)) stop("\"shape\" and \"scale\" are not correctly specified or estimated.")
	if (!(a>0)) stop("\"shape\" has to be strictly positive.")
	if (!(b>0)) stop("\"scale\" has to be strictly positive.")
	f<-function(x){
	  y<-dgamma(x, shape=a, scale=1) # b is scale
	  return(y)
	}
  }
  if (distr=="norm"&&degree<11) {  
	h<-orthpol_norm(degree,mu=MEAN,sigma=SD)
  }
  else if (distr=="exp"&&degree<11) {  
	h<-orthpol_exp(degree,sigma=1/LAMBDA)
  }
  else if (distr=="pois") {
	h<-orthpol_pois(degree,lambda=lambda)  
  }
  else if (distr=="unif") {
	h<-orthpol_unif(degree,a=MIN,b=MAX)  
  }
  else if (distr=="logis"&&degree<5) {  
	h<-orthpol_logis(degree,mu=MU,sigma=SIGMA)
  }
  else if (distr=="ZIP"&&degree<5) {  
	h<-orthpol_ZIP(degree,p=p,lambda=lambda)
  }
  else if (distr=="laplace"&&degree<5) {  
	h<-orthpol_laplace(degree,alpha=a,beta=b)
  }
  else if (distr=="logar"&&degree<5) {  
	h<-orthpol_logar(degree,p=Beta)
  }
  else if (distr=="negb"&&degree<5) {  
	h<-orthpol_negb(degree,p=p,r=r)
  }
  else if (distr=="extrval"&&degree<5) {  
	h<-orthpol_extrval(degree,a=a,b=b)
  }
  else if (distr=="geom"&&degree<5) {  
	h<-orthpol_geom(degree,p=p)
  }
  else if (distr=="ZTP"&&degree<5) {  
	h<-orthpol_ZTP(degree,lambda=lambda)
  }
  else if (distr=="genpareto"&&degree<5) {  
	h<-orthpol_genpareto(degree,s=s,k=k)
  }
  else if (distr=="betab"&&degree<5) {  
	h<-orthpol_betab(degree,ntrials=n,a=alpha,b=beta)
  }
  else if (distr=="gamma"&&degree<5) {  
	h<-orthpol_gamma(degree,a=a,b=b)
  }
  else if (distr=="logis"||distr=="exp"||distr=="norm"||distr=="unif"||distr=="laplace"||distr=="extrval"
	||distr=="genpareto"||distr=="gamma"||(distr=="otherwise"&&is.na(moments[1])&&typedistr=="cont")){
      h0<-make.poly(f=f,k=degree,moments=NA)[[degree+1]]
	h<-switch(distr,
		norm=function(x){h0((x-MEAN)/SD)},
		exp=function(x){h0(x*LAMBDA)},
		logis=function(x){h0((x-MU)/SIGMA)},
		laplace=function(x){h0((x-a)/b)},
		extrval=function(x){h0((x-a)/b)},
		genpareto=function(x){h0(x/s)},
		gamma=function(x){h0(x/b)},
		h0) # corresponds with unif or continuous f: no transformation used
  }
  else if (distr=="pois"||distr=="ZIP"||distr=="negb"||distr=="logar"||distr=="geom"||distr=="ZTP"
	||distr=="betab"||(distr=="otherwise"&&is.na(moments[1])&&typedistr=="disc")){
      h<-make.poly.disc(f=f,k=degree,moments=NA)[[degree+1]]
  }
  else { # in this case the moments are used: distr="otherwise" and !is.na(moments) 
	h<-make.poly(f=f,k=degree,moments=moments)[[degree+1]]
  }
  return(h)
}

