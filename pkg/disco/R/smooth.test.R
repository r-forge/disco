smooth.test <-
function(x,y=NULL,data=NULL,order,distr="unif",method="NONE",pars=c(0,1),B=NULL,rescale=F,f=NA,moments=NA,typedistr="cont",chol=FALSE,ntrials=NA,basis="Lg",var="jn",st=F,horizon="order",criterion="AIC",graph=F,plot.range=NULL,...) {
	if(!is.null(criterion)) {
		if(criterion=="NULL") {
			criterion<-NULL
		}
	}
	onesample<-TRUE
	g<-NA
	resp<-NA
	MC<-match.call(expand.dots = FALSE)
	if(!is.null(y)) {
		onesample<-FALSE
		# two-sample --> cdk
		DNAME1<-deparse(substitute(x))
#  		assign(DNAME1,x)
#  		NAME2<-deparse(substitute(y))
#  		assign(DNAME2,y)
		DNAME<-paste(DNAME1," (sample 1) and ",DNAME2,sep="",collapse="")
  		xall<-c(x,y)
  		g<-c(rep(1,length(x)),rep(2,length(y)))
		#RES<-cd(x1=x,x2=y,order=order,basis=basis,var=var,st=st,horizon=horizon,criterion=criterion,graph=graph)
		RES<-cdk(x=xall,g=g,order=order,basis=basis,var=var,st=st,criterion=criterion,horizon=horizon,B=B)
	}
	if(class(x)=="formula") {
		mf <- MC
    	m <- match(c("x", "data", "subset", "weights", "na.action", 
        	"offset"), names(mf), 0L) # I changed formula with x
    	mf <- mf[c(1L, m)]
    	names(mf)[2]<-"formula"
    	mf$drop.unused.levels <- TRUE
    	mf[[1L]] <- as.name("model.frame")
    	mf <- eval(mf, parent.frame())
        DNAME <- names(mf)[1] # name of response variable
    	response <- attr(attr(mf, "terms"), "response")
  		if(terms(x)[[3]]==1) {
  			# one-sample --> smooth.test
  		   	x.vector<-mf[[response]]
  		   	assign(DNAME,x.vector)
  		}
  		if(terms(x)[[3]]!=1) {
  			# 2- or k-sample --> cd or cdk
  			onesample<-FALSE
  			g <-(mf[[-response]])
  			resp<-mf[[response]]
  			assign(DNAME,resp)
  			RES<-cdk(x=eval(parse(text=DNAME)),g=g,order=order,basis=basis,var=var,st=st,criterion=criterion,horizon=horizon,B=B)
  		}
  	}
  	else {
  		DNAME<-deparse(substitute(x))
  		assign(DNAME,x)
  	}
  	if(onesample) {
  		if(is.null(criterion)) {
  			tmp<-cd1(sample=eval(parse(text=DNAME)),order=order,distr=distr,method=method,pars=pars,B=B,rescale=rescale,f=f,moments=moments,typedistr=typedistr,chol=chol,output=F,ntrials=ntrials)
  			RES<-cd1.2.cdk(tmp)
  		}
  		if(!is.null(criterion)) {
  			RES<-ddcd1(sample=eval(parse(text=DNAME)),max.order=order,horizon=horizon,criterion=criterion,distr=distr,method=method,pars=pars,B=B,f=f,typedistr=typedistr,ntrials,graph=graph,plot.range=plot.range,x.label=DNAME,...) 
  		}
  	}
    #return(list(RES=RES,g=g,resp=resp)) # debug mode
    RES$call<-MC
    RES$DNAME<-DNAME
    return(RES)
}

