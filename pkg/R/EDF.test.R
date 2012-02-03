EDF.test <-
function(x,y=NULL,distr="norm",type="AD",pars=NA,B=100,groups=NULL,data=NULL,warn=T) {
  # x: data vector (one sample test) or formula
  # y: data vector (only for two-sample test when no formula is provided)
  # distr: specifies the distribution tested for (norm, exp, unif)
  # type: type of EDF test: AD or CvM
  # pars: if pars=NA then simple null hypothesis, otherwise composite
  # B: number of bootstrap runs; if NULL, then asymptotic approximation
  # groups: vector with the levels of the factor in the formula that have to be compared in 2-sample EDF test
  # data: name of data set (only if formula is provided)
  # warn: logical indicating whether warnings should be printen (warn=F is used in repeated calls to the function for bootstrapping)
  
  MC<-match.call()
  
  if(!is.null(B)) {
  	if(is.na(B)) {
  		B<-NULL # to make the function agree with the book
  	}
  }
  
  onesample<-TRUE
  DNAME1<-NULL
  if(!is.null(y)) {
  	# only two-sample has nonempty y
  	onesample<-FALSE
  	mf <- match.call(expand.dots = TRUE)
  	Xind<-match("xlab",names(mf))
  	Yind<-match("ylab",names(mf))
  	XNAME<-deparse(substitute(x))
  	YNAME<-deparse(substitute(y))
    cl<-call("EDF.test.2sample",x=x,y=y,B=B,type=type,xname=XNAME,yname=YNAME)
  }
  if(class(x)=="formula") {
  	mf <- match.call(expand.dots = FALSE)
  	names(mf)[names(mf)=="x"]<-"formula"
    m <- match(c("formula", "data", "subset", "weights", "na.action", 
        "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    DNAME <- names(mf)[1]
    DNAME.factor<-names(mf)[2]
    response <- attr(attr(mf, "terms"), "response")
  	if(terms(x)[[3]]==1) {
  				# one-sample
  		        x.vector<-mf[[response]]
  		        DNAME1<-ifelse(is.null(DNAME1),names(mf)[1],DNAME1)
  	}
  	if(terms(x)[[3]]!=1) {
  		# k-sample
  		onesample<-FALSE
  		g <- factor(mf[[-response]])
  		resp<-mf[[response]]
  		if(!is.null(groups)) {
  			if(length(groups)!=2) stop("when the groups argument is used, it should specify two groups")
  			gind<-g%in%groups
  			resp<-resp[gind]
  			g<-factor(g[gind])
  		}
        DATA <- split(resp, g)
        if(length(DATA)==2) {
        	xname<-paste(DNAME," (",DNAME.factor,"=",levels(g)[1],")",sep="")
        	yname<-paste(DNAME," (",DNAME.factor,"=",levels(g)[2],")",sep="")
        	cl<-call("EDF.test.2sample",x=DATA[[1]],y=DATA[[2]],B=B,type=type,xname=xname,yname=yname)
            #eval(cl)
        }
        if(length(DATA)>2) {
  			cl<-call("EDF.test.ksample",data=DATA,B=B,type=type,formula=x,dname=DNAME)
  			#eval(cl)
  		}
  	}
  }
  if(class(x)!="formula") {
    x.vector<-x
    DNAME1<-ifelse(is.null(DNAME1),deparse(substitute(x)),DNAME1)
  }
  if(onesample) {
  	cl<-call("EDF.test.1sample",x=x.vector,B=B,type=type,distr=distr,name=DNAME1,pars=pars,warn=warn)
  	#eval(cl)
  }
  eval(cl)
}

