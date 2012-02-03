QQplot <-
function(x,y=NULL,distr=qnorm,pars=c(0,1),blom=0,PIT=F,data=NULL,name=NULL,groups=NULL,...) {
  # x: data vector or formula (for 2 and k-sample problems)
  # y: data vector (only when x is data vector and for the 2-sample problem)
  # distr: specifies the quantile function that has to be used; now only qnorm and qunif are defined 
  # pars: vector with parameters -- correspods to parameters used by q-function
  # blom: parameter used for the plotting positions (according to Blom)
  # if PIT=T then distr should be pDISTR which is user-defined and has "pars" as only parameter argument. (PIT=probability integral transformation)
  #    Expample
  #    > pEXP<-function(x,pars) pexp(x,rate=pars)
  #    > QQplot(PCB,distr=pEXP,pars=1/mean(PCB),blom=0.375,PIT=T)
  #
  # data: data frame (only when x is formula)
  # name: name to be used for graphs instead of x (for one sample plots)
  # groups: vector with 2 levels of factor for which a QQ plot must be plotted (only when x is formula)
  # ... : further arguments for the plot function and for QQplot.moresample
  onesample<-TRUE
  DNAME1<-name
  if(!is.null(y)) {
  	onesample<-FALSE
  	mf <- match.call(expand.dots = TRUE)
  	Xind<-match("xlab",names(mf))
  	Yind<-match("ylab",names(mf))
  	XNAME<-deparse(substitute(x))
  	YNAME<-deparse(substitute(y))
  	extras<-match.call(expand.dots=FALSE)$...
    cl<-call("QQplot.moresample",x=x,y=y)
    if(is.null(extras$xlab)) cl$xlab=ifelse(is.na(Xind),XNAME,NULL)
     if(is.null(extras$ylab)) cl$ylab=ifelse(is.na(Yind),YNAME,NULL)
    if (length(extras) > 0) {
        existing <- !is.na(match(names(extras), names(cl)))
        for (a in names(extras)[existing]) cl[[a]] <- extras[[a]]
        if (any(!existing)) {
            cl <- c(as.list(cl), extras[!existing])
            cl <- as.call(cl)
        }
    }
    eval(cl)
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
    
  	#m <- match.call(expand.dots = FALSE)
    #if (is.matrix(eval(m$data, parent.frame()))) 
    #    m$data <- as.data.frame(data)
    #m[[1L]] <- as.name("model.frame")
    #m$... <- NULL
    #mf <- eval(m, parent.frame())
    DNAME <- names(mf)[1]
    #names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
  	if(terms(x)[[3]]==1) {
  		        x.vector<-mf[[response]]
  		        DNAME1<-ifelse(is.null(DNAME1),names(mf)[1],DNAME1)
  	}
  	if(terms(x)[[3]]!=1) {
  		onesample<-FALSE
  		g <- factor(mf[[-response]])
  		resp<-mf[[response]]
  		if(!is.null(groups)) {
  			#if(length(groups)!=2) stop("when the groups argument is used, it should specify two groups")
  			gind<-g%in%groups
  			resp<-resp[gind]
  			g<-factor(g[gind])
  		}
        DATA <- split(resp, g)
        if(length(DATA)==2) {
        	extras<-match.call(expand.dots=FALSE)$...
           cl<-call("QQplot.moresample",x=DATA[[1]],y=DATA[[2]])
           if(is.null(extras$xlab)) {cl$xlab=paste(DNAME," (",levels(g)[1],")",sep="")}
             if(is.null(extras$ylab)) {cl$ylab=paste(DNAME," (",levels(g)[2],")",sep="")}
           if (length(extras) > 0) {
              existing <- !is.na(match(names(extras), names(cl)))
              for (a in names(extras)[existing]) cl[[a]] <- extras[[a]]
              if (any(!existing)) {
                 cl <- c(as.list(cl), extras[!existing])
                 cl <- as.call(cl)
              }
           }
             eval(cl)
        }
        if(length(DATA)>2) {
        	l<-length(DATA)
        	par(mfrow=c(l,l))
        	for(i in 1:l) {
        		for(j in 1:l) {
        			if(i!=j) {
        				QQplot.moresample(x=DATA[[i]],y=DATA[[j]],xlab=paste(DNAME," (",levels(g)[i],")",sep=""),ylab=paste(DNAME," (",levels(g)[j],")",sep=""))
        			}
        			else {
        				boxplot(DATA[[i]],xlab=paste(DNAME," (",levels(g)[i],")",sep=""))
        			}
        		}
        	}
        }

  	}
  }
  else {
    x.vector<-x
    DNAME1<-ifelse(is.null(name),deparse(substitute(x)),name)
  }
  if(onesample) {
  	d<-deparse(substitute(distr))
  	QQplot.onesample(x=x.vector,d=d,pars=pars,blom=blom,PIT=PIT,name=DNAME1,distr=distr,...)
  }
}

