#'Construct a PP plot
#'
#'This function constructs a PP plot for both either one sample or more samples, as explained in the book Comparing distributions.
#'@param x data vector or formula (for 2 and k-sample problems)
#'@param y data vector (only when x is data vector and for the 2-sample problem)
#'@param distr specifies the quantile function that has to be used; now only qnorm and qunif are defined
#'@param pars vector with parameters -- correspods to parameters used by q-function
#'@param blom parameter used for the plotting positions (according to Blom)
#'@param data: data frame (only when x is formula)
#'@param name: name to be used for graphs instead of x (for one sample plots)
#'@param groups: vector with 2 levels of factor for which a PP plot must be plotted (only when x is formula)
#'@param ... : further arguments for the plot function and for \code{\link{PPplot.moresample}}
#'@return This function doesn't return a useful value, it just plots the PPplot
#'@author Olivier Thas
#'@aliases PPplot PP-plot
#'@note More information is given in section 3.2 and 8.1 of the book 'Comparing Distributions' by Olivier Thas.
#'@references Thas Olivier(2009), Comparing Distributions. Springer ( \url{http://www.springer.com/statistics/book/978-0-387-92709-1})

PPplot <-
function(x,y=NULL,distr=qnorm,pars=c(0,1),blom=0,data=NULL,name=NULL,groups=NULL,...) {

  onesample<-TRUE
  DNAME1<-name
  distr <- match.fun(distr)                      # JM: extend functionality. Make distr general

  if(!is.null(y)) {
  	onesample<-FALSE
  	mf <- match.call(expand.dots = TRUE)
  	Xind<-match("xlab",names(mf))
  	Yind<-match("ylab",names(mf))
  	XNAME<-deparse(substitute(x))
  	YNAME<-deparse(substitute(y))
  	extras<-match.call(expand.dots=FALSE)$...
    cl<-call("PPplot.moresample",x=x,y=y)
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
    
    DNAME <- names(mf)[1]
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
           cl<-call("PPplot.moresample",x=DATA[[1]],y=DATA[[2]])
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
        				PPplot.moresample(x=DATA[[i]],y=DATA[[j]],xlab=paste(DNAME," (",levels(g)[i],")",sep=""),ylab=paste(DNAME," (",levels(g)[j],")",sep=""))
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
  	PPplot.onesample(x=x.vector,d=d,pars=pars,blom=blom,name=DNAME1,distr=distr,...)
  }
}

