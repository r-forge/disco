\name{PPplot}
\Rdversion{1.1}
\alias{PPplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PPplot(x, y = NULL, distr = qnorm, pars = c(0, 1), blom = 0, data = NULL, name = NULL, groups = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{distr}{
%%     ~~Describe \code{distr} here~~
}
  \item{pars}{
%%     ~~Describe \code{pars} here~~
}
  \item{blom}{
%%     ~~Describe \code{blom} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{groups}{
%%     ~~Describe \code{groups} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x,y=NULL,distr=qnorm,pars=c(0,1),blom=0,data=NULL,name=NULL,groups=NULL,...) {
  # x: data vector or formula (for 2 and k-sample problems)
  # y: data vector (only when x is data vector and for the 2-sample problem)
  # distr: specifies the quantile function that has to be used; now only qnorm and qunif are defined 
  # pars: vector with parameters -- correspods to parameters used by q-function
  # blom: parameter used for the plotting positions (according to Blom)
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
  			if(length(groups)!=2) stop("when the groups argument is used, it should specify two groups")
  			gind<-g\%in\%groups
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
