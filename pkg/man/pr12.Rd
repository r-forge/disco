\name{pr12}
\Rdversion{1.1}
\alias{pr12}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pr12(x, y = NULL, groups = NULL, data = NULL, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{groups}{
%%     ~~Describe \code{groups} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
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
function(x,y=NULL,groups=NULL,data=NULL,alpha=0.05) {
  # x: data vector (one sample test) of formula
  # y: data vector (only for two-sample case when no formula is provided)
  # groups: vector with the levels of the factor in the formula that have to be compared 
  # data: name of data set (only if formula is provided)
  # alpha: 1-alpha is the confidence level of the CI

  DNAME1<-NULL
  RES<-NULL
  if(!is.null(y)) {
  	# only two-sample has nonempty y
  	onesample<-FALSE
  	mf <- match.call(expand.dots = TRUE)
  	Xind<-match("xlab",names(mf))
  	Yind<-match("ylab",names(mf))
  	XNAME<-deparse(substitute(x))
  	YNAME<-deparse(substitute(y))
    RES<-pr12.2sample(x=x,y=y,xname=XNAME,yname=YNAME,alpha=alpha)
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
  				stop("At least two samples are required.")
  	}
  	if(terms(x)[[3]]!=1) {
  		# k-samples
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
        	XNAME<-paste(DNAME," (",levels(g)[1],")",sep="")
        	YNAME<-paste(DNAME," (",levels(g)[2],")",sep="")
        	RES<-pr12.2sample(x=DATA[[1]],y=DATA[[2]],xname=XNAME,yname=YNAME,alpha=alpha)
        }
        if(length(DATA)>2) {
        	l<-length(DATA)
        	PR<-matrix(ncol=l,nrow=l)
        	gnames<-levels(g)
        	rownames(PR)<-gnames
        	colnames(PR)<-gnames
        	XNAME<-gnames
        	YNAME<-gnames
        	PR.CI<-PR
        	for(i in 1:(l-1)) {
        		for(j in (i+1):l) {
        			if(i!=j) {
        				tmp<-pr12.2sample(x=DATA[[i]],y=DATA[[j]],xname=gnames[i],yname=gnames[j],alpha=alpha)
			        	PR[i,j]<-tmp$estimate
			        	PR[j,i]<-PR[i,j]
			        	PR.CI[j,i]<-tmp$lower.bound
			        	PR.CI[i,j]<-tmp$upper.bound
        			}
        		}
        	}
        }
	}
	}
	if(!is.null(RES)) {
		PR<-RES$estimate
		PR.CI<-c(RES$lower.bound,RES$upper.bound)	
	}
	METHOD<-"Estimation of Pr(X1<=X2), and the Halperin-Mee confidence interval"
	RVAL<-list(method=METHOD,estimate=PR,ci.bounds=PR.CI,alpha=alpha,xname=XNAME,yname=YNAME)
	class(RVAL)<-"pr12"
    return(RVAL)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
