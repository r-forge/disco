\name{EDF.test}
\Rdversion{1.1}
\alias{EDF.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Empirical Distribution Function
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function includes many of the traditional EDF (Empirical Distribution Function) tests for the one-sample and the K-sample problems (e.g. Anderson-Darling and Cramer-von Mises). 
}
\usage{
EDF.test(x, y = NULL, distr = "norm", type = "AD", pars = NA, B = 100, groups = NULL, data = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
When \code{x} is a numeric vector and \code{y} is NULL, then a one-sample test will be performed. When \code{x} and \code{y} are numeric vectors, a two-sample test will be performed. When \code{y} is NULL, and \code{x} is a formula, the test for the specified model will be performed. See Details for further information. 
}
  \item{y}{
%%     ~~Describe \code{y} here~~
  When \code{y} is not NULL (default), it should be a numeric vector and a two-sample EDF test will be performed. See also \code{x}. 
}
  \item{distr}{
%%     ~~Describe \code{distr} here~~
The name (character string) of the distribution to be tested for (only for the one-sample test). It must be one of 'norm' (normal), 'exp' (exponential) or 'unif' (uniform).
}
  \item{type}{
%%     ~~Describe \code{type} here~~
A character string that specifies the EDF test. It should be one of 'AD' (Anderson-Darling) or 'CvM' (Cramer-von Mises). 
}
  \item{pars}{
%%     ~~Describe \code{pars} here~~
The parameter values of \code{distr} when a simple null hypothesis is to be tested.
}
  \item{B}{
%%     ~~Describe \code{B} here~~
The number of bootstrap runs for the p-value calculation. When B=NULL (default) p-values are calculated using the asymptotic approximation. 
}
  \item{groups}{
%%     ~~Describe \code{groups} here~~
This is only used if \code{x} is a formula that involves a factor with more then two levels and when only two   levels have to be selected for performing a two-sample EDF test. \code{group} is a numeric vector that specifies the groups (levels of the factor in the formula) that have to be compared.  
}
  \item{data}{
%%     ~~Describe \code{data} here~~
When \code{x} is a formula, \code{data} should be the name of the data set. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
All the EDF tests included in this function are described in D'Agostino and Stephens (1986). 
}
\value{
The result of the \code{EDF.test} function is an object of the \code{htest} class, containing the following components:
\item{statistic}{the value of the  EDF test statistic}
\item{p.value}{the p-value for the test}
\item{method}{a character string indicating what type of EDF test was performed}
\item{data.name}{a character string giving the name(s) of the data}

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
D'Agostino, R. and Stephens, M. (1986). Goodness-of-Fit Techniques. Marcel-Dekker, New-York, USA. 
}
\author{
%%  ~~who you are~~
Olivier Thas
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
function(x,y=NULL,distr="norm",type="AD",pars=NA,B=100,groups=NULL,data=NULL) {
  # x: data vector (one sample test) of formula
  # y: data vector (only for two-sample test when no formula is provided)
  # distr: specifies the distribution tested for (norm, exp, unif)
  # type: type of EDF test: AD or CvM
  # pars: if pars=NA then simple null hypothesis, otherwise composite
  # B: number of bootstrap runs; if NA, then asymptotic approximation
  # groups: vector with the levels of the factor in the formula that have to be compared in 2-sample EDF test
  # data: name of data set (only if formula is provided)
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
  			gind<-g\%in\%groups
  			resp<-resp[gind]
  			g<-factor(g[gind])
  		}
        DATA <- split(resp, g)
        if(length(DATA)==2) {
        	xname<-paste(DNAME," (",levels(g)[1],")",sep="")
        	yname<-paste(DNAME," (",levels(g)[2],")",sep="")
        	cl<-call("EDF.test.2sample",x=DATA[[1]],y=DATA[[2]],B=B,type=type,xname=xname,yname=yname)
            #eval(cl)
        }
        if(length(DATA)>2) {
  			cl<-call("EDF.test.ksample",data=DATA,B=B,type=type,formula=x,dname=DNAME)
  			#eval(cl)
  		}
  }
  }
  else {
    x.vector<-x
    DNAME1<-ifelse(is.null(DNAME1),deparse(substitute(x)),DNAME1)
  }
  if(onesample) {
  	cl<-call("EDF.test.1sample",x=x.vector,B=B,type=type,distr=distr,name=DNAME1)
  	#eval(cl)
  }
  eval(cl)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
\keyword{nonparametric}% __ONLY ONE__ keyword per line
