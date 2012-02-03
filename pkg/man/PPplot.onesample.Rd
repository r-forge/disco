\name{PPplot.onesample}
\Rdversion{1.1}
\alias{PPplot.onesample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PPplot.onesample(x, d, pars = NA, blom = 0, name = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{pars}{
%%     ~~Describe \code{pars} here~~
}
  \item{blom}{
%%     ~~Describe \code{blom} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
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
function(x,d,pars=NA,blom=0,name=NULL,...) {
  # x: data vector
  # d: specifief the quantile function that has to be used; now only qnorm and qunif are defined 
  # pars: vector with parameters -- correspods to parameters used by q-function
  # blom: parameter used for the plotting positions (according to Blom)
  # name: name to be used on graph instead of x
  dots<-as.list(substitute(list(...)))[-1]
  ndots<-names(dots)
  DNAME<-ifelse(is.null(name),deparse(substitute(x)),name)
  n<-length(x)
  x<-sort(x)
  p<-ppoints(n,a=blom)
  # parameter estimates should be given in pars
  #d<-deparse(substitute(distr))

  Fhat<-ecdf(x)  

  q<-switch(d,
            qnorm=Fhat(qnorm(p,mean=pars[1],sd=pars[2])),
            qunif=Fhat(qunif(p,min=pars[1],max=pars[2])),
		Fhat(distr(p,pars=pars)))
  i.main<-sum(ndots=="main")
  i.ylab<-sum(ndots=="ylab")
  i.xlab<-sum(ndots=="xlab")
  if(i.main==0) main<-paste("PP plot of",DNAME)
	else main<-dots$main
  if(i.xlab==0) xlab<-"p"
	else xlab<-dots$xlab
  if(i.ylab==0) ylab<-"empirical probability"
	else ylab<-dots$ylab
  plot(p,q,main=main,
       ylab=ylab,xlab=xlab)
  abline(c(0,1))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
