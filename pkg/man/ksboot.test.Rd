\name{ksboot.test}
\Rdversion{1.1}
\alias{ksboot.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ksboot.test(x, distr = "pnorm", B = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{distr}{
%%     ~~Describe \code{distr} here~~
}
  \item{B}{
%%     ~~Describe \code{B} here~~
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
function(x,distr="pnorm",B=100) {
  # Kolmogorov-Smirnov test based on the bootstrap null distribution
  # x: data vector
  # distr: distribution to test for (normal, exponential, lognormal)
  # B: number of bootstrap runs
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
