\name{EDF.test.1sample}
\Rdversion{1.1}
\alias{EDF.test.1sample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EDF.test.1sample(x, distr = "norm", type = "AD", pars = NA, B = 100, name = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{distr}{
%%     ~~Describe \code{distr} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{pars}{
%%     ~~Describe \code{pars} here~~
}
  \item{B}{
%%     ~~Describe \code{B} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
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
function(x,distr="norm",type="AD",pars=NA,B=100,name=NULL) {
  METHOD<-"One-sample "
  nametest<-switch(type,
                AD="Anderson-Darling",
                CvM="Cramer-von Mises")
  if(is.null(nametest)) stop("The type of test should be any of AD or CvM")
  METHOD<-paste(METHOD,nametest,"Test for the")
  namedistr<-switch(distr,
                norm="normal",
                exp="exponential",
                unif="uniform")
  if(is.null(namedistr)) stop("The distribution is not correctly specified")
  METHOD<-paste(METHOD,namedistr,"distribution")
  if(is.na(B)) METHOD<-paste(METHOD," (asymptotic approximation)")
  if(!is.na(B)) METHOD<-paste(METHOD," (based on",B,"bootstrap runs)")
  n<-length(x)
  x<-sort(x)
  composite<-F
  if(is.na(pars[1])) {
    ## composite null hypothesis --> parameter estimation
    composite<-T
    if(distr=="norm") {
      pars<-c(mean(x),sd(x))
    }
    else if(distr=="exp") {
      pars<-1/mean(x)
    }
  }
  if(distr=="norm") {
    U<-pnorm(x,mean=pars[1],sd=pars[2])
    rdistr<-function(n) rnorm(n,mean=pars[1],sd=pars[2])
  }
  else if(distr=="exp") {
    U<-pexp(x,rate=pars)
    rdistr<-function(n) rexp(n,rate=pars)
  }
  else if(distr=="unif") {
    U<-punif(x,min=pars[1],max=pars[2])
    rdistr<-function(n) runif(n,min=pars[1],max=pars[2])
  }

  t.obs<-EDF.teststat(U,type=type,composite=composite,distr=distr)

  if(is.na(B)) {
    ## asymptotic approximation to the null distribution
    p.value<-NA
    if(type=="AD" & !composite) {
      p.value<-0.25
      if(t.obs>1.248) p.value<-0.25
      if(t.obs>1.610) p.value<-0.15
      if(t.obs>1.933) p.value<-0.1
      if(t.obs>2.492) p.value<-0.05
      if(t.obs>3.070) p.value<-0.025
      if(t.obs>3.875) p.value<-0.01
      if(t.obs>4.5) p.value<-0.005
      if(t.obs>6) p.value<-0.001
      if(t.obs<=1.248) warning("The p-value is only a lower bound.")
         else warning("The p-value is only an upper bound.",call.=F)
    }
    if(type=="AD" & composite & distr=="norm") {
      if(t.obs<0.2) p.value<-1-exp(-13.436+101.14*t.obs-223.73*t.obs^2)
      if(t.obs>=0.2 & t.obs<0.34) p.value<-1-exp(-8.318+42.796*t.obs-59.938*t.obs^2)
      if(t.obs>=0.34 & t.obs<0.6) p.value<-exp(0.9177-4.279*t.obs-1.38*t.obs^2)
      if(t.obs>=0.6) p.value<-exp(1.2937-5.709*t.obs+0.0186*t.obs^2)
    }
    if(type=="CvM" & composite & distr=="norm") {
      if(t.obs<0.0275) p.value<-1-exp(-13.953+775.5*t.obs-12542.61*t.obs^2)
      if(t.obs>=0.0275 & t.obs<0.051) p.value<-1-exp(-5.903+179.546*t.obs-1515.29*t.obs^2)
      if(t.obs>=0.051 & t.obs<0.092) p.value<-exp(0.886-31.62*t.obs+10.897*t.obs^2)
      if(t.obs>=0.092) p.value<-exp(1.111-34.242*t.obs+12.832*t.obs^2)
    }
    if(type=="CvM" & !composite) {
      p.value<-0.25
      if(t.obs>0.209) p.value<-0.25
      if(t.obs>0.284) p.value<-0.15
      if(t.obs>0.347) p.value<-0.1
      if(t.obs>0.461) p.value<-0.05
      if(t.obs>0.581) p.value<-0.025
      if(t.obs>0.743) p.value<-0.01
      if(t.obs>0.869) p.value<-0.005
      if(t.obs>1.167) p.value<-0.001
      if(t.obs<=0.209) warning("The p-value is only a lower bound.")
         else warning("The p-value is only an upper bound.",call.=F)
    }
    if(is.na(p.value)) warning(paste("No approximate p-value available for the",nametest,"test for the",namedistr,"distribution"))
  }
  else {
    ## bootstrap null distribution
    d<-rep(NA,B)
    for(i in 1:B) {
      x.star<-rdistr(n)
      if(composite) d[i]<-EDF.test(x.star,type=type,distr=distr,pars=NA,B=NA)$statistic  
        else d[i]<-EDF.test(x.star,type=type,distr=distr,pars=pars,B=NA)$statistic  
    }
    p.value<-length(d[d>=t.obs])/B
  }
 
#  if(is.na(B)) {
#  	PARAMETER<-Inf 
#  	names(PARAMETER)<-"asymptotic approximation"
#  }
#  if(!is.na(B)) {
#    PARAMETER<-B
#    names(PARAMETER)<-"number of bootstrap runs"
#  }
  
  names(t.obs)<-"T"

  RVAL<-list(statistic=c(t.obs), p.value=p.value,
        method=METHOD,
        data.name=name)
  class(RVAL)<-"htest"
  return(RVAL)  
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
