\name{MISE.sequence}
\Rdversion{1.1}
\alias{MISE.sequence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MISE.sequence(sample, S, min.order, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sample}{
%%     ~~Describe \code{sample} here~~
}
  \item{S}{
%%     ~~Describe \code{S} here~~
}
  \item{min.order}{
%%     ~~Describe \code{min.order} here~~
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
function(sample,S,min.order,...) {
  # returns all theta, and the MISEs within the horizon S
  # min.order depends on method of estimation and distribution; 
  #   it is determined in ddsmooth.test
  max.order<-max(unlist(S))
  st<-smooth.test(sample,rescale=T,order=max.order,B=NULL,output=F,...) 
  Sigma<-st$Sigma
  EVar<-st$EVar/length(sample)
  theta<-st$comp/sqrt(length(sample))
  theta2<-theta^2
  U<-matrix(st$comp,ncol=1)
  stats<-rep(NA,length(S))
  mises<-stats
  for(i in 1:length(S)) {
    ind<-S[[i]]-min.order+1 # indices corresponding to S
    nind<-!(1:(max.order-min.order+1))\%in\%ind # indices corresponding to \bar{S}
    temp<-try(t(U[ind])\%*\%solve(Sigma[ind,ind])\%*\%U[ind],silent=TRUE)
    if (!inherits(temp,"try-error")) { #det(Sigma[ind,ind]) \neq 0
	stats[i]<-temp
	mises[i]<-sum(EVar[ind])+sum(theta2[nind])-sum(EVar[nind])
    }
    else {
	stats[i]<-NA
	mises[i]<-NA # to ensure that this model is removed from the set of models so it is surely not selected 
    }
  }
  return(list(mises=mises,stats=stats,theta=theta))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
