\name{betab.optfn}
\Rdversion{1.1}
\alias{betab.optfn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
betab.optfn(pars, x, ntrials, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pars}{
%%     ~~Describe \code{pars} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{ntrials}{
%%     ~~Describe \code{ntrials} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
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
function(pars,x,ntrials,n){
	a<-pars[1] #this is alpha
	b<-pars[2] #this is beta
	t1<-sum(digamma(x+a))-n*digamma(a+ntrials+b)-n*digamma(a)+n*digamma(a+b)
	t2<-sum(digamma(ntrials-x+b))-n*digamma(a+ntrials+b)-n*digamma(b)+n*digamma(a+b)
	return(c(t1,t2))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
