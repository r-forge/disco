\name{EDF.teststat}
\Rdversion{1.1}
\alias{EDF.teststat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EDF.teststat(U, type, composite, distr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{U}{
%%     ~~Describe \code{U} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{composite}{
%%     ~~Describe \code{composite} here~~
}
  \item{distr}{
%%     ~~Describe \code{distr} here~~
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
function(U,type,composite,distr) {
  # aux function for the EDF.test function
  n<-length(U)
  if(type=="AD") {
    ## CHECK!!!
    h <- (2 * seq(1:n) - 1) * (log(U) + log(1 - rev(U)))
    stat <- -n - mean(h)
    if(composite&distr=="norm") stat <- (1 + 0.75/n + 2.25/n^2) * stat
  }
  else if(type=="CvM") {
    stat<-sum((U-(2 * seq(1:n) - 1)/(2*n))^2)+1/(12*n)
    if(composite&distr=="norm") stat <- stat*(1+0.5/n)
    if(!composite&distr=="norm") stat<-(stat-0.4/n+0.6/n^2)*(1+1/n)
  }
  return(stat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
