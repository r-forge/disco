\name{PPplot.moresample}
\Rdversion{1.1}
\alias{PPplot.moresample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PPplot.moresample(x, y, m = max(length(x), length(y)), ref = T, add = F, xlab = NULL, ylab = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{m}{
%%     ~~Describe \code{m} here~~
}
  \item{ref}{
%%     ~~Describe \code{ref} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
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
function(x,y,m=max(length(x),length(y)),ref=T,add=F,xlab=NULL,ylab=NULL,...) {
  # x is reference distribution
  F1<-ecdf(x)
  F2<-ecdf(y)
  p1<-c()
  p2<-c()
  for(i in 1:m) {
    F1.inv<-inv.cdf(F1,(i-0.5)/m,x.range=sort(x))
    p1<-c(p1,(i-0.5)/m)
    p2<-c(p2,F2(F1.inv))
  }
  if(add) {
    points(p1,p2,...)
  } 
  else {
    plot(p1,p2,xlab=xlab,ylab=ylab,ylim=c(0,1),xlim=c(0,1),...)
  }
  if(ref) abline(c(0,1))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
