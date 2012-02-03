\name{gajek}
\Rdversion{1.1}
\alias{gajek}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gajek(f, g, lower = 0, upper = 1, N = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
%%     ~~Describe \code{f} here~~
}
  \item{g}{
%%     ~~Describe \code{g} here~~
}
  \item{lower}{
%%     ~~Describe \code{lower} here~~
}
  \item{upper}{
%%     ~~Describe \code{upper} here~~
}
  \item{N}{
%%     ~~Describe \code{N} here~~
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
function(f,g,lower=0,upper=1,N=100) {
 sx<-seq(lower,upper,1/N)
 f.sx<-f(sx)
 eps.s<-seq(0,max(f.sx),1/N)
 integrals<-c()
 for(eps in eps.s) {
   fc<-function(x) {
     d<-f(x)-eps*g(x)
     d[d<0]<-0
     return(d)  
   }
   i<-integrate(fc,lower=lower,upper=upper)$value
   integrals<-c(integrals,i)
 }
 errors<-abs(integrals-1)
 error<-min(errors)

 int<-integrals[errors==error]
 epsilon<-eps.s[errors==error]
 fc<-function(x) {
     d<-f(x)-epsilon*g(x)
     d[d<0]<-0
     return(d)  
 }
 r<-list(error=error,integral=int,epsilon=epsilon,fc=fc)
 return(r)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
