\name{orthpol_gamma}
\Rdversion{1.1}
\alias{orthpol_gamma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
orthpol_gamma(k, a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
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
function(k,a,b) { # shape a and scale b
	if (k==0) {
        h<-function(x){
          y<-1
          return(y)
        }
      }
	else if (k==1) {
        h<-function(x){
          y<-(x-b*a)/b/a^(1/2)
          return(y)
        }
      }
	else if (k==2) {
        h<-function(x){
          y<-1/2*(x^2-2*x*b*a+b^2*a^2-2*b*x+b^2*a)*2^(1/2)/b^2/a^(1/2)/(a+1)^(1/2)
          return(y)
        }
      }
	else if (k==3) {
        h<-function(x){
          y<-1/6*(x^3-6*b*x^2-3*b*a*x^2+9*b^2*a*x+3*b^2*a^2*x+6*b^2*x-2*b^3*a-3*b^3*a^2-b^3*a^3)*6^(1/2)/(a+2)^(1/2)/(a+1)^(1/2)/a^(1/2)/b^3
          return(y)
        }
      }
	else if (k==4) {
        h<-function(x){
          y<-1/12*(x^4-12*b*x^3-4*b*a*x^3+6*b^2*a^2*x^2+36*b^2*x^2+30*b^2*a*x^2-4*b^3*a^3*x-24*b^3*x-44*b^3*a*x-24*b^3*a^2*x+6*b^4*a^3+11*b^4*a^2+6*b^4*a+b^4*a^4)*6^(1/2)/b^4/a^(1/2)/(a^3+6*a^2+11*a+6)^(1/2)
          return(y)
        }
      }
	return(h)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
