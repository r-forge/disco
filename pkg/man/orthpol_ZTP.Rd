\name{orthpol_ZTP}
\Rdversion{1.1}
\alias{orthpol_ZTP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
orthpol_ZTP(k, lambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{lambda}{
%%     ~~Describe \code{lambda} here~~
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
function(k,lambda) {
	if (k==0) {
        h<-function(x){
          y<-1
          return(y)
        }
      }
	else if (k==1) {
        h<-function(x){
          y<--(-x*exp(lambda)+x+lambda*exp(lambda))/(exp(lambda)-1)/(-lambda*exp(lambda)*(-exp(lambda)+1+lambda)/(exp(lambda)-1)^2)^(1/2)
          return(y)
        }
      }
	else if (k==2) {
        h<-function(x){
          y<--(lambda^2*exp(lambda)-2*x*lambda*exp(lambda)-x*exp(lambda)+x^2*exp(lambda)+x-x^2-x^2*lambda+lambda^2*x+3*lambda*x)/((-2*exp(lambda)+lambda^2+2+2*lambda)*lambda^2*exp(lambda)/(-exp(lambda)+1+lambda)/(exp(lambda)-1))^(1/2)/(-exp(lambda)+1+lambda)
          return(y)
        }
      }
	else if (k==3) {
        h<-function(x){
          y<--1/2*(-4*x*exp(lambda)+6*x^2*exp(lambda)+2*lambda^3*exp(lambda)-6*x*lambda*exp(lambda)-6*x*lambda^2*exp(lambda)-2*x^3*exp(lambda)+6*x^2*lambda*exp(lambda)+10*lambda*x+6*lambda^3*x-9*lambda^2*x^2-2*lambda^3*x^2+4*x+lambda^2*x^3-12*x^2*lambda-6*x^2+lambda^4*x+2*lambda*x^3+14*lambda^2*x+2*x^3)*exp(2*lambda)*lambda^3*2^(1/2)/(exp(lambda)-1)^3/(lambda^9*exp(lambda)^5*(-2*exp(lambda)+lambda^2+2+2*lambda)*(-6*exp(lambda)+6+3*lambda^2+6*lambda+lambda^3)/(exp(lambda)-1)^7)^(1/2)
          return(y)
        }
      }
	else if (k==4) {
        h<-function(x){
          y<-1/6*(lambda^3*x^4+6*lambda*x^4+3*lambda^2*x^4+6*x^4-6*exp(lambda)*x^4-36*x^3+24*lambda*exp(lambda)*x^3-3*lambda^4*x^3-42*lambda^2*x^3-60*lambda*x^3-18*lambda^3*x^3+36*exp(lambda)*x^3+3*lambda^5*x^2+24*lambda^4*x^2-36*lambda^2*exp(lambda)*x^2+66*x^2-72*exp(lambda)*lambda*x^2+141*lambda^2*x^2+83*lambda^3*x^2+138*lambda*x^2-66*exp(lambda)*x^2-90*lambda^3*x+36*exp(lambda)*lambda^2*x-9*lambda^5*x+24*lambda^3*exp(lambda)*x-lambda^6*x-84*lambda*x-36*x-39*lambda^4*x+36*exp(lambda)*x-102*lambda^2*x+48*exp(lambda)*lambda*x-6*lambda^4*exp(lambda))*6^(1/2)/(-6*exp(lambda)+6+lambda^3+6*lambda+3*lambda^2)/((-24*exp(lambda)+24+12*lambda^2+24*lambda+4*lambda^3+lambda^4)*lambda^4*exp(lambda)/(exp(lambda)-1)/(-6*exp(lambda)+6+lambda^3+6*lambda+3*lambda^2))^(1/2)
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
