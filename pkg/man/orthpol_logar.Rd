\name{orthpol_logar}
\Rdversion{1.1}
\alias{orthpol_logar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
orthpol_logar(k, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
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
function(k,p) {
	if (k==0) {
        h<-function(x){
          y<-1
          return(y)
        }
      }
	else if (k==1) {
        h<-function(x){
          y<-(x*ln(1-p)*p-x*ln(1-p)-p)/ln(1-p)/(p-1)/(-(p+ln(1-p))*p/ln(1-p)^2/(p-1)^2)^(1/2)
          return(y)
        }
      }
	else if (k==2) {
        h<-function(x){
          y<-(-p^2+x^2*ln(1-p)-x*ln(1-p)+x*ln(1-p)*p^2+x*p^2-x*p+x^2*p^3+x^2*ln(1-p)*p^2-2*x^2*p^2-2*x^2*ln(1-p)*p+x^2*p)/(p-1)^2/(p+ln(1-p))/(-p^2/ln(1-p)*(2*p+p^2+2*ln(1-p))/(p-1)^4/(p+ln(1-p)))^(1/2)
          return(y)
        }
      }
	else if (k==3) {
        h<-function(x){
          y<-1/2*p^3/ln(1-p)^3*(p^5*x^2+7*p^4*x^2-6*x^2*ln(1-p)*p^2-6*x^2*ln(1-p)*p-4*x*p+6*ln(1-p)*p^3*x^2+6*p^3*x-2*ln(1-p)*x^3-2*x*p^2+4*x*ln(1-p)*p^3-4*x*ln(1-p)-4*p^3-3*x^3*p^3+5*x^3*p^2-2*x^3*p-x^3*p^4+p^5*x^3-6*ln(1-p)*x^3*p^2-11*x^2*p^3+6*ln(1-p)*x^3*p-3*x^2*p^2+6*x^2*p+2*ln(1-p)*x^3*p^3+6*x^2*ln(1-p))/(-p^9/ln(1-p)^7*(2*p+p^2+2*ln(1-p))*(6*p+2*p^3+3*p^2+6*ln(1-p))/(p-1)^18)^(1/2)/(p-1)^9
          return(y)
        }
      }
	else if (k==4) {
        h<-function(x){
          y<-1/12*(26*p^3*x^4-21*p^2*x^4+6*p*x^4-14*p^4*x^4+36*ln(1-p)*p^2*x^4-24*ln(1-p)*p*x^4+6*ln(1-p)*p^4*x^4-24*ln(1-p)*p^3*x^4-5*p^6*x^4+2*p^7*x^4+6*ln(1-p)*x^4+6*p^5*x^4+36*ln(1-p)*p^4*x^3+72*ln(1-p)*p*x^3-72*ln(1-p)*p^3*x^3-36*p*x^3+24*p^3*x^3+54*p^2*x^3-18*p^5*x^3-36*ln(1-p)*x^3-42*p^4*x^3+6*p^7*x^3+12*p^6*x^3+84*p^5*x^2+66*ln(1-p)*x^2-118*p^4*x^2-48*ln(1-p)*p*x^2+66*ln(1-p)*p^4*x^2+17*p^6*x^2-36*ln(1-p)*p^2*x^2-48*ln(1-p)*p^3*x^2+4*p^7*x^2-15*p^2*x^2+66*p*x^2-38*p^3*x^2+36*ln(1-p)*p^4*x-36*ln(1-p)*x-18*p^2*x+66*p^4*x-12*p^3*x-36*p*x-36*p^4)*2^(1/2)/(-p^4*(12*ln(1-p)+12*p+4*p^3+3*p^4+6*p^2)/(6*p+2*p^3+3*p^2+6*ln(1-p))/(p-1)^8/ln(1-p))^(1/2)/(p-1)^4/(6*p+2*p^3+3*p^2+6*ln(1-p))
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
