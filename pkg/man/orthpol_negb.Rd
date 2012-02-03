\name{orthpol_negb}
\Rdversion{1.1}
\alias{orthpol_negb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
orthpol_negb(k, p, r)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
}
  \item{r}{
%%     ~~Describe \code{r} here~~
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
function(k,p,r) {
	if (k==0) {
        h<-function(x){
          y<-1
          return(y)
        }
      }
	else if (k==1) {
        h<-function(x){
          y<-(p*x-r+p*r)/p/(-(-1+p)*r/p^2)^(1/2)
          return(y)
        }
      }
	else if (k==2) {
        h<-function(x){
          y<-1/2*(2*p^2*r*x+r^2*p^2+p^2*x+p^2*x^2+p^2*r-2*p*x-2*p*r*x-2*p*r-2*r^2*p+r+r^2)/p^2*2^(1/2)/((-1+p)^2*(1+r)*r/p^4)^(1/2)
          return(y)
        }
      }
	else if (k==3) {
        h<-function(x){
          y<--1/6*6^(1/2)/(-r^5*(-1+p)^9*(1+r)^3*(2+r)/p^18)^(1/2)*r^2*(2*p^6*x+30*p^4*x-24*p^5*r^3-40*p^3*r-100*p^3*r^2+2*r+24*p^2*x+30*p^4*r+r^4-6*r^4*p+15*r^4*p^2-20*r^4*p^3+15*r^4*p^4+4*r^3-80*r^3*p^3+5*r^2+66*p^2*r*x-15*p*r*x-116*p^3*r*x+2*r*p^6+5*r^2*p^6+60*r^3*p^2-24*r^3*p+75*r^2*p^4+60*r^3*p^4+30*p^2*r+75*r^2*p^2-30*r^2*p-12*p*r+9*p^2*r*x^2-33*p^3*r*x^2-12*p*r^2*x+57*p^2*r^2*x-108*p^3*r^2*x+4*r^3*p^6+r^4*p^6-p^3*x^3+3*p^4*r*x^3-12*p^3*r^2*x^2+6*p^2*x^2-21*p^3*x^2-38*p^3*x+18*p^4*r^2*x^2+15*p^2*r^3*x-30*p^3*r^3*x+30*p^4*r^3*x+3*p^2*r^2*x^2-3*p*r^3*x+45*p^4*r*x^2-12*p^5*r-30*p^5*r^2-6*p^5*r^4-3*p^5*x^3+3*p^4*x^3-27*p^5*x^2*r+102*p^4*r^2*x-48*p^5*x*r^2-3*p^5*r*x^3+p^6*x^3-12*p^5*r^2*x^2-15*p^5*r^3*x+6*p^6*x^2*r+9*p^6*x*r^2+p^6*r*x^3+3*p^6*r^2*x^2+3*p^6*r^3*x-r*p^3*x^3+102*p^4*r*x-45*p^5*x*r-15*p^5*x^2+27*p^4*x^2+3*p^6*x^2-12*p^5*x-6*p*x+8*p^6*x*r)/p^9
          return(y)
        }
      }
	else if (k==4) {
        h<-function(x){
          y<-1/12/p^4*(p^4*x^4+6*p^4*x^3-12*p^3*x^3-4*r*p^3*x^3+4*p^4*r*x^3+30*p^2*r*x^2+6*p^2*r^2*x^2-48*p^3*r*x^2+11*p^4*x^2-36*p^3*x^2+18*p^4*r*x^2-12*p^3*r^2*x^2+36*p^2*x^2+6*p^4*r^2*x^2-4*p*r^3*x-24*p^3*x+12*p^2*r^3*x+6*p^4*x+18*p^4*r^2*x+102*p^2*r*x-44*p*r*x-80*p^3*r*x-24*p*x+4*p^4*r^3*x-24*p*r^2*x+66*p^2*r^2*x-12*p^3*r^3*x+36*p^2*x+22*p^4*r*x-60*p^3*r^2*x+r^4*p^4-4*r^4*p^3-24*r^3*p+6*r^3+6*r^3*p^4+36*p^2*r+66*r^2*p^2-44*r^2*p-24*p*r-24*r^3*p^3+11*r^2-24*p^3*r-44*p^3*r^2+6*r+36*r^3*p^2+6*p^4*r+11*r^2*p^4+r^4-4*r^4*p+6*r^4*p^2)*6^(1/2)/((-1+p)^4*r*(r^3+11*r+6*r^2+6)/p^8)^(1/2)
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
