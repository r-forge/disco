\name{orthpol_geom}
\Rdversion{1.1}
\alias{orthpol_geom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
orthpol_geom(k, p)
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
          y<-(x*p-1+p)/(-(-1+p))^(1/2)
          return(y)
        }
      }
	else if (k==2) {
        h<-function(x){
          y<-1/2*(3*p^2*x+p^2*x^2+2*p^2-4*p*x-4*p+2)/((-1+p)^2)^(1/2)
          return(y)
        }
      }
	else if (k==3) {
        h<-function(x){
          y<--1/6/(-(-1+p)^9)^(1/2)*(6-36*p+90*p^2-120*p^3+90*p^4+9*x^2*p^2-36*p^5+6*p^6-3*p^5*x^3+45*p^4*x^2-60*p^5*x+11*p^6*x+6*p^6*x^2+132*p^4*x-27*p^5*x^2+p^6*x^3-33*x^2*p^3+81*x*p^2-146*x*p^3-x^3*p^3+3*x^3*p^4-18*x*p)
          return(y)
        }
      }
	else if (k==4) {
        h<-function(x){
          y<-1/24*(p^4*x^4+10*p^4*x^3-16*p^3*x^3-96*p^3*x^2+35*p^4*x^2+72*p^2*x^2+216*p^2*x+50*p^4*x-176*p^3*x-96*p*x+24*p^4+144*p^2-96*p^3-96*p+24)/((-1+p)^4)^(1/2)
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
