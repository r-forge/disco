\name{orthpol_laplace}
\Rdversion{1.1}
\alias{orthpol_laplace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
orthpol_laplace(k, alpha, beta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{beta}{
%%     ~~Describe \code{beta} here~~
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
function(k,alpha,beta) {
	#location-scale family!
	if (k==0) {
        h<-function(x){
          y<-1
          return(y)
        }
      }
	else if (k==1) {
        h<-function(x){
          z<-(x-alpha)/beta
          y<-z/sqrt(2)
          return(y)
        }
      }
	else if (k==2) {
        h<-function(x){
          z<-(x-alpha)/beta
          y<-(z^2-2)/(2*sqrt(5))
          return(y)
        }
      }
	else if (k==3) {
        h<-function(x){
          z<-(x-alpha)/beta
          y<-(z^3-12*z)/(12*sqrt(3))
          return(y)
        }
      }
	else if (k==4) {
        h<-function(x){
          z<-(x-alpha)/beta
          y<-1/3576*sqrt(745)*(z^4-168/5*z^2+216/5)
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
