\name{orthpol_logis}
\Rdversion{1.1}
\alias{orthpol_logis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
orthpol_logis(k, mu, sigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{mu}{
%%     ~~Describe \code{mu} here~~
}
  \item{sigma}{
%%     ~~Describe \code{sigma} here~~
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
function(k,mu,sigma) {
	#location-scale family!
	if (k==0) {
        h<-function(x){
          y<-1
          return(y)
        }
      }
	else if (k==1) {
        h<-function(x){
          z<-(x-mu)/sigma
          y<-sqrt(3)/pi*z
          return(y)
        }
      }
	else if (k==2) {
        h<-function(x){
          z<-(x-mu)/sigma
          y<-3/sqrt(3.2)/pi^2*(z^2-pi^2/3)
          return(y)
        }
      }
	else if (k==3) {
        h<-function(x){
          z<-(x-mu)/sigma
          y<-5*sqrt(7)/12/pi^3*(z^3-4.2*pi^2/3*z)
          return(y)
        }
      }
	else if (k==4) {
        h<-function(x){
          z<-(x-mu)/sigma
          y<-35/64/pi^4*(z^4-26/7*pi^2*z^2+27/35*pi^4)
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
