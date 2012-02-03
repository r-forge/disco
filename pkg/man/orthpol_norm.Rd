\name{orthpol_norm}
\Rdversion{1.1}
\alias{orthpol_norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
orthpol_norm(k, mu, sigma)
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
          y<-z
          return(y)
        }
      }
	else if (k==2) {
        h<-function(x){
          z<-(x-mu)/sigma
          y<-1/2*2^(1/2)*z^2-1/2*2^(1/2)
          return(y)
        }
      }
	else if (k==3) {
        h<-function(x){
          z<-(x-mu)/sigma
          y<-1/6*3^(1/2)*2^(1/2)*z^3-1/2*3^(1/2)*2^(1/2)*z
          return(y)
        }
      }
	else if (k==4) {
        h<-function(x){
          z<-(x-mu)/sigma
          y<-1/12*6^(1/2)*z^4-1/2*6^(1/2)*z^2+1/4*6^(1/2)
          return(y)
        }
      }
	else if (k==5) {
        h<-function(x){
          z<-(x-mu)/sigma
          y<-1/60*15^(1/2)*2^(1/2)*z^5-1/6*15^(1/2)*2^(1/2)*z^3+1/4*15^(1/2)*2^(1/2)*z
          return(y)
        }
      }
	else if (k==6) {
        h<-function(x){
          z<-(x-mu)/sigma
          y<-1/60*5^(1/2)*z^6-1/4*5^(1/2)*z^4+3/4*5^(1/2)*z^2-1/4*5^(1/2)
          return(y)
        }
      }
	else if (k==7) {
        h<-function(x){
          z<-(x-mu)/sigma
          y<-1/840*70^(1/2)*2^(1/2)*z^7-1/40*70^(1/2)*2^(1/2)*z^5+1/8*70^(1/2)*2^(1/2)*z^3-1/8*70^(1/2)*2^(1/2)*z
          return(y)
        }
      }
	else if (k==8) {
        h<-function(x){
          z<-(x-mu)/sigma
          y<-1/1680*70^(1/2)*z^8-1/60*70^(1/2)*z^6+1/8*70^(1/2)*z^4-1/4*70^(1/2)*z^2+1/16*70^(1/2)
          return(y)
        }
      }
	else if (k==9) {
        h<-function(x){
          z<-(x-mu)/sigma
          y<-1/5040*35^(1/2)*2^(1/2)*z^9-1/140*35^(1/2)*2^(1/2)*z^7+3/40*35^(1/2)*2^(1/2)*z^5-1/4*35^(1/2)*2^(1/2)*z^3+3/16*35^(1/2)*2^(1/2)*z
          return(y)
        }
      }
	else if (k==10) {
        h<-function(x){
          z<-(x-mu)/sigma
          y<-1/5040*7^(1/2)*z^10-1/112*7^(1/2)*z^8+1/8*7^(1/2)*z^6-5/8*7^(1/2)*z^4+15/16*7^(1/2)*z^2-3/16*7^(1/2)
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
