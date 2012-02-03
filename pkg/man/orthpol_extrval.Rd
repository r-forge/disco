\name{orthpol_extrval}
\Rdversion{1.1}
\alias{orthpol_extrval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
orthpol_extrval(k, a, b)
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
function(k,a,b) {
	#location-scale family!
	if (k==0) {
        h<-function(x){
          y<-1
          return(y)
        }
      }
	else if (k==1) {
        h<-function(x){
          z<-(x-a)/b
          y<-0.7796968012*z-0.4500532075
          return(y)
        }
      }
	else if (k==2) {
        h<-function(x){
          z<-(x-a)/b
          y<-.3451996482*(z-.5772156649)^2-.5045182395*z-.2766148303
          return(y)
        }
      }
	else if (k==3) {
        h<-function(x){
          z<-(x-a)/b
          y<-.1060499473*(z-.5772156649)^3-.4944037009*(z-.5772156649)^2-.2194200910*z+.6849580626
          return(y)
        }
      }
	else if (k==4) {
        h<-function(x){
          z<-(x-a)/b
          y<-.2493263979e-1*(z-.5772156649)^4-.2416834756*(z-.5772156649)^3+.2690771450*(z-.5772156649)^2+.7769092062*z-.6743236152
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
