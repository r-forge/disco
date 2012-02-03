\name{gamma.covarMLE}
\Rdversion{1.1}
\alias{gamma.covarMLE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gamma.covarMLE(a)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
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
function(a) { # shape a (independent of scale b)
	C<-matrix(0,ncol=3,nrow=3)
	C[1,1]<-1/2*(2*trigamma(a)*a^2-2*a+2*trigamma(a)*a-3)/(a+1)/(trigamma(a)*a-1)
	C[1,2]<-1/3*1/(a+1)/(trigamma(a)*a-1)*3^(1/2)/(a+2)^(1/2)
	C[2,1]<-C[1,2]
	C[1,3]<--1/2*1/(a+1)/(trigamma(a)*a-1)*3^(1/2)/(a+2)^(1/2)/(a+3)^(1/2)
	C[3,1]<-C[1,3]
	C[2,2]<-1/3*(9*trigamma(a)*a^2-9*a+6*trigamma(a)*a-8+3*a^3*trigamma(a)-3*a^2)/(a+1)/(a+2)/(trigamma(a)*a-1)
	C[2,3]<-1/((a+1)*(a+2)*(trigamma(a)*a-1)*(a+3)^(1/2))
	C[3,2]<-C[2,3]
	C[3,3]<-1/2*(2*a^4*trigamma(a)-2*a^3+12*a^3*trigamma(a)-12*a^2+22*trigamma(a)*a^2-22*a+12*trigamma(a)*a-15)/(a+3)/(a+2)/(a+1)/(trigamma(a)*a-1)
	return(C)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
