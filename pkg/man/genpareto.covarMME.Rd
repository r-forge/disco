\name{genpareto.covarMME}
\Rdversion{1.1}
\alias{genpareto.covarMME}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
genpareto.covarMME(k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{
%%     ~~Describe \code{k} here~~
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
function(k) { # shape k (independent of scale s)
	C<-matrix(0,ncol=2,nrow=2)
	C[1,1]<-(1440*k^6+2208*k^5+1474*k^4+513*k^3+109*k^2+15*k+1)/(1+3*k)^3/(1+2*k)^3
	C[1,2]<-24*(50*k^3+55*k^2+24*k+3)*k^3/(1/(1+6*k))^(1/2)/(1+3*k)^3/(1+2*k)^3/(1/(1+8*k))^(1/2)/(1+4*k)
	C[2,1]<-C[1,2]
	C[2,2]<-(241920*k^9+357984*k^8+251280*k^7+105632*k^6+34632*k^5+9966*k^4+2205*k^3+321*k^2+27*k+1)/(1+2*k)^3/(1+3*k)^3/(1+4*k)^3
	return(C)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
