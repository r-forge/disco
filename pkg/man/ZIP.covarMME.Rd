\name{ZIP.covarMME}
\Rdversion{1.1}
\alias{ZIP.covarMME}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ZIP.covarMME(lambda, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lambda}{
%%     ~~Describe \code{lambda} here~~
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
function(lambda,p) { # rate lambda and prob p
	C<-matrix(0,ncol=2,nrow=2)
	C[1,1]<-(lambda^3*p+5*lambda^2*p+6*lambda*p+6+2*lambda)/(lambda^3*p+3*lambda^2*p+6*lambda*p+6)
	C[1,2]<--2*lambda^(3/2)*(lambda^2*p+2*lambda*p+2)*(lambda*p+1)/((lambda^2*p+2*lambda*p+2)*(lambda^3*p+3*lambda^2*p+6*lambda*p+6))^(1/2)*3^(1/2)/((lambda^4*p+24*lambda*p+4*lambda^3*p+12*lambda^2*p+24)*(lambda^3*p+3*lambda^2*p+6*lambda*p+6))^(1/2)
	C[2,1]<-C[1,2]
	C[2,2]<-(lambda^7*p^2+7*lambda^6*p^2+36*lambda^5*p^2+12*lambda^4*p+96*lambda^4*p^2+144*lambda^3*p^2+144*lambda^2*p^2+288*lambda*p+72*lambda^3*p+144*lambda^2*p+144+12*lambda^2)/(lambda^4*p+24*lambda*p+4*lambda^3*p+12*lambda^2*p+24)/(lambda^3*p+3*lambda^2*p+6*lambda*p+6)
	return(C)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
