\name{ZIP.covarMLE}
\Rdversion{1.1}
\alias{ZIP.covarMLE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ZIP.covarMLE(lambda, p)
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
	C<-matrix(c((-lambda^3*p-2*lambda^2*p-lambda^2+2*lambda*p*exp(lambda)-2*lambda*p-2*lambda+2*exp(lambda)-2)/(lambda^2*p+2*lambda*p+2)/(exp(lambda)-1-lambda), (lambda*p^2*exp(lambda)-lambda*p^2+lambda*p-p+p*exp(lambda)+1)*lambda^(5/2)*2^(1/2)/(lambda^2*p+2*lambda*p+2)/(lambda*p+1)^(1/2)/(exp(lambda)-1-lambda)/(lambda^3*p+3*lambda^2*p+6*lambda*p+6)^(1/2), -(lambda*p^2*exp(lambda)-lambda*p^2+lambda*p-p+p*exp(lambda)+1)*6^(1/2)*lambda^3/(lambda^3*p+3*lambda^2*p+6*lambda*p+6)^(1/2)/(lambda^4*p+24*lambda*p+4*lambda^3*p+12*lambda^2*p+24)^(1/2)/(exp(lambda)-1-lambda)/(lambda*p+1)^(1/2)/(lambda^2*p+2*lambda*p+2)^(1/2),
			(lambda*p^2*exp(lambda)-lambda*p^2+lambda*p-p+p*exp(lambda)+1)*lambda^(5/2)*2^(1/2)/(lambda^2*p+2*lambda*p+2)/(lambda*p+1)^(1/2)/(exp(lambda)-1-lambda)/(lambda^3*p+3*lambda^2*p+6*lambda*p+6)^(1/2), (12*lambda^3*p^2*exp(lambda)+12*p^2*exp(lambda)*lambda^2+lambda^5*p^2*exp(lambda)+3*lambda^4*p^2*exp(lambda)+24*lambda*p*exp(lambda)+12*lambda^2*p*exp(lambda)+12*exp(lambda)-6*lambda^5*p^2-lambda^6*p^2-15*lambda^4*p^2-24*lambda^3*p^2-12*lambda^2*p^2-24*lambda*p-36*lambda^2*p-4*lambda^4*p-12*lambda^3*p-12-12*lambda-2*lambda^3)/(exp(lambda)-1-lambda)/(lambda^2*p+2*lambda*p+2)/(lambda^3*p+3*lambda^2*p+6*lambda*p+6), 2*(lambda*p^2*exp(lambda)-lambda*p^2+lambda*p-p+p*exp(lambda)+1)*lambda^(7/2)*3^(1/2)/(lambda^2*p+2*lambda*p+2)^(1/2)/(lambda^3*p+3*lambda^2*p+6*lambda*p+6)/(lambda^4*p+24*lambda*p+4*lambda^3*p+12*lambda^2*p+24)^(1/2)/(exp(lambda)-1-lambda),
			-(lambda*p^2*exp(lambda)-lambda*p^2+lambda*p-p+p*exp(lambda)+1)*6^(1/2)*lambda^3/(lambda^3*p+3*lambda^2*p+6*lambda*p+6)^(1/2)/(lambda^4*p+24*lambda*p+4*lambda^3*p+12*lambda^2*p+24)^(1/2)/(exp(lambda)-1-lambda)/(lambda*p+1)^(1/2)/(lambda^2*p+2*lambda*p+2)^(1/2), 2*(lambda*p^2*exp(lambda)-lambda*p^2+lambda*p-p+p*exp(lambda)+1)*lambda^(7/2)*3^(1/2)/(lambda^2*p+2*lambda*p+2)^(1/2)/(lambda^3*p+3*lambda^2*p+6*lambda*p+6)/(lambda^4*p+24*lambda*p+4*lambda^3*p+12*lambda^2*p+24)^(1/2)/(exp(lambda)-1-lambda), (-144-12*lambda^5*p+48*lambda^3*p*exp(lambda)+7*lambda^6*p^2*exp(lambda)+lambda^7*p^2*exp(lambda)+144*exp(lambda)+24*lambda^5*p^2*exp(lambda)+84*lambda^4*p^2*exp(lambda)+288*lambda*p*exp(lambda)+144*p^2*exp(lambda)*lambda^2-144*lambda^2*p^2-108*lambda^5*p^2-228*lambda^4*p^2-288*lambda^3*p^2-37*lambda^6*p^2-432*lambda^2*p-144*lambda+144*lambda^2*p*exp(lambda)+144*lambda^3*p^2*exp(lambda)-288*lambda*p-8*lambda^7*p^2-lambda^8*p^2-6*lambda^4-192*lambda^3*p-48*lambda^4*p)/(lambda^3*p+3*lambda^2*p+6*lambda*p+6)/(lambda^4*p+24*lambda*p+4*lambda^3*p+12*lambda^2*p+24)/(exp(lambda)-1-lambda)), nrow=3, byrow=TRUE)
	return(C)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
