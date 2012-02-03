\name{logis.covarMLE}
\Rdversion{1.1}
\alias{logis.covarMLE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
logis.covarMLE()
}
%- maybe also 'usage' for other objects documented here.
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
function() { # independent of nuisance parameters
	C<-matrix(c(1-9/pi^2,0,sqrt(21)/(2*pi^2),0,
                  0,1-45/(12+4*pi^2),0,3*sqrt(5)/(6+2*pi^2),
                  sqrt(21)/(2*pi^2),0,1-7/(12*pi^2),0,
                  0,3*sqrt(5)/(6+2*pi^2),0,1-1/(3+pi^2)),ncol=4,byrow=TRUE)
	return(C)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
