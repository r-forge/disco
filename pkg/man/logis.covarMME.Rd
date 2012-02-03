\name{logis.covarMME}
\Rdversion{1.1}
\alias{logis.covarMME}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
logis.covarMME()
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
	I <- diag(rep(1,2))
 	S_t1b<-matrix(c(sqrt(3)/pi,0,
                  0,2/sqrt(3.2)),nrow=2,byrow=TRUE)
 	S_t2b<-matrix(c(-sqrt(7)/(6*pi),0,
                  0,-1/3),nrow=2,byrow=TRUE)
      K <- S_t2b \%*\% solve(S_t1b)
      C <- I + K \%*\% t(K)
	# a little less precise: 
	#C<-matrix(c(1.064815,0,
      #            0,1.088889),ncol=2,byrow=TRUE)
	return(C)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
