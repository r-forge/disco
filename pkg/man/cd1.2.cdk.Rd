\name{cd1.2.cdk}
\Rdversion{1.1}
\alias{cd1.2.cdk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cd1.2.cdk(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
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
function(obj) {
#	S<-list()
	S<-obj
	S$stat<-obj$statistics[length(obj$statistics)]
	S$comp<-obj$statistics[-length(obj$statistics)]^2
	S$p.value<-obj$p.value
	S$par.est<-obj$par.est
	S$names.par<-names(obj$par.est)
	S$EVar<-obj$EVar
	S$Sigma<-obj$Sigma
	S$K<-1
#	S$order<-obj$order
#	S$order.select<-obj$order.select
#	S$distr<-obj$distr

	class(S)<-"cdk"
	return(S)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
