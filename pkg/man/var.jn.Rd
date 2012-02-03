\name{var.jn}
\Rdversion{1.1}
\alias{var.jn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
var.jn(x1, x2, fn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{
%%     ~~Describe \code{x1} here~~
}
  \item{x2}{
%%     ~~Describe \code{x2} here~~
}
  \item{fn}{
%%     ~~Describe \code{fn} here~~
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
function(x1,x2,fn) {
	n1<-length(x1)
  	stat<-c(x1,x2)
  	for(i in 1:n1) {
  	  	stat[i]<-comp(x1[-i],x2,fn)
  	}
  	for(i in 1:length(x2)) {
  	  	stat[i+n1]<-comp(x1,x2[-i],fn)
  	}
	v<-var(stat)*(length(stat)-1)^2/length(stat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
