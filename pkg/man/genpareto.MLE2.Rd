\name{genpareto.MLE2}
\Rdversion{1.1}
\alias{genpareto.MLE2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
genpareto.MLE2(sample)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sample}{
%%     ~~Describe \code{sample} here~~
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
function(sample) { #methode van Choulakian (met herschaling)
	n<-length(sample)
	xbar<-mean(sample)
	x<-sample/xbar
	thetapos<-seq(-4*xbar,1/max(x),length.out=4000)
	start<- -n-sum(log(1-thetapos[1]*x))-n*log(-sum(log(1-thetapos[1]*x))/(n*thetapos[1]))
	thetadef<- thetapos[1]
	thetapos<-thetapos[-c(1,4000)]
	for (theta in thetapos) {
		tmp<--n-sum(log(1-theta*x))-n*log(-sum(log(1-theta*x))/(n*theta))
		if (tmp>start) {	
			start<-tmp
			thetadef<-theta
		}
	}
	theta<-thetadef
	k <- -sum(log(1-theta*x))/n
	s <- k/theta*xbar
	return(c(s,k))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
