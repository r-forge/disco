\name{betab.MME}
\Rdversion{1.1}
\alias{betab.MME}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
betab.MME(sample, ntrials)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sample}{
%%     ~~Describe \code{sample} here~~
}
  \item{ntrials}{
%%     ~~Describe \code{ntrials} here~~
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
function(sample,ntrials) { #the number of trials is known (fixed)
	n<-ntrials
	MEAN<-mean(sample)
	VAR<-sum((sample-MEAN)^2)/length(sample)
	alpha<-(n-MEAN-VAR/MEAN)*MEAN/(n*VAR/MEAN+MEAN-n)
	beta<-(n-MEAN-VAR/MEAN)*(n-MEAN)/(n*VAR/MEAN+MEAN-n)
	return(c(alpha,beta))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
