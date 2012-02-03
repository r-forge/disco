\name{genpareto.MLE}
\Rdversion{1.1}
\alias{genpareto.MLE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
genpareto.MLE(sample)
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
function(sample) {
	n<-length(sample)
	par_start<-genpareto.MME(sample)
	tmp <- try(nls.lm(par_start[2]/par_start[1], genpareto.optfn, sample = sample, n = n),silent=TRUE)
	if (!inherits(tmp,"try-error")) {
	  theta <- tmp$par
	  k <- -sum(log(1-theta*sample))/n
	  s <- k/theta
	  pars <- c(s, k)
	  if (is.nan(pars[1])||is.nan(pars[2])||is.na(pars[1])||is.na(pars[2])||is.infinite(pars[1])||is.infinite(pars[2])) {
		pars<-genpareto.MLE2(sample)
	  }
	  else {
		W1<-(1-k)/s^2*sum(sample/(1-k*sample/s)) - n/s
		W2<- -sum(log(1-k*sample/s))/k^2 - (1/k-1)/s*sum(sample/(1-k*sample/s))
		if (!is.na((W1^2+W2^2)>1)) {
			if ((W1^2+W2^2)>1) {pars<-genpareto.MLE2(sample)}
		}
	      else {pars<-genpareto.MLE2(sample)}
	  }
	}
	else {pars<-genpareto.MLE2(sample)}
	s <- pars[1]
	k <- pars[2]
	return(c(s, k))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
