\name{approx.perm.k}
\Rdversion{1.1}
\alias{approx.perm.k}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
approx.perm.k(data, stat, B)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{stat}{
%%     ~~Describe \code{stat} here~~
}
  \item{B}{
%%     ~~Describe \code{B} here~~
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
function(data,stat,B) {
	k<-length(data)
	z<-data[[1]]
	n<-c(0,length(data[[1]]))
	for(s in 2:k) {
		z<-c(z,data[[s]])
		n<-c(n,n[s]+length(data[[s]]))
	}
	d<-c()
	data2<-data
	for(i in 1:B) {
		ind<-sample(1:length(z))
		data2<-lapply(1:k,function(j,ind2,z2,n2) {
			 z2[ind2[(n2[j]+1):n2[j+1]]]}, ind2=ind,z2=z,n2=n)
		d<-c(d,stat(data2))
	}
	p<-mean(d>=stat(data))
	invisible(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
