\name{var.logis2}
\Rdversion{1.1}
\alias{var.logis2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
var.logis2(r, z, sigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{
%%     ~~Describe \code{r} here~~
}
  \item{z}{
%%     ~~Describe \code{z} here~~
}
  \item{sigma}{
%%     ~~Describe \code{sigma} here~~
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
function(r,z,sigma) {
  n<-length(z)
  Eh.Dmu<-switch(r-2,
	mean(h3.Dmu.logis2(z,sigma)),
	mean(h4.Dmu.logis2(z,sigma)),
	mean(h5.Dmu.logis2(z,sigma)),
	mean(h6.Dmu.logis2(z,sigma)),
	mean(h7.Dmu.logis2(z,sigma)),
	mean(h8.Dmu.logis2(z,sigma)))
  Eh.Dsigma<-switch(r-2,
	mean(h3.Dsigma.logis2(z,sigma)),
	mean(h4.Dsigma.logis2(z,sigma)),
	mean(h5.Dsigma.logis2(z,sigma)),
	mean(h6.Dsigma.logis2(z,sigma)),
	mean(h7.Dsigma.logis2(z,sigma)),
	mean(h8.Dsigma.logis2(z,sigma)))
  v<-mean((g_r.logis2(r,z,sigma,Eh.Dmu,Eh.Dsigma))^2)-(mean(g_r.logis2(r,z,sigma,Eh.Dmu,Eh.Dsigma)))^2
  v<-v*n/(n-1)
  return(v)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
