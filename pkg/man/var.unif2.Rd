\name{var.unif2}
\Rdversion{1.1}
\alias{var.unif2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
var.unif2(r, z, sigma)
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
  Eh.Da<-switch(r-2,
	mean(h3.Da.unif2(z,sigma)),
	mean(h4.Da.unif2(z,sigma)),
	mean(h5.Da.unif2(z,sigma)),
	mean(h6.Da.unif2(z,sigma)),
	mean(h7.Da.unif2(z,sigma)),
	mean(h8.Da.unif2(z,sigma)))
  Eh.Db<-switch(r-2,
	mean(h3.Db.unif2(z,sigma)),
	mean(h4.Db.unif2(z,sigma)),
	mean(h5.Db.unif2(z,sigma)),
	mean(h6.Db.unif2(z,sigma)),
	mean(h7.Db.unif2(z,sigma)),
	mean(h8.Db.unif2(z,sigma)))
  v<-mean((g_r.unif2(r,z,sigma,Eh.Da,Eh.Db))^2)-(mean(g_r.unif2(r,z,sigma,Eh.Da,Eh.Db)))^2
  v<-v*n/(n-1)
  return(v)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
