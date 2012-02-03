\name{var.extrval2}
\Rdversion{1.1}
\alias{var.extrval2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
var.extrval2(r, z, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{
%%     ~~Describe \code{r} here~~
}
  \item{z}{
%%     ~~Describe \code{z} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
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
function(r,z,b) {
  n<-length(z)
  Eh.Da<-switch(r-2,
	mean(h3.Da.extrval2(z,b)),
	mean(h4.Da.extrval2(z,b)),
	mean(h5.Da.extrval2(z,b)),
	mean(h6.Da.extrval2(z,b)),
	mean(h7.Da.extrval2(z,b)),
	mean(h8.Da.extrval2(z,b)))
  Eh.Db<-switch(r-2,
	mean(h3.Db.extrval2(z,b)),
	mean(h4.Db.extrval2(z,b)),
	mean(h5.Db.extrval2(z,b)),
	mean(h6.Db.extrval2(z,b)),
	mean(h7.Db.extrval2(z,b)),
	mean(h8.Db.extrval2(z,b)))
  v<-mean((g_r.extrval2(r,z,b,Eh.Da,Eh.Db))^2)-(mean(g_r.extrval2(r,z,b,Eh.Da,Eh.Db)))^2
  v<-v*n/(n-1)
  return(v)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
