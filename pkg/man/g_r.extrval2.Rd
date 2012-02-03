\name{g_r.extrval2}
\Rdversion{1.1}
\alias{g_r.extrval2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
g_r.extrval2(r, z, b, Eh.Da, Eh.Db)
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
  \item{Eh.Da}{
%%     ~~Describe \code{Eh.Da} here~~
}
  \item{Eh.Db}{
%%     ~~Describe \code{Eh.Db} here~~
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
function(r,z,b,Eh.Da,Eh.Db) {
  h<-orth.poly(r,"extrval",c(0,1))
  tmp<-h(z)+b.a.extrval2(z,b)*Eh.Da+b.b.extrval2(z,b)*Eh.Db
  return(tmp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
