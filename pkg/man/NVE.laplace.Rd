\name{NVE.laplace}
\Rdversion{1.1}
\alias{NVE.laplace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
NVE.laplace(sample, order, parest = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sample}{
%%     ~~Describe \code{sample} here~~
}
  \item{order}{
%%     ~~Describe \code{order} here~~
}
  \item{parest}{
%%     ~~Describe \code{parest} here~~
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
function(sample,order,parest=NA){
  #choose order between 3 and 8#
 if (order<9 && order>2) {
  n<-length(sample)
  if (is.na(parest[1])) {
  tmp<-laplace.MME(sample)
  a<-tmp[1]
  b<-tmp[2]
  }
  else {
  a<-parest[1]
  b<-parest[2]
  }
  z<-(sample-a)/b
  var_est<-var.laplace2(order,z,b)
 }
 else {var_est<-NA}
 return(var_est)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
