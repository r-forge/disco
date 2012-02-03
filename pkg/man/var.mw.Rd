\name{var.mw}
\Rdversion{1.1}
\alias{var.mw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
var.mw(x1, x2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{
%%     ~~Describe \code{x1} here~~
}
  \item{x2}{
%%     ~~Describe \code{x2} here~~
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
function(x1,x2) {
  n1<-length(x1)
  n2<-length(x2)

  #Pr(x1<=x2) 
  p12<-as.numeric(1-wilcox.test(x1,x2)$stat/(n1*n2))

  # Pr(max(x11,x12)<x2)
  tmp<-expand.grid(x1,x1)
  mx<-pmax(tmp[,1],tmp[,2])
  remove("tmp")
  tmp<-expand.grid(mx,x2)
  p112<-mean(tmp[,1]<=tmp[,2])
  remove("tmp") 
  # Pr(max(x21,x22)<x1)
  tmp<-expand.grid(x2,x2)
  mx<-pmax(tmp[,1],tmp[,2]) 
  remove("tmp") 
  tmp<-expand.grid(mx,x1)
  p221<-mean(tmp[,1]<=tmp[,2])
  remove("tmp") 

  v<-((n1-1)*(p112-p12^2)+(n2-1)*(p221-(1-p12)^2)+p12*(1-p12))/(n1*n2)
  return(list(var=v,mw=p12))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
