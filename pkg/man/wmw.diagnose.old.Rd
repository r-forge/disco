\name{wmw.diagnose.old}
\Rdversion{1.1}
\alias{wmw.diagnose.old}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wmw.diagnose.old(x1, x2)
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
  # function to diagnose some of the assumptions underlying the WMW test
  n1<-length(x1)
  n2<-length(x2)
  cnt1<-0
  cnt2<-0
  for(i in 1:10000) {
    if(max(sample(x1,2))<=sample(x2,1)) cnt1<-cnt1+1 
    if(max(sample(x2,2))<=sample(x1,1)) cnt2<-cnt2+1 
  }
  p112<-cnt1/10000
  p221<-cnt2/10000
  vmw<-var.mw(x1,x2)$var

  tmp<-list(p112=p112,p221=p221,var=vmw,n1=n1,n2=n2)
  class(tmp)<-"wmwdiagnose"

  tmp
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
