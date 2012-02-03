\name{pr12.2sample}
\Rdversion{1.1}
\alias{pr12.2sample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pr12.2sample(x, y, xname, yname, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{xname}{
%%     ~~Describe \code{xname} here~~
}
  \item{yname}{
%%     ~~Describe \code{yname} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
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
function(x,y,xname,yname,alpha=0.05) {
  # computes Pr(X<=Y)
  w<-wilcox.test(x,y)$stat
  n1<-length(x)
  n2<-length(y)
  pr<-1-w/(n1*n2)
  z<-qnorm(1-alpha/2)
  xs1<-sort(x)
  xs2<-sort(y)
  r1<-y
  r2<-x
  for(j in 1:n2) {
    r1[j]<-length(xs1[xs1<xs2[j]])
  } 
  for(i in 1:n1) {
    r2[i]<-length(xs2[xs2<xs1[i]])
  } 
  K<-(sum(r1*(r1-1))+sum(r2*(r2-1)))/(n1*n2)-(n2-1)
  theta<-ifelse(((pr==0)|(pr==1)),1,((K+2*(n2-1)*pr)/(n1+n2-2)-pr^2)/(pr*(1-pr)))
  gamma<-ifelse(theta<0,1,theta*(n1+n2-2)+1)
  cc<-1+gamma*z^2/(n1*n2)
  B<-sqrt((pr*(1-pr)*gamma*z^2+gamma^2*z^4/(4*n1*n2))/(n1*n2))
  A<-pr+gamma*z^2/(2*n1*n2)
  low<-(A-B)/cc
  up<-(A+B)/cc
  
  tmp<-list(estimate=pr,lower.bound=low,upper.bound=up,alpha=alpha)
  invisible(tmp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
