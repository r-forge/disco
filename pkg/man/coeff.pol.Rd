\name{coeff.pol}
\Rdversion{1.1}
\alias{coeff.pol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
coeff.pol(i, a, cte, mus)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i}{
%%     ~~Describe \code{i} here~~
}
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{cte}{
%%     ~~Describe \code{cte} here~~
}
  \item{mus}{
%%     ~~Describe \code{mus} here~~
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
function (i, a, cte, mus) 
{
# j: order of x^j in polynomial
# i: order of polynomial
# a matrix of a coefficients
# cte vector with norm. constants
# mus: moments
# returns $a_{j,i}$ in orthogonal polynomial as wel as cte=c (norm constant)
    if (i == 0) {
        a[0+1,i+1] <- 1
        cte[1]<-1
    }
    if (i == 1) {
        a[0+1,i+1] <- -mus[1]     #/sqrt(mus[2]-mus[1]^2)
        a[1+1,i+1] <- 1           #/sqrt(mus[2]-mus[1]^2)
        cte[2]<-mus[2]-mus[1]^2       # changed to noncentral moments
    }
    if (i >= 2) {
        b1<-0
        for(k in 0:(i-1)) {
          for(m in 0:(i-1)) {
            b1<-b1-a[m+1,(i-1)+1]*a[k+1,(i-1)+1]*mus[m+k+1]
          }
        }
        b1<-b1/cte[(i-1)+1]
        b2<-0
        for(k in 0:(i-2)) {
          for(m in 0:(i-1)) {
            b2<-b2-a[m+1,(i-1)+1]*a[k+1,(i-2)+1]*mus[m+k+1]
          }
        }
        b2<-b2/sqrt(cte[(i-1)+1]*cte[(i-2)+1])
        ct<-0
        for(k in 0:(i-1)) {
          for(m in 0:(i-1)) {
            ct<-ct+a[m+1,(i-1)+1]*a[k+1,(i-1)+1]*mus[m+k+2]
          }
        }
        cte[i+1]<-ct/cte[(i-1)+1]-b1^2-b2^2
	  for(j in 0:i) {
          if (j == 0) {
            a[j+1,i+1] <- b1*a[j+1,(i-1)+1]/sqrt(cte[(i-1)+1])+b2*a[j+1,(i-2)+1]/sqrt(cte[(i-2)+1])
          }
          else {
            a[j+1,i+1] <- a[(j-1)+1,(i-1)+1]/sqrt(cte[(i-1)+1])+b1*a[j+1,(i-1)+1]/sqrt(cte[(i-1)+1])+b2*a[j+1,(i-2)+1]/sqrt(cte[(i-2)+1])
          }
	  }  
    }
  return(list(a=a,cte=cte))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
