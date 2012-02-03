\name{make.polynomial.disc}
\Rdversion{1.1}
\alias{make.polynomial.disc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make.polynomial.disc(f, k, moments = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
%%     ~~Describe \code{f} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{moments}{
%%     ~~Describe \code{moments} here~~
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
function (f, k, moments = NA) 
{
# makes orthonormal polynomials from order 0 up to k
h.all<-list()
a<-matrix(0,nrow=k+1,ncol=k+1) # rows=order of x, cols=order of poly
cte<-rep(0,k+1)
    h <- function(x) {
        y <- 1
        return(y)
    }
    a[1,1]<-1
    cte[1]<-1
    h.all[[1]]<-h
    if(k>0) {
        if (is.na(moments[1])) {
		mus <- prepare.poly.disc(f, k)
   	  }
	  else {
	  	mus <- moments
	  }
	  for(r in 1:k) {
          a.cte<-coeff.pol(i=r,a=a,cte=cte,mus=mus)
          cte<-a.cte$cte
          a<-a.cte$a
          h <- function(x) {
            y <- 0
            for (j in 0:r) {
                y <- y + a[j+1,r+1]*x^j
            }
            y <- y/sqrt(cte[r+1])
            return(y)
          }
          h.all[[r+1]]<-h
        }
    }
    return(h.all)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
