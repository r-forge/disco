\name{make.poly.disc}
\Rdversion{1.1}
\alias{make.poly.disc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make.poly.disc(f, k, moments = NA, coef = F, norm = T)
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
  \item{coef}{
%%     ~~Describe \code{coef} here~~
}
  \item{norm}{
%%     ~~Describe \code{norm} here~~
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
function (f, k, moments = NA, coef = F, norm = T) 
{
    if (coef) {
        r <- make.coeff.disc(f, k, moments, norm)
    }
    else {
        r <- make.polynomial.disc(f, k, moments)
    }
    invisible(r)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
