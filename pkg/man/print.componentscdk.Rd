\name{print.componentscdk}
\Rdversion{1.1}
\alias{print.componentscdk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.componentscdk(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
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
function(obj) {
	squared<-obj$squared
	order<-obj$order
	mo<-obj$x$min.order
	cat("\n")
	if(squared&(obj$x$K<=2)) {
		cat("squared ")
	}
	cat("components\n")
	if((obj$x$K==2)|(obj$x$K==1)) {
	  if(squared) {
	  	cmp<-obj$x$comp
	  }
	  if(!squared) {
	  	cmp<-obj$x$coefficients
	  }
  	  for(i in 1:order) {
		if(i>=mo) {
			cat("    ",i,ist(i),"component =",sgnspc(cmp[i-mo+1]),cmp[i-mo+1]," p-value =",round(obj$x$p.value[i-mo+1],4),"\n")
		}
      }
    }
    if(obj$x$K>2) {
      n.sample<-nrow(obj$m)
      for(i in 1:n.sample) {
        cat("sample ",i,": ",sep="")
        for(j in 1:order) {
        	cat(sgnspc(obj$m[i,j]),round(obj$m[i,j],4),"  ",sep="")
        }
        if(obj$contrast[1]=="sum") cat(" * ",round(sum(obj$m[i,1:order]^2),4),sep="")
        cat("\n")
      }	
      cat("\n")
      if(obj$contrast[1]=="sum") cat("comp    : ",round(obj$x$comp[1:order],4),"\n",sep="  ")
    } 
	invisible(obj)	
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
