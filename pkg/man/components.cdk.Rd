\name{components.cdk}
\Rdversion{1.1}
\alias{components.cdk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
components.cdk(x, order = NULL, contrast = c("sum", NULL), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{order}{
%%     ~~Describe \code{order} here~~
}
  \item{contrast}{
%%     ~~Describe \code{contrast} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function(x,order=NULL,contrast=c("sum",NULL),...) {
	if(is.null(order)) order<-x$order
	#n.comp<-min(length(x$comp),order)
	cat("\n")
	if(x$K==2) {
  	  for(i in 1:order) {
		cat("    ",i,ist(i),"component =",sgnspc(x$comp[i]),x$comp[i]," p-value =",round(x$p.value[i],4),"\n")
      }
    }
    if(x$K>2) {
      m<-x$comp.ind
      n.sample<-nrow(m)
      if(contrast[1]=="control") {
        ctr<-diag(x$K)
        ctr[,as.numeric(contrast[2])]<--1
        ctr[as.numeric(contrast[2]),]<-0
        m<-ctr\%*\%m
      }
      for(i in 1:n.sample) {
        cat("sample ",i,": ",sep="")
        for(j in 1:order) {
        	cat(sgnspc(m[i,j]),round(m[i,j],4),"  ",sep="")
        }
        if(contrast[1]=="sum") cat(" * ",round(sum(m[i,1:order]^2),4),sep="")
        cat("\n")
      }	
      cat("\n")
      if(contrast[1]=="sum") cat("comp    : ",round(x$comp[1:order],4),"\n",sep="  ")
    } 
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
