\name{print.cdk}
\Rdversion{1.1}
\alias{print.cdk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.cdk(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function(x,...) {
	if(is.null(x$criterion)) {
     	if(x$K>1) {
     		cat("K-sample smooth goodness-of-fit test (K=",x$K,")\n\n",sep="")
     	}
     	if(x$K==1) {
     		cat("One-sample smooth goodness-of-fit test\n\n")
     		cat("Null hypothesis:",x$distr,"against",x$order,ist(x$order),"order alternative\n",sep=" ")
     		cat("Nuisance parameter estimation:",x$method.est,"\n",sep=" ")
     		cat("Parameter estimates:",x$par.est,"(",x$names.par,")\n\n",sep=" ")
     	}
     	cat("Smooth test statistic T_k =",round(x$stat,4)," p-value =",round(x$p.value[length(x$p.value)],4),"\n\n")
    }
    if(!is.null(x$criterion)) {
    	if(x$K>1) {
    		cat("Adaptive K-sample smooth goodness-of-fit test (K=",x$K,")\n\n",sep="")
    	}
    	if(x$K==1) {
    		cat("Adaptive One-sample smooth goodness-of-fit test\n\n",sep="")
    	}	
    	cat("Horizon: ",x$horizon," selection (max. order = ",x$order,")\n",sep="")
    	cat("Selection rule:",x$criterion,"\n\n")
    	cat("Adaptive smooth test statistic T_k =",round(x$stat,4)," p-value =",round(x$p.value[length(x$p.value)],4),"\n")
    	cat("Selected components: ",paste(as.character(x$order.select),collapse=","),"\n\n")
    	cat("Components\n")
    }
#    if(x$K>1) {
#    	seq.order<-1:x$order
#    }
#    if(x$K==1) {
#    	seq.order<-(length(x$par.est)+1):(length(x$par.est)+length(x$comp))
#    }
    cnt<-1
   	for(i in x$order.select) {
   	  	cat("    ",i,ist(i),"component =",sgnspc(x$comp[cnt]),round(x$comp[cnt],4)," p-value =",round(x$p.value[cnt],4),"\n")
   	  	cnt<-cnt+1
   	}
   	cat("\n")
   	cat("All p-values are obtained by",x$method.p.value,"\n\n")
   	
   	if(x$K==2) {
   	  cat("Estimation of likely orderings\n")
   	  cat("  Pr(X1<=X2) =",round(x$diagnose$prob2$estimate,4),"\n")
   	  cat("  Pr(max(X11,X12)<=X2) =",round(x$diagnose$prob3$p112,4),"\n")
   	  cat("  Pr(max(X11,X12)<=X2) =",round(x$diagnose$prob3$p221,4),"\n")  
   	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
