\name{EDF.test.2sample}
\Rdversion{1.1}
\alias{EDF.test.2sample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EDF.test.2sample(x, y, B = 100, type = "AD", xname = NULL, yname = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{B}{
%%     ~~Describe \code{B} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{xname}{
%%     ~~Describe \code{xname} here~~
}
  \item{yname}{
%%     ~~Describe \code{yname} here~~
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
function(x,y,B=100,type="AD",xname=NULL,yname=NULL) {
	METHOD<-NA
	if(type=="AD") {
		t.obs<-AD.test(x,y)
		METHOD<-"The two-sample Anderson-Darling test"
		names(t.obs)<-"AD"
	    p.value<-approx.perm(x,y,stat=AD.test,B=B)
	}
	if(type=="CvM") {
		t.obs<-CvM.test(x,y)
		METHOD<-"The two-sample Cramer-von Mises test"
		names(t.obs)<-"CvM"
	    p.value<-approx.perm(x,y,stat=CvM.test,B=B)
	}
	if(is.na(METHOD)) stop("The test of type ",type," is not implemented")
	if(is.na(B)) {
			METHOD<-paste(METHOD,"(asymptotic approximation)")
	}
	if(!is.na(B)) {
			METHOD<-paste(METHOD,"(based on",B,"random permutations)")
	}
	name<-paste("x:",xname," y:",yname)
	RVAL<-list(statistic=c(t.obs), p.value=p.value,
        method=METHOD,
        data.name=name)
    class(RVAL)<-"htest"
    return(RVAL)  
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
