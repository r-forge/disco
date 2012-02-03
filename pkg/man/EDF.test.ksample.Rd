\name{EDF.test.ksample}
\Rdversion{1.1}
\alias{EDF.test.ksample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EDF.test.ksample(data, B = 100, type = "AD", formula = NULL, dname = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{B}{
%%     ~~Describe \code{B} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{dname}{
%%     ~~Describe \code{dname} here~~
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
function(data,B=100,type="AD",formula=NULL,dname=NULL) {
	METHOD<-NA
	if(type=="AD") {
		t.obs<-ADk.test(data)
		METHOD<-"The k-sample Anderson Darling test"
		names(t.obs)<-"AD"
		PARAMETER<-length(data)
		names(PARAMETER)<-"k"
	    p.value<-approx.perm.k(data,stat=ADk.test,B=B)
	}
	if(type=="CvM") {
		t.obs<-CvMk.test(data)
		METHOD<-"The k-sample Cramer-von Mises test"
		names(t.obs)<-"CvM"
		PARAMETER<-length(data)
		names(PARAMETER)<-"k"
	    p.value<-approx.perm.k(data,stat=CvMk.test,B=B)
	}
	if(is.na(METHOD)) stop("The test of type ",type," is not implemented")
	if(is.na(B)) {
			METHOD<-paste(METHOD,"(asymptotic approximation)")
	}
	if(!is.na(B)) {
			METHOD<-paste(METHOD,"(based on",B,"random permutations)")
	}
	name<-paste(dname," with response: ",formula[2]," and grouping variable: ",formula[3])
	RVAL<-list(statistic=c(t.obs), p.value=p.value,
        method=METHOD,
        data.name=name,parameter=PARAMETER)
    class(RVAL)<-"htest"
    return(RVAL)  
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
