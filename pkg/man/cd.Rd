\name{cd}
\Rdversion{1.1}
\alias{cd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cd(x1, x2, order = 4, basis = "Lg", var = "jn", st = F, horizon = "order", criterion = "AIC", graph = c(F, F))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{
%%     ~~Describe \code{x1} here~~
}
  \item{x2}{
%%     ~~Describe \code{x2} here~~
}
  \item{order}{
%%     ~~Describe \code{order} here~~
}
  \item{basis}{
%%     ~~Describe \code{basis} here~~
}
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{st}{
%%     ~~Describe \code{st} here~~
}
  \item{horizon}{
%%     ~~Describe \code{horizon} here~~
}
  \item{criterion}{
%%     ~~Describe \code{criterion} here~~
}
  \item{graph}{
%%     ~~Describe \code{graph} here~~
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
function(x1,x2,order=4,basis="Lg",var="jn",st=F,horizon="order",criterion="AIC",graph=c(F,F)) {
	n1<-length(x1)
	n2<-length(x2)
	n<-n1+n2
	x<-c(x1,x2)
	r<-(rank(x)-0.5)/(n1+n2)
	comp<-rep(NA,order)
	v<-comp
	if(var=="emp") {
		tmp<-var.emp(x1,x2,o=order)
	}
	for(i in 1:order) {
		fn<-Lg(i)
		comp[i]<-(mean(fn(r[1:n1]))-mean(fn(r[(n1+1):(n1+n2)])))*sqrt(n1*n2/(n))
		v[i]<-switch(var,
			jn=var.jn(x1,x2,fn),
			emp=ifelse(i<=4,tmp[i],NA))
	}
	# adaptive : only order implemented
	penalty=switch(criterion,
		AIC=2,
		BIC=log(n))
	crit<-cumsum(comp^2-penalty)
	stat.dd<-cumsum(comp^2)[crit==max(crit)]
	if(!is.null(criterion)) {
		order.select<-1:((1:order)[crit==max(crit)])
	}	

	if(is.null(criterion)) {
		order.select<-1:order
	}
	# graphs
	theta<-comp*sqrt(n/(n1*n2))
	if(sum(graph)==2) par(mfrow=c(1,2))
	if(graph[1]) {
	  cd1<-1
	  cd2<-1
	  u<-seq(0,1,0.01)
	  for(i in 1:order) {
	    cd1<-cd1+n2/n*theta[i]*Lg(i)(u)	
	    cd2<-cd2-n1/n*theta[i]*Lg(i)(u)	
	  }	
	  mx<-max(c(cd1,cd2))
	  mn<-min(c(cd1,cd2))
	  plot(u,cd1,type="l",ylim=c(mn,mx),ylab="comparison density")
	  lines(u,cd2,lty=2)
	  abline(h=1,lty=3)
    }
	if(graph[2]) {
	  x<-c(x1,x2)
	  h<-density(x)
	  m<-length(h$x)
	  u<-((1:m)-0.5)/m
	  cd1<-1
	  cd2<-1
	  for(i in 1:order) {
	    cd1<-cd1+n2/n*theta[i]*Lg(i)(u)	
	    cd2<-cd2-n1/n*theta[i]*Lg(i)(u)	
	  }	
	  mx<-max(c(cd1,cd2,h$y))
	  mn<-min(c(cd1,cd2,h$y))
	  plot(h$x,cd1*h$y,type="l",ylim=c(mn,mx),lty=1)
	  lines(h$x,cd2*h$y,lty=2)
    }
	stat<-sum(comp^2)
	
	K<-2
	if(K==2) {
	  prob2<-pr12(x1,x2)
	  prob3<-wmw.diagnose(x1,x2)	
	}
	
	B<-NULL # to be used as argument when bootstrap or permutation is used
	mpv<-"the asymptotic approximation"
	if(!is.null(B)) mpv<-paste("means of",B,"permutations")
	tmp<-list(stat=stat,comp=comp,EVar=v,p.value=c(1-pnorm(abs(comp)),NA),order=order,basis=basis,method.var=var,K=2,method.p.value=mpv,order.select=order.select,diagnose=list(prob2=prob2,prob3=prob3)) # now only for non-dd
	class(tmp)<-"cdk"
	#if(!st) return(tmp)
	#if(st) return(comp/sqrt(v))
	#invisible(tmp)
	tmp
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
