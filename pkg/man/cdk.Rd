\name{cdk}
\Rdversion{1.1}
\alias{cdk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cdk(x, g, order = 4, basis = "Lg", var = "jn", st = F, criterion = "AIC")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{g}{
%%     ~~Describe \code{g} here~~
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
  \item{criterion}{
%%     ~~Describe \code{criterion} here~~
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
function(x,g,order=4,basis="Lg",var="jn",st=F,criterion="AIC") {
	n<-length(x)
	ns<-as.numeric(table(g))
	k<-length(ns)
	r<-(rank(x)-0.5)/n
	comp<-matrix(nrow=k,ncol=order)
	cmp<-comp
	v<-comp
	order.select<-1:order
#	if(var=="emp") {
#		tmp<-var.emp(x1,x2,o=order)
#	}
	x1<-x[1:ns[1]]
	x2<-x[(ns[1]+1):(ns[2]+ns[1])]
	for(i in 1:order) {
		fn<-Lg(i)
		ind<-1
		for(s in 1:k) {
  		  comp[s,i]<-sqrt(ns[s])*mean(fn(r[ind:(ns[s]+ind-1)]))
  		  cmp[s,i]<-comp[s,i]*sqrt(n/ns[s]) # where does this factor come from?
  		  ind<-ind+ns[s]
  		}
#		v[i]<-switch(var,
#			jn=var.jn(x1,x2,fn),
#			emp=ifelse(i<=4,tmp[i],NA))
	}
	stat<-sum(cmp^2)
#	if(!st) return(list(stat=stat,comp=comp,var=v,statst=comp/sqrt(v)))
#	if(st) return(comp/sqrt(v))
    comps<-colMeans(cmp^2)
    stat<-sum(comps)

   	# adaptive : only order implemented
   	if(!is.null(criterion)) {
  	  penalty=switch(criterion,
	  	AIC=2*(k-1),
		BIC=(k-1)*log(n))
	  crit<-cumsum(comps-penalty) # this doesn't seem right! penalty does not change with order
	  order.select<-1:((1:order)[crit==max(crit)])
	  stat<-cumsum(comps)[crit==max(crit)]
	}

    p<-1-pchisq(comps,df=k-1)
    p.stat<-1-pchisq(stat,df=order*(k-1)) # note: not correct for adaptive test
    prob2<-NA
    prob3<-NA
    if(k==2) {
	  prob2<-pr12(x1,x2)
	  prob3<-wmw.diagnose(x1,x2)	
	}
	
    tmp<-list(comp.ind=comp,comp=comps,stat=sum(comps),p.values=c(p,p.stat),EVar=NA,order=order,basis=basis,horizon="order",criterion=criterion,method.var=NA,method.p.value="the asymptotic approximation",K=k,order.select=order.select,diagnose=list(prob2=prob2,prob3=prob3))
    class(tmp)<-"cdk"
    
    # important note: for the K-sample test the components are squared!
    
    tmp
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
