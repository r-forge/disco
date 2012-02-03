\name{smooth.test}
\Rdversion{1.1}
\alias{smooth.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Smooth tests for the one-sample and K-sample goodness-of-fit problems
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function performs one-sample and K-sample smooth goodness-of-fit tests. It also includes generalised smooth tests. For the one-sample problem, MLE and MME can be used for nuisance parameter estimation, and asymptotic approximations and the bootstrap are available for p-value calculation. Data-driven versions are also available. 
}
\usage{
smooth.test(x, y = NULL, data = NULL, order, distr = "unif", method = "NONE", pars = c(0, 1), B = NULL, rescale = F, f = NA, moments = NA, typedistr = "cont", chol = FALSE, ntrials = NA, basis = "Lg", var = "jn", st = F, horizon = "order", criterion = "AIC", graph = c(F, F))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
When \code{x} is a numeric vector and \code{y} is NULL, then a one-sample smooth test will be performed. When \code{x} and \code{y} are numeric vectors, a two-sample smooth test will be performed. When \code{y} is NULL, and \code{x} is a formula, the test for the specified model will be performed. See Details for further information. 
}
  \item{y}{
%%     ~~Describe \code{y} here~~
  When \code{y} is not NULL (default), it should be a numeric vector and a two-sample smooth test will be performed. See also \code{x}. 
}
  \item{data}{
%%     ~~Describe \code{data} here~~
When \code{x} is a formula, \code{data} should be the name of the data set. 
}
  \item{order}{
%%     ~~Describe \code{order} here~~
The order of the smooth test. When \code{criterion} is not NULL, i.e. when a data-driven smooth test is specified, \code{order} refers to the maximal order in the search horizon. 
}
  \item{distr}{
%%     ~~Describe \code{distr} here~~
The name (character string) of the distribution to be tested for (only for the one-sample test). It must be one of 'norm' (normal), 'exp' (exponential), 'logar' (logarithmic), 'pois' (Poisson), 'geom' (Geometric), 'ZTP' (zero-truncated Poisson), logis' (logistic), 'negb' (negative binomial), 'laplace' (Laplace), 'ZIP' (zero-inlfated Poisson), extrval' (extreme value), 'unif' (uniform), genpareto' (generalised Pareto), 'betab' (beta binomial), 'gamma' (gamma) or 'NONE' (when 'NONE' is specifief, \code{f} or \code{moments} should be specified). 
}
  \item{method}{
%%     ~~Describe \code{method} here~~
The method of nuisance parameter estimation (only for the one-sample test). It must be one of 'MLE' (maximum likelihood estimation), 'MME' (method of moments estimation) or 'NONE' (default). The latter is only to be used for simple null hypotheses. 
}
  \item{pars}{
%%     ~~Describe \code{pars} here~~
The parameter values of \code{distr} when a simple null hypothesis is to be tested (i.e. when \code{method}='NONE'). See Details for further information.
}
  \item{B}{
%%     ~~Describe \code{B} here~~
The number of bootstrap runs for the p-value calculation. When B=NULL (default) p-values are calculated using the asymptotic approximation. 
}
  \item{rescale}{
%%     ~~Describe \code{rescale} here~~
When \code{rescale}=T the components are rescaled using empirical variance estimates. See Thas et al. (2009), Klar (???) or Chapter ??? in Rayner et al. (2009) for details. Default is \code{rescale}=F, for which the asymptotic variances under the null hypothesis are used. 
}
  \item{f}{
%%     ~~Describe \code{f} here~~
A density function. When \code{f} is provided, a simple one-sample goodness-of-fit test for this density function will be performed. When \code{f} is not NA, \code{distr} should be set to 'NONE' and \code{moments} should be set to NA. Default is \code{f}=NA. See Details for further information. 
}
  \item{moments}{
%%     ~~Describe \code{moments} here~~
A numeric vector of length 2*\code{order}. Then \code{moments} is specified, a one-sample generalised smooth test will be performed to test the null hypothesis that the first \code{order} moments of the true distribution coincide with the first \code{order} moments specified in \code{moments}. When \code{moments} is specified, \code{distr} should be set to 'NONE' and \code{f} to NA.  
}
  \item{typedistr}{
%%     ~~Describe \code{typedistr} here~~
It specifies whether f refers to a continuous ('cont', default) or a discrete ('disc') distribution. It must only be specified when \code{f} is specified. 
}
  \item{chol}{
%%     ~~Describe \code{chol} here~~
When \code{chol}=T, a Cholesky decomposition is applied to obtain the components. It is only available for one-sample test. Default is \code{chol}=F.
}
  \item{ntrials}{
%%     ~~Describe \code{ntrials} here~~
A positive natural number representing the fixed number of trials in the beta binomial distribution, only used when \code{distr}='betab'.
}
  \item{basis}{
%%     ~~Describe \code{basis} here~~
A character string to specify the orthonormal basis used to compute the components. This is only required for the K-sample tests. Default is \code{basis}='Lg', referring to the Legendre basis. 
}
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{st}{
%%     ~~Describe \code{st} here~~
}
  \item{horizon}{
%%     ~~Describe \code{horizon} here~~
A character string that specifies the type of model selection method used for the data-driven smooth tests. It must be one of 'order' (order selection, default), 'subset' (subset selection) or NULL. When \code{horizon}=NULL, the smooth test with fixed order fill be performed (i.e. no data-driven test). When a data-driven test is performed, \code{order} refers to the maximal order of the models in the search horizon. 
}
  \item{criterion}{
%%     ~~Describe \code{criterion} here~~
A character string that specifies the model selection criterion. It must be one of 'AIC' (default), 'BIC' or 'MISE'. 
}
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
This R function encompasses a large class of smooth tests. The one-sample smooth tests of goodness-of-fit have been described in the book of Rayner et al. (2009). Almost all tests discussed in that book are included in the \code{Smooth.test} function. Because of the close relation between the one-sample methods and the K-sample methods, the \code{Smooth.test} function also includes the smooth tests for the latter. More details on this class of tests and the relations between all the different types of smooth tests, may be found in Thas (2010). 

The type of test may be specified through a formula in the \code{x} argument of the \code{Smooth.test} function. In particular, when Y is a variable name of a \code{\link{data.frame}} object, say \code{test}, then \code{Y~1} together with \code{data}=test and \code{distr}='norm' specifies that a one-sample smooth test for testing normality will be performed. When the formula is of the form \code{Y~X}, in which X is the variable name of a \code{\link{factor}} in the test \code{data.frame}, then a K-sample smooth test will be performed. 

When a one-sample smooth test must be performed for a distribution that is not implemented in the \code{Smooth.test} function, a user-specified density function may be used by setting \code{f} to this user-specified function. It should be an R function with only one argument (the quantile) and which returns a numeric element (the density). No unknown nuisance parameters are allowed. The \code{Smooth.test} function relies on the \code{\link{orth.poly}} function which generates polynomials that are orthonormal to the density function specified in \code{f}. See Rayner et al. (2008) and Emerson (1968) for more details on these algorithms. 

For the distributions that are implemented in \code{Smooth.test} (i.e for norm, exp, logarithmic, logistic, gamma, geometric, uniform, ZIP, ZTP, Poisson, beta binomial, negative binomial, extreme value and generalised Pareto), the argument \code{pars} refers to the parameters of these distributions. The definitions of these parameters depend on the distribution (\code{distr}):
\item{norm}{mean,sd}
\item{logis}{location,scale}
\item{negb}{size,prob}
\item{laplace}{location,scale}
\item{ZIP}{lambda,phi}
\item{extrval}{location,scale}
\item{exp}{rate}
\item{logar}{prob}
\item{pois}{lambda}
\item{geom}{prob}
\item{ZTP}{lambda}
\item{unif}{min.max}
\item{genpareto}{scale,-shape}
\item{betab}{size=\code{ntrials},shape1,shape2}
\item{gamma}{shape,scale}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
The function returns an object of the \code{\link{class}} "\code{cdk}". 

An object of the class "\code{cdk}" is a list with at least the following components:
\item{stat}{the (data-driven) smooth test statistic}
\item{comp}{a numeric vector that contains the components of the (data-driven) test statistic}
\item{p.value}{a numeric vector that contains the p-values of the components (same order as in \code{comp}). The last element of \code{p.value} is the p-value that corresponds to \code{stat}.}
\item{par.est}{the nuisance parameter estimates (only for the one-sample tests)}
\item{names.par}{a character vector that contains the names of the nuisance parameters (only for the one-sample tests)}
\item{EVar}{the empirical variance estimates (only for the one-sample tests when \code{rescale}=T)}
\item{Sigma}{the covariance matrix used for the calculation of \code{stat} (only for the one-sample tests)}
\item{K}{\code{K}=1 for the one-sample tests, and \code{K} refers to the number of samples for the K-sample tests}
\item{order}{the order of the smooth test, or the maximal order in the horizon for the data-driven smooth tests}
\item{order.select}{a numeric vector referring to the selected components (only for the data-driven tests)}
\item{distr}{the argument \code{distr}}
\item{method.p.value}{character string indicating the method of p-value calculation}

}
\references{
Rayner, J.C.W., Thas, O. and Best, D.J. (2009). Smooth Tests of Goodness of Fit -- Using R. Wiley, Singapore. 
Thas, O. (2009). Comparing Distributions. Springer, New-York. 
Rayner, J.C.W., Thas, O. and De Boeck, B. (2008). A generalized Emerson recurrence relation. Australian and New Zealand Journal of Statistics, 50, 235-240.
Emerson ...
Klar ... 
}
\author{
%%  ~~who you are~~
Olivier Thas and Bert De Boeck
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
The function \code{\link{diagnostic}} works on objects of the \code{cdk} class, and it returns some diagnostics (only for the 2-sample tests). The function \code{\link{components}} works also on the \code{cdk} class; it give detailed output for the components. 
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x,y=NULL,data=NULL,order,distr="unif",method="NONE",pars=c(0,1),B=NULL,rescale=F,f=NA,moments=NA,typedistr="cont",chol=FALSE,ntrials=NA,basis="Lg",var="jn",st=F,horizon="order",criterion="AIC",graph=c(F,F)) {
	onesample<-TRUE
	g<-NA
	resp<-NA
	if(!is.null(y)) {
		onesample<-FALSE
		# two-sample --> cd
		DNAME1<-deparse(substitute(x))
  		assign(DNAME1,x)
  		DNAME2<-deparse(substitute(y))
  		assign(DNAME2,y)
		RES<-cd(x1=eval(parse(text=DNAME1)),x2=eval(parse(text=DNAME2)),order=order,basis=basis,var=var,st=st,horizon=horizon,criterion=criterion,graph=graph)
	}
	if(class(x)=="formula") {
		mf <- match.call(expand.dots = FALSE)
    	m <- match(c("x", "data", "subset", "weights", "na.action", 
        	"offset"), names(mf), 0L) # I changed formula with x
    	mf <- mf[c(1L, m)]
    	names(mf)[2]<-"formula"
    	mf$drop.unused.levels <- TRUE
    	mf[[1L]] <- as.name("model.frame")
    	mf <- eval(mf, parent.frame())
        DNAME <- names(mf)[1] # name of response variable
    	response <- attr(attr(mf, "terms"), "response")
  		if(terms(x)[[3]]==1) {
  			# one-sample --> smooth.test
  		   	x.vector<-mf[[response]]
  		   	assign(DNAME,x.vector)
  		}
  		if(terms(x)[[3]]!=1) {
  			# 2- or k-sample --> cd or cdk
  			onesample<-FALSE
  			g <-(mf[[-response]])
  			resp<-mf[[response]]
  			assign(DNAME,resp)
  			RES<-cdk(x=eval(parse(text=DNAME)),g=g,order=order,basis=basis,var=var,st=st,criterion=criterion)
  		}
  	}
  	else {
  		DNAME<-deparse(substitute(x))
  		assign(DNAME,x)
  	}
  	if(onesample) {
  		if(is.null(criterion)) {
  			tmp<-cd1(sample=eval(parse(text=DNAME)),order=order,distr=distr,method=method,pars=pars,B=B,rescale=rescale,f=f,moments=moments,typedistr=typedistr,chol=chol,output=F,ntrials=ntrials)
  			RES<-cd1.2.cdk(tmp)
  		}
  		if(!is.null(criterion)) {
  			RES<-ddcd1(sample=eval(parse(text=DNAME)),max.order=order,horizon=horizon,criterion=criterion,distr=distr,method=method,pars=pars,B=B,f=f,typedistr=typedistr,ntrials,plot=F) 
  		}
  	}
    #return(list(RES=RES,g=g,resp=resp)) # debug mode
    return(RES)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{smooth test}
\keyword{goodness-of-fit}% __ONLY ONE__ keyword per line
