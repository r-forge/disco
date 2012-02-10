#'Plot a PP plot for one sample
#'
#'This function is called from \code{\link{PPplot}} and should not be called directly.
#'@param x a data vector
#'@param d a distribution function, eg \code{link{qnorm}} or \code{link{qunif}}. The first argument to this function has to be a vector of probabilities. Other parameters to this function should be applied in the argument \code{pars}. See also the examples.
#'@param pars The extra arguments to the distribution function, in the form of a named list. See also the examples.
#'@param blom See the \code{blom} argument of \code{\link{PPplot}}
#'@return This function doesn't return a useful value
#'@note To keep the function consistent with the examples in the book, the argument \code{pars} can also take a named or unnamed vector. This vector is turned into a list internally and a warning is given that this functionality will disappear in a next major revision. Also the argument \code{name} is dropped from an earlier version. Use \code{main} instead
#'@author Olivier Thas, Joris Meys


PPplot.onesample <-
function(x,d=qnorm,pars=list(),blom=0,name=NULL,...) {
  # x: data vector
  # d: specifies the quantile function that has to be used; now only qnorm and qunif are defined 
  # pars: vector with parameters -- correspods to parameters used by q-function
  # blom: parameter used for the plotting positions (according to Blom)
  # name: name to be used on graph instead of x
  dots<-as.list(substitute(list(...)))[-1]
  ndots<-names(dots)
  DNAME<-ifelse(is.null(name),deparse(substitute(x)),name)
  n<-length(x)
  x<-sort(x)
  p<-ppoints(n,a=blom)
  # parameter estimates should be given in pars
  d<-match.fun(d)

  Fhat<-ecdf(x)  

  q<-Fhat(do.call(d,c(list(p),as.list(pars))))
  
  
  
  i.main<-sum(ndots=="main")
  i.ylab<-sum(ndots=="ylab")
  i.xlab<-sum(ndots=="xlab")
  if(i.main==0) main<-paste("PP plot of",DNAME)
	else main<-dots$main
  if(i.xlab==0) xlab<-"p"
	else xlab<-dots$xlab
  if(i.ylab==0) ylab<-"empirical probability"
	else ylab<-dots$ylab
  plot(p,q,main=main,
       ylab=ylab,xlab=xlab)
  abline(c(0,1))
}

