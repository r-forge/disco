#'Construct a PP plot
#'
#'This function constructs a PP plot for both either one sample or more samples, as explained in the book Comparing distributions.
#'@param x data vector or formula (for 2 and k-sample problems)
#'@param y data vector (only when x is data vector and for the 2-sample problem)
#'@param distr specifies the quantile function that has to be used; now only qnorm and qunif are defined
#'@param pars vector with parameters -- correspods to parameters used by q-function
#'@param blom parameter used for the plotting positions (according to Blom)
#'@param data: data frame (only when x is formula)
#'@param name: name to be used for graphs instead of x (for one sample plots)
#'@param groups: vector with 2 levels of factor for which a PP plot must be plotted (only when x is formula)
#'@param ... : further arguments for the plot function and for \code{\link{PPplot.moresample}}
#'@return This function doesn't return a useful value, it just plots the PPplot
#'@author Olivier Thas
#'@aliases PPplot PP-plot
#'@note More information is given in section 3.2 and 8.1 of the book 'Comparing Distributions' by Olivier Thas.
#'@references Thas Olivier(2009), Comparing Distributions. Springer ( \url{http://www.springer.com/statistics/book/978-0-387-92709-1})

PPplot.default <-
function(x,y=NULL,name=NULL,...) {

  mf <- match.call()
  extras <- match.call(expand.dots=FALSE)$...
  onesample<-TRUE
  DNAME1<-name
                       # JM: extend functionality. Make distr general
  if(!is.null(y)) {
  	onesample<-FALSE
  	mf <- match.call(expand.dots = TRUE)

  	Xind<-match("xlab",names(mf))
  	Yind<-match("ylab",names(mf))
  	XNAME<-deparse(substitute(x))
  	YNAME<-deparse(substitute(y))
  	extras<-match.call(expand.dots=FALSE)$...
    cl<-call("PPplot.moresample",x=x,y=y)
    if(is.null(extras$xlab)) cl$xlab=ifelse(is.na(Xind),XNAME,NULL)
    if(is.null(extras$ylab)) cl$ylab=ifelse(is.na(Yind),YNAME,NULL)
    if (length(extras) > 0) {
        existing <- !is.na(match(names(extras), names(cl)))
        for (a in names(extras)[existing]) cl[[a]] <- extras[[a]]
        if (any(!existing)) {
            cl <- c(as.list(cl), extras[!existing])
            cl <- as.call(cl)
        }
    }
    eval(cl)
  } else {
    x.vector<-x
    DNAME1<-ifelse(is.null(name),deparse(substitute(x)),name)
  }
  
  if(onesample) {
  	PPplot.onesample(x=x.vector,name=DNAME1,...)
  }
}

