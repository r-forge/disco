# Groups is defunct, use xlev from model.frame instead. 

PPplot.formula <- function(formula, ...)
{
    # check formula
    if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]), 
        "term.labels")) != 1L)) 
        stop("'formula' missing or incorrect")


    # Create the model frame
    mc <- match.call(expand.dots=TRUE)
    m <- match(c("formula", "data", "subset", "weights", "na.action", 
        "offset","xlev"), names(mc), 0L)
    mf <- mc[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    
    # Make the list
    fact <- match(attr(terms(mf),'term.labels'),names(mf))
    x <- mf[,-fact]
    y <- factor(mf[,fact])
    x <- split(x,y)
    
    # Select the plot arguments    
    pargs <- as.list(mc)[-c(1L,m)]
    pargs <- c(pargs,list(x=x))
    
    # Make a main if necessary
    i.main <- 'main' %in% names(mc)
    if(!i.main){
      main <- paste(as.character(formula)[c(2,1,3)],collapse=' ')
      main <- paste('PP plot for',main)
      pargs <- c(pargs,list(main=main))
    }
    do.call(PPplot, pargs)
        
}

