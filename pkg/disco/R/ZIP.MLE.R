ZIP.MLE <-
function(sample) {
    x.bar<-mean(sample)
    n<-length(sample)
    n0<-length(sample[sample==0])
    tmp<-optim(c(n0/n,x.bar),ZIP.optfn,x.bar=x.bar,n0=n0,n=n)
    pars<-tmp$par
    # names(pars)<-c("p","lambda")
    pars<-pars[2:1]
    return(pars)
    # returns c(lambda,p)
}

