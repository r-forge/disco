EDF.test.2sample <-
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
	if(is.null(B)) {
			METHOD<-paste(METHOD,"(asymptotic approximation)")
	}
	if(!is.null(B)) {
			METHOD<-paste(METHOD,"(based on",B,"random permutations)")
	}
	name<-paste("x:",xname," y:",yname)
	RVAL<-list(statistic=c(t.obs), p.value=p.value,
        method=METHOD,
        data.name=name)
    class(RVAL)<-"htest"
    return(RVAL)  
}

