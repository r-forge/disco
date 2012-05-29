EDF.test.ksample <-
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
	if(is.null(B)) {
			METHOD<-paste(METHOD,"(asymptotic approximation)")
	}
	if(!is.null(B)) {
			METHOD<-paste(METHOD,"(based on",B,"random permutations)")
	}
	name<-paste(dname," with response: ",formula[2]," and grouping variable: ",formula[3])
	RVAL<-list(statistic=c(t.obs), p.value=p.value,
        method=METHOD,
        data.name=name,parameter=PARAMETER)
    class(RVAL)<-"htest"
    return(RVAL)  
}

