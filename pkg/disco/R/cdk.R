###############################
# CHANGE LOG
###############################
# 2012/05/09 : Added var='jn' as argument to make function work again. - JM
#              corrected switch statement - JM
#              added st=FALSE as argument to avoid error in smooth.test

cdk <-
function( x,g,order=4,basis="Lg",B=NULL,criterion="AIC",
          horizon="order",stat.only=F,
          var="jn",st=FALSE) {
	# var="jn",st=F : these are extra arguments for when empirical standardisation gets implemented
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
		fn<-switch(basis,Lg=Lg(i),Four=Fourier(i))
		ind<-1
		for(s in 1:k) {
  		  comp[s,i]<-sqrt(ns[s])*mean(fn(r[ind:(ns[s]+ind-1)]))
  		  cmp[s,i]<-comp[s,i]*sqrt(n/ns[s]) # where does this factor come from?
  		  ind<-ind+ns[s]
  		}
#  		if(st) {
#			v[i]<-switch(var,
#				jn=var.jn(x1,x2,fn),
#				emp=ifelse(i<=4,tmp[i],NA))
#			cmp[,i]<-
#		}
	}
	stat<-sum(cmp^2)
#	if(!st) return(list(stat=stat,comp=comp,var=v,statst=comp/sqrt(v)))
#	if(st) return(comp/sqrt(v))
    comps2<-colMeans(cmp^2)
    stat<-sum(comps2)

   	# adaptive : only order implemented
   	if(!is.null(criterion)) {
  	  penalty=switch(criterion,
	  	AIC=2*(k-1),
		BIC=(k-1)*log(n))
	  crit<-cumsum(comps2-penalty*(1:order)) 
	  order.select<-1:((1:order)[crit==max(crit)])
	  stat<-cumsum(comps2)[crit==max(crit)]
	}

    
    if(is.null(B)) {
    	p<-1-pchisq(comps2,df=k-1)
    	p.stat<-1-pchisq(stat,df=order*(k-1)) 
    	PMETHOD<-"the asymptotic approximation"
    }
    if(!is.null(B)) {
    	PMETHOD<-"the approximate permutation null distribution"
    	nd<-matrix(nrow=(order+1),ncol=B)
    	for(i in 1:B) {
    		x.B<-sample(x)
    		tmp<-cdk(x=x.B,g=g,order=order,basis=basis,var=var,st=st,B=NULL,criterion=criterion,horizon=horizon,stat.only=T)
    		nd[,i]<-c(tmp$stat,tmp$comp)
    	}
    	p.stat<-mean(stat<=nd[1,],na.rm=T)
    	p<-sapply(2:(order+1),function(i,d,e) {mean(d[i-1]<=e[i,],na.rm=T)},d=comps2,e=nd)
    }
    prob2<-NA
    prob3<-NA
    if(!stat.only) {
    	if(k==2) {
	  		prob2<-pr12(x1,x2)
	  		prob3<-diagnose(x1,x2)	
		}
	}
	
    tmp<-list(coefficients=comp,components=comps2,statistic=stat,p.values=c(p,p.stat),EVar=NA,order=order,basis=basis,horizon=horizon,criterion=criterion,method.var=NA,method.p.value=PMETHOD,K=k,order.selected=order.select,min.order=1,diagnose=list(pr12=prob2,order2=prob3))
    class(tmp)<-"cdk"
    
    
    tmp
}

