extrval.covarMLE <-
function() { # independent of nuisance parameters
  	S_t1b<-matrix(c(sqrt(6)/pi,sqrt(6)/pi*gam,
                  -12*zeta(3)/pi^2/sqrt(11/90*pi^4-24*zeta(3)^2/pi^2),(pi^2/3-12*zeta(3)*gam/pi^2)/sqrt(11/90*pi^4-24*zeta(3)^2/pi^2)),nrow=2,byrow=TRUE)
  	S_bb<-matrix(c(1,-1+gam,
                 -1+gam,1+1/6*pi^2-2*gam+gam^2),nrow=2,byrow=TRUE)
  	S_t2b<-matrix(c((0.1060499473*3*pi^2/6-0.219420091),(0.1060499473*3*(2*zeta(3)+gam*pi^2/6)-0.4944037009*2*pi^2/6-0.219420091*gam), 
                  (0.02493263979*4*(2*zeta(3))-0.2416834756*3*(pi^2/6)+0.7769092062),(0.02493263979*4*(3*pi^4/20+gam*2*zeta(3))-0.2416834756*3*(2*zeta(3)+gam*pi^2/6)+0.269077145*2*(pi^2/6)+0.7769092062*gam)),nrow=2,byrow=TRUE)
	S_hb<-rbind(S_t1b,S_t2b)
	C<-diag(rep(1,4))- S_hb %*% solve(S_bb) %*% t(S_hb)
	# a little less precise: 
	#C<-matrix(c(.2249753730e-1,.9421018314e-1,.2740580591e-1,-.6344435072e-1,
      #            .9421018314e-1,.5032986969,.3673292342,-.2512184570,
      #            .2740580591e-1,.3673292342,.7185257457,.1970238182,
      #            -.6344435072e-1,-.2512184574,.1970238172,.8600794224),ncol=4,byrow=TRUE)
	# a little more precise: 
	#C[1,1]<-(pi^4-36-6*pi^2)/pi^4
	#C[1,2]<--12*5^(1/2)*3^(1/2)*(-36*zeta(3)-6*zeta(3)*pi^2+pi^4)/pi^4/(11*pi^6-2160*zeta(3)^2)^(1/2)
	#C[2,1]<-C[1,2]
	#C[2,2]<-(11*pi^10-2160*pi^4*zeta(3)^2-77760*zeta(3)^2-12960*zeta(3)^2*pi^2+4320*zeta(3)*pi^4-60*pi^8)/pi^4/(11*pi^6-2160*zeta(3)^2)
	return(C)
}

