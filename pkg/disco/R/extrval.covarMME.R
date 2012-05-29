extrval.covarMME <-
function() { # independent of nuisance parameters
	I <- diag(rep(1,2))
  	S_t1b <- matrix(c(sqrt(6)/pi,sqrt(6)/pi*gam,
                  -12*zeta(3)/pi^2/sqrt(11/90*pi^4-24*zeta(3)^2/pi^2),(pi^2/3-12*zeta(3)*gam/pi^2)/sqrt(11/90*pi^4-24*zeta(3)^2/pi^2)),nrow=2,byrow=TRUE)
   	S_t2b <- matrix(c((0.1060499473*3*pi^2/6-0.219420091),(0.1060499473*3*(2*zeta(3)+gam*pi^2/6)-0.4944037009*2*pi^2/6-0.219420091*gam), 
                  (0.02493263979*4*(2*zeta(3))-0.2416834756*3*(pi^2/6)+0.7769092062),(0.02493263979*4*(3*pi^4/20+gam*2*zeta(3))-0.2416834756*3*(2*zeta(3)+gam*pi^2/6)+0.269077145*2*(pi^2/6)+0.7769092062*gam)),nrow=2,byrow=TRUE)
      K <- S_t2b %*% solve(S_t1b)
      C <- I + K %*% t(K)
	return(C)
}

