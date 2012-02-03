var.logis2 <-
function(r,z,sigma) {
  n<-length(z)
  Eh.Dmu<-switch(r-2,
	mean(h3.Dmu.logis2(z,sigma)),
	mean(h4.Dmu.logis2(z,sigma)),
	mean(h5.Dmu.logis2(z,sigma)),
	mean(h6.Dmu.logis2(z,sigma)),
	mean(h7.Dmu.logis2(z,sigma)),
	mean(h8.Dmu.logis2(z,sigma)))
  Eh.Dsigma<-switch(r-2,
	mean(h3.Dsigma.logis2(z,sigma)),
	mean(h4.Dsigma.logis2(z,sigma)),
	mean(h5.Dsigma.logis2(z,sigma)),
	mean(h6.Dsigma.logis2(z,sigma)),
	mean(h7.Dsigma.logis2(z,sigma)),
	mean(h8.Dsigma.logis2(z,sigma)))
  v<-mean((g_r.logis2(r,z,sigma,Eh.Dmu,Eh.Dsigma))^2)-(mean(g_r.logis2(r,z,sigma,Eh.Dmu,Eh.Dsigma)))^2
  v<-v*n/(n-1)
  return(v)
}

