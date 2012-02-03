g_r.logis2 <-
function(r,z,sigma,Eh.Dmu,Eh.Dsigma) {
  h<-orth.poly(r,"logis",c(0,1))
  tmp<-h(z)+b.mu.logis2(z,sigma)*Eh.Dmu+b.sigma.logis2(z,sigma)*Eh.Dsigma
  return(tmp)
}

