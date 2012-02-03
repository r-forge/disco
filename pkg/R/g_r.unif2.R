g_r.unif2 <-
function(r,z,sigma,Eh.Da,Eh.Db) {
  h<-orth.poly(r,"unif",c(0,1))
  tmp<-h(z)+b.a.unif2(z,sigma)*Eh.Da+b.b.unif2(z,sigma)*Eh.Db
  return(tmp)
}

