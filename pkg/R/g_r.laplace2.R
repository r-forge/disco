g_r.laplace2 <-
function(r,z,b,Eh.Da,Eh.Db) {
  h<-orth.poly(r,"laplace",c(0,1))
  tmp<-h(z)+b.a.laplace2(z,b)*Eh.Da+b.b.laplace2(z,b)*Eh.Db
  return(tmp)
}

