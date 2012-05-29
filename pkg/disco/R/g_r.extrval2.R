g_r.extrval2 <-
function(r,z,b,Eh.Da,Eh.Db) {
  h<-orth.poly(r,"extrval",c(0,1))
  tmp<-h(z)+b.a.extrval2(z,b)*Eh.Da+b.b.extrval2(z,b)*Eh.Db
  return(tmp)
}

