var.unif2 <-
function(r,z,sigma) {
  n<-length(z)
  Eh.Da<-switch(r-2,
	mean(h3.Da.unif2(z,sigma)),
	mean(h4.Da.unif2(z,sigma)),
	mean(h5.Da.unif2(z,sigma)),
	mean(h6.Da.unif2(z,sigma)),
	mean(h7.Da.unif2(z,sigma)),
	mean(h8.Da.unif2(z,sigma)))
  Eh.Db<-switch(r-2,
	mean(h3.Db.unif2(z,sigma)),
	mean(h4.Db.unif2(z,sigma)),
	mean(h5.Db.unif2(z,sigma)),
	mean(h6.Db.unif2(z,sigma)),
	mean(h7.Db.unif2(z,sigma)),
	mean(h8.Db.unif2(z,sigma)))
  v<-mean((g_r.unif2(r,z,sigma,Eh.Da,Eh.Db))^2)-(mean(g_r.unif2(r,z,sigma,Eh.Da,Eh.Db)))^2
  v<-v*n/(n-1)
  return(v)
}

