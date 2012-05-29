var.laplace2 <-
function(r,z,b) {
  n<-length(z)
  Eh.Da<-switch(r-2,
	mean(h3.Da.laplace2(z,b)),
	mean(h4.Da.laplace2(z,b)),
	mean(h5.Da.laplace2(z,b)),
	mean(h6.Da.laplace2(z,b)),
	mean(h7.Da.laplace2(z,b)),
	mean(h8.Da.laplace2(z,b)))
  Eh.Db<-switch(r-2,
	mean(h3.Db.laplace2(z,b)),
	mean(h4.Db.laplace2(z,b)),
	mean(h5.Db.laplace2(z,b)),
	mean(h6.Db.laplace2(z,b)),
	mean(h7.Db.laplace2(z,b)),
	mean(h8.Db.laplace2(z,b)))
  v<-mean((g_r.laplace2(r,z,b,Eh.Da,Eh.Db))^2)-(mean(g_r.laplace2(r,z,b,Eh.Da,Eh.Db)))^2
  v<-v*n/(n-1)
  return(v)
}

