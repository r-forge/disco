var.extrval2 <-
function(r,z,b) {
  n<-length(z)
  Eh.Da<-switch(r-2,
	mean(h3.Da.extrval2(z,b)),
	mean(h4.Da.extrval2(z,b)),
	mean(h5.Da.extrval2(z,b)),
	mean(h6.Da.extrval2(z,b)),
	mean(h7.Da.extrval2(z,b)),
	mean(h8.Da.extrval2(z,b)))
  Eh.Db<-switch(r-2,
	mean(h3.Db.extrval2(z,b)),
	mean(h4.Db.extrval2(z,b)),
	mean(h5.Db.extrval2(z,b)),
	mean(h6.Db.extrval2(z,b)),
	mean(h7.Db.extrval2(z,b)),
	mean(h8.Db.extrval2(z,b)))
  v<-mean((g_r.extrval2(r,z,b,Eh.Da,Eh.Db))^2)-(mean(g_r.extrval2(r,z,b,Eh.Da,Eh.Db)))^2
  v<-v*n/(n-1)
  return(v)
}

