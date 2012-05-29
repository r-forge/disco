create.model <-
function(k) {
  models<-list()
  for (mod.length in 1:k) {
	mod.extra<-combn(k,mod.length,simplify=F)
	for (i in 1:choose(k,mod.length)) {		#or 1:length(mod.extra)
		models[[length(models)+1]]<-mod.extra[[i]]
	}
  }
  return(models)
}

