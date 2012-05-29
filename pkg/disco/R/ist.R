ist <-
function(i) {
	a<-switch(i,"st","nd","rd")
	if(is.null(a)) a<-"th"
	return(a)
}

