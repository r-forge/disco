Boxplot <-
function(x,side="",...) {
  # x: data object (vector)
  # side = "points" this specifies what has to be plotted at the right hand side
  # 	 = "gitter"
  #      = "bars"
  boxplot(x,...)
  switch(side,
  			points=points(rep(1.35,length(x)),x),
  			jitter=points(1.35+runif(length(x),min=-0.075,max=0.075),x),
  			bar=bars(x))
}

