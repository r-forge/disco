CvM.test <-
function(x1,x2) {
  z<-sort(c(x1,x2))	
  n1<-length(x1)
  n2<-length(x2)
  n<-n1+n2
  A<-0
  for(i in 1:(n-1)) {
  	Mi<-sum(x1<=z[i])
  	A<-A+(n*Mi-n1*i)^2
  }
  A<-A/(n1*n2*n^2)
  invisible(A)
}

