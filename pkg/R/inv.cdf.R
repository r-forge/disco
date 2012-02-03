inv.cdf <-
function(cdf,p,x.range=c(-10,10,0.01)) {
  cdf.range<-cdf(x.range)
  inv<-min(x.range[cdf.range>=p])
  invisible(inv)
}

