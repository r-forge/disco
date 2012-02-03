rZTP <-
function(n,lambda) {
 qpois(runif(n, dpois(0, lambda), 1), lambda) 
}

