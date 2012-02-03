prepare.poly <-
function (f, k) 
# computes moments
{
    mu <- rep(0, 2 * k )
    for (i in 1:(2 * k )) {
        g <- function(x) {
            y <- (x)^i * f(x)   # - mu[1] removed
            return(y)
        }
        templist <- integrate(g, -Inf, Inf, subdivisions=1000)
        mu[i] <- templist$value
    }
    return(mu)
}

