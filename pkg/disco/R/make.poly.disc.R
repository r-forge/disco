make.poly.disc <-
function (f, k, moments = NA, coef = F, norm = T) 
{
    if (coef) {
        r <- make.coeff.disc(f, k, moments, norm)
    }
    else {
        r <- make.polynomial.disc(f, k, moments)
    }
    invisible(r)
}

