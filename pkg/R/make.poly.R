make.poly <-
function (f, k, moments = NA, coef = F, norm = T) 
{
    if (coef) {
        r <- make.coeff(f, k, moments, norm)
    }
    else {
        r <- make.polynomial(f, k, moments)
    }
    invisible(r)
}

