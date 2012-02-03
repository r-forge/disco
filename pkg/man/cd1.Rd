\name{cd1}
\Rdversion{1.1}
\alias{cd1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cd1(sample, order, distr = "unif", method = "NONE", pars = c(0, 1), B = 200, rescale = FALSE, f = NA, moments = NA, typedistr = "cont", chol = FALSE, output = TRUE, ntrials = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sample}{
%%     ~~Describe \code{sample} here~~
}
  \item{order}{
%%     ~~Describe \code{order} here~~
}
  \item{distr}{
%%     ~~Describe \code{distr} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{pars}{
%%     ~~Describe \code{pars} here~~
}
  \item{B}{
%%     ~~Describe \code{B} here~~
}
  \item{rescale}{
%%     ~~Describe \code{rescale} here~~
}
  \item{f}{
%%     ~~Describe \code{f} here~~
}
  \item{moments}{
%%     ~~Describe \code{moments} here~~
}
  \item{typedistr}{
%%     ~~Describe \code{typedistr} here~~
}
  \item{chol}{
%%     ~~Describe \code{chol} here~~
}
  \item{output}{
%%     ~~Describe \code{output} here~~
}
  \item{ntrials}{
%%     ~~Describe \code{ntrials} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (sample, order, distr = "unif", method = "NONE", pars = c(0, 
    1), B = 200, rescale = FALSE, f = NA, moments = NA, typedistr = "cont", 
    chol = FALSE, output = TRUE, ntrials = NA) 
{
    if (rescale && chol) 
        stop("Empirical rescaling and the Cholesky decomposition cannot be combined.")
    if (!is.numeric(sample)) 
        stop("The given i.i.d. sample has to be a numeric vector.")
    if (((trunc(order) - order) != 0) || (order < 1)) 
        stop("The order of the smooth test has to be a strict positive integer.")
    if (!(method == "MLE" || method == "MME" || method == "NONE")) 
        stop("The nuisance parameter estimation method is not 1 of 3 given possibilities.")
    degree <- order
    par_est <- NA
    p_val <- NA
    sampleN <- length(sample)
    n <- sampleN
    if (distr == "norm" || distr == "logis" || distr == "ZIP" || 
        distr == "negb" || distr == "laplace" || distr == "extrval" || 
        distr == "exp" || distr == "logar" || distr == "pois" || 
        distr == "geom" || distr == "ZTP" || distr == "unif" || 
        distr == "genpareto" || distr == "betab" || distr == 
        "gamma") {
        if (rescale && method == "MLE" && !(distr == "norm" || 
            distr == "exp" || distr == "logar" || distr == "pois" || 
            distr == "geom" || distr == "ZTP")) 
            stop("Empirical rescaling together with MLE is not implemented for this distribution.")
        if (chol && method == "MLE" && !(distr == "norm" || distr == 
            "exp" || distr == "logar" || distr == "pois" || distr == 
            "geom" || distr == "ZTP")) 
            stop("Cholesky decomposition together with MLE is not implemented (for this distribution).")
        if (rescale && method == "MME" && (distr == "ZIP" || 
            distr == "negb" || distr == "genpareto" || distr == 
            "betab" || distr == "gamma")) 
            stop("Empirical rescaling (together with MME) is not implemented for this distribution.")
        if (rescale && method == "MME" && (distr == "logis" || 
            distr == "laplace" || distr == "extrval" || distr == 
            "unif") && (order > 8)) 
            stop("Empirical rescaling together with MME is only implemented until the 8th order for this distribution.")
        if (distr == "betab" && is.na(ntrials)) 
            stop("The number of trials has to be given through \"ntrials\".")
        if (distr == "genpareto" && method != "NONE") {
            warning("The smooth test for the generalized Pareto distribution could give an error. \n\t\t\t\t\t\t If this is the case, use of the alternative estimation method is suggested.")
        }
        if (method == "MLE") {
            min.order <- switch(distr, norm = 3, exp = 2, logar = 2, 
                pois = 2, geom = 2, ZTP = 2, logis = 1, negb = 2, 
                laplace = 1, ZIP = 2, extrval = 1, unif = 1, 
                genpareto = 1, betab = 1, gamma = 2)
            par_est <- switch(distr, norm = norm.MLE(sample), 
                exp = exp.MLE(sample), logar = logar.MLE(sample), 
                pois = pois.MLE(sample), geom = geom.MLE(sample), 
                ZTP = ZTP.MLE(sample), logis = logis.MLE(sample), 
                negb = negb.MLE(sample), laplace = laplace.MLE(sample), 
                ZIP = ZIP.MLE(sample), extrval = extrval.MLE(sample), 
                unif = unif.MLE(sample), genpareto = genpareto.MLE(sample), 
                betab = betab.MLE(sample, ntrials), gamma = gamma.MLE(sample))
            names(par_est) <- switch(distr, norm = c("mean", 
                "sd"), exp = "rate", logar = "prob", pois = "lambda", 
                geom = "prob", ZTP = "lambda", logis = c("location", 
                  "scale"), negb = c("size", "prob"), laplace = c("location", 
                  "scale"), ZIP = c("lambda", "phi"), extrval = c("location", 
                  "scale"), unif = c("min", "max"), genpareto = c("scale", 
                  "-shape"), betab = c("shape1", "shape2"), gamma = c("shape", 
                  "scale"))
            pars <- par_est
            if (is.na(pars[1]) || is.nan(pars[1]) || is.infinite(pars[1])) 
                stop("The nuisance parameter(s) estimation failed.")
            if (length(pars) == 2) {
                if (is.na(pars[2]) || is.nan(pars[2]) || is.infinite(pars[2])) 
                  stop("The nuisance parameter(s) estimation failed.")
            }
        }
        else if (method == "MME") {
            if (distr == "gamma") {
                warning("The MME based smooth test for the gamma distribution is unreliable. \n\t\t\t\t\t\t Therefore the MLE based smooth test is recommended.")
            }
            min.order <- switch(distr, norm = 3, exp = 2, logar = 2, 
                pois = 2, geom = 2, ZTP = 2, logis = 3, negb = 3, 
                laplace = 3, ZIP = 3, extrval = 3, unif = 3, 
                genpareto = 3, betab = 3, gamma = 3)
            par_est <- switch(distr, norm = norm.MLE(sample), 
                exp = exp.MLE(sample), logar = logar.MLE(sample), 
                pois = pois.MLE(sample), geom = geom.MLE(sample), 
                ZTP = ZTP.MLE(sample), logis = logis.MME(sample), 
                negb = negb.MME(sample), laplace = laplace.MME(sample), 
                ZIP = ZIP.MME(sample), extrval = extrval.MME(sample), 
                unif = unif.MME(sample), genpareto = genpareto.MME(sample), 
                betab = betab.MME(sample, ntrials), gamma = gamma.MME(sample))
            names(par_est) <- switch(distr, norm = c("mean", 
                "sd"), exp = "rate", logar = "prob", pois = "lambda", 
                geom = "prob", ZTP = "lambda", logis = c("location", 
                  "scale"), negb = c("size", "prob"), laplace = c("location", 
                  "scale"), ZIP = c("lambda", "phi"), extrval = c("location", 
                  "scale"), unif = c("min", "max"), genpareto = c("scale", 
                  "-shape"), betab = c("shape1", "shape2"), gamma = c("shape", 
                  "scale"))
            pars <- par_est
            if (is.na(pars[1]) || is.nan(pars[1]) || is.infinite(pars[1])) 
                stop("The nuisance parameter(s) estimation failed.")
            if (length(pars) == 2) {
                if (is.na(pars[2]) || is.nan(pars[2]) || is.infinite(pars[2])) 
                  stop("The nuisance parameter(s) estimation failed.")
            }
        }
        else if (method == "NONE") {
            min.order <- 1
            if (!is.numeric(pars)) 
                stop("The nuisance parameter(s) have to be given properly,\n\t\t\t\t\t\tsince no estimation method is chosen.")
            if (is.na(pars[1]) || is.nan(pars[1]) || is.infinite(pars[1])) 
                stop("The nuisance parameter(s) have to be given properly,\n\t\t\t\t\t\t\t\t\t\t\t\tsince no estimation method is chosen.")
            if (length(pars) == 2) {
                if (is.na(pars[2]) || is.nan(pars[2]) || is.infinite(pars[2])) 
                  stop("The nuisance parameter(s) have to be given properly,\n\t\t\t\t\t\t\t\t\t\t\t\tsince no estimation method is chosen.")
            }
        }
        if (order < min.order) 
            stop("The order is too small to perform a meaningfull \n\t\tsmooth test for this null distribution.")
        p_val <- rep(0, degree - min.order + 1 + 1)
        STAll <- test.stat(degree, distr, pars, sample, method, 
            f, moments, typedistr, chol, ntrials)
        ST <- STAll$Tstat
        ind.comp <- 1:(length(ST) - 1)
        if (rescale) 
            ST[ind.comp] <- ST[ind.comp] * (sqrt(diag(STAll$Sigma)[ind.comp]))/sqrt(STAll$EVar)
        if (is.null(B)) {
            p_val <- pchisq(c((ST[ind.comp])^2, ST[degree - min.order + 
                2]), df = c(rep(1, degree - min.order + 1), degree - 
                min.order + 1), lower.tail = FALSE)
        }
        else {
            T <- matrix(0, degree - min.order + 1 + 1, B)
            error.columns <- NULL
            for (j in 1:B) {
                x <- switch(distr, logis = rlogis(n, location = pars[1], 
                  scale = pars[2]), negb = rnbinom(n, size = pars[1], 
                  prob = pars[2]), laplace = rlaplace(n, location = pars[1], 
                  scale = pars[2]), ZIP = rzipois(n, lambda = pars[1], 
                  phi = pars[2]), extrval = rgumbel(n, location = pars[1], 
                  scale = pars[2]), norm = rnorm(n, mean = pars[1], 
                  sd = pars[2]), exp = rexp(n, rate = pars[1]), 
                  logar = rlog(n, prob = pars[1]), pois = rpois(n, 
                    lambda = pars[1]), geom = rgeom(n, prob = pars[1]), 
                  ZTP = rZTP(n, lambda = pars[1]), unif = runif(n, 
                    min = pars[1], max = pars[2]), genpareto = rgpd(n, 
                    scale = pars[1], shape = -pars[2]), betab = rbetabin.ab(n, 
                    size = ntrials, shape1 = pars[1], shape2 = pars[2]), 
                  gamma = rgamma(n, shape = pars[1], scale = pars[2]))
                if (method == "NONE") {
                  STAll.B <- try(test.stat(degree, distr, pars, 
                    x, method, f, moments, typedistr, chol, ntrials), 
                    silent = TRUE)
                }
                else {
                  STAll.B <- try(test.stat(degree, distr, pars = NA, 
                    x, method, f, moments, typedistr, chol, ntrials), 
                    silent = TRUE)
                }
                if (!inherits(STAll.B, "try-error")) {
                  T[, j] <- STAll.B$Tstat
                }
                else {
                  error.columns <- c(error.columns, j)
                }
                if (rescale) 
                  T[ind.comp, j] <- T[ind.comp, j] * (sqrt(diag(STAll.B$Sigma)[ind.comp]))/sqrt(STAll.B$EVar)
            }
            if (!is.null(error.columns)) {
                T <- T[, -error.columns]
                warning(length(error.columns), " bootstrap samples had to be ignored, \n\t\t\t\t since their test statistic was undefined.\n")
            }
            for (i in 1:(degree - min.order + 1)) {
                tmp <- mean(T[i, ] >= ST[i])
                p_val[i] <- 2 * min(tmp, 1 - tmp)
            }
            p_val[degree - min.order + 2] <- mean(T[degree - 
                min.order + 2, ] >= ST[degree - min.order + 2])
        }
    }
    else {
        if (method != "NONE") 
            stop("The smooth test for testing a distribution absent from the given list,   \n\t\t\t\t\t   can only be performed without nuisance parameter estimation.")
        if (is.na(moments[1])) {
            if (!is.function(f)) 
                stop("The null distribution is not (correctly) specified.")
            if (!(typedistr == "cont" || typedistr == "disc")) 
                stop("The type of the null distribution \"f\" is not correctly specified.")
        }
        else {
            if (!is.numeric(moments) || (length(moments) != 2 * 
                degree)) 
                stop("The null distribution is not (correctly) specified: \n\t\t\t\t\t\t\t\t\t\t\t\tbad input for argument \"moments\".")
            if (!is.null(B)) 
                stop("Bootstrapping is not possible when the null distribution is given by \"moments\". \n\t\t\t\t\t\tChoose \"B=NULL\" when this argument is used. ")
        }
        p_val <- rep(0, degree + 1)
        STAll <- test.stat(degree, distr = "otherwise", pars, 
            sample, method, f, moments, typedistr, chol, ntrials)
        ST <- STAll$Tstat
        ind.comp <- 1:(length(ST) - 1)
        if (rescale) 
            ST[ind.comp] <- ST[ind.comp] * (sqrt(diag(STAll$Sigma)[ind.comp]))/sqrt(STAll$EVar)
        if (is.null(B)) {
            p_val <- pchisq(c((ST[ind.comp])^2, ST[degree + 1]), 
                df = c(rep(1, degree), degree), lower.tail = FALSE)
        }
        else {
            T <- matrix(0, degree + 1, B)
            error.columns <- NULL
            for (j in 1:B) {
                x <- switch(typedistr, cont = rsamplecont(sampleN, 
                  f), disc = rsampledisc(sampleN, f))
                STAll.B <- try(test.stat(degree, distr = "otherwise", 
                  pars, x, method, f, moments, typedistr, chol, 
                  ntrials), silent = TRUE)
                if (!inherits(STAll.B, "try-error")) {
                  T[, j] <- STAll.B$Tstat
                }
                else {
                  error.columns <- c(error.columns, j)
                }
                if (rescale) 
                  T[ind.comp, j] <- T[ind.comp, j] * (sqrt(diag(STAll.B$Sigma)[ind.comp]))/sqrt(STAll.B$EVar)
            }
            if (!is.null(error.columns)) {
                T <- T[, -error.columns]
                warning(length(error.columns), " bootstrap samples had to be ignored, \n\t\t\t\t since their test statistic was undefined.\n")
            }
            for (i in 1:degree) {
                tmp <- mean(T[i, ] >= ST[i])
                p_val[i] <- 2 * min(tmp, 1 - tmp)
            }
            p_val[degree + 1] <- mean(T[degree + 1, ] >= ST[degree + 
                1])
        }
    }
    if (is.na(p_val[1])) {
        stop("This smooth goodness-of-fit test can't be performed unexpectedly.\n\t\t  Check whether your input arguments are correct.")
    }
    else {
        statistics <- ST
        if (is.na(par_est[1])) 
            par_est <- "no parameter estimation needed"
        if (output) {
            q <- length(statistics)
            cat("Smooth goodness-of-fit test\n")
            cat("Null hypothesis:", distr, "against", degree, 
                "th order alternative\n")
            if (distr == "betab") {
                cat("The number of trials are assumed known and fixed as", 
                  ntrials, "\n")
            }
            cat("Nuisance parameter estimation:", method, "\n")
            if (method != "NONE") {
                cat("Parameter estimates:", par_est, " (", names(par_est), 
                  ")\n\n")
            }
            else {
                cat("\n")
            }
            cat("Smooth test statistic S_k =", statistics[q], 
                " p-value =", p_val[q], "\n")
            cnt <- 1
            if (!chol) {
                for (i in (degree - q + 2):degree) {
                  if (rescale) {
                    cat("    ", i, "th empirically rescaled component V_k =", 
                      statistics[cnt], " p-value =", p_val[cnt], 
                      "\n")
                  }
                  else {
                    cat("    ", i, "th theoretically rescaled component V_k =", 
                      statistics[cnt], " p-value =", p_val[cnt], 
                      "\n")
                  }
                  cnt <- cnt + 1
                }
            }
            else {
                for (i in (degree - q + 2):degree) {
                  cat("    ", (degree - q + 2), "to", i, "combined (th. rescaled) Cholesky component V_k =", 
                    statistics[cnt], " p-value =", p_val[cnt], 
                    "\n")
                  cnt <- cnt + 1
                }
            }
            cat("\n")
            if (is.null(B)) {
                cat("All p-values are obtained by the asymptotical chi-square approximation\n\n")
            }
            else {
                cat("All p-values are obtained by the bootstrap with", 
                  B, "runs\n\n")
            }
        }
        comp <- STAll$comp
        Sigma <- STAll$Sigma
        EVar <- STAll$EVar
    }
    if(is.null(B)) {
    	MPV<-"the asymptotic approximation"
    }
    if(!is.null(B)) {
    	MPV<-"the bootstrap"
    }
    invisible(list(statistics = statistics, p.value = p_val, 
        par.est = par_est, comp = comp, Sigma = Sigma, EVar = EVar, order=order, method.est=method,method.p.value=MPV,distr=distr,order.select=(min.order:order)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
