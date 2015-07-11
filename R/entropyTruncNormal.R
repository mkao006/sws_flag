##' The function returns the entropy of a truncated normal
##' distribution.
##'
##' @param a lower bound. These may be ‘-Inf’
##' @param b upper bound. These may be ‘Inf’
##' @param mean mean of the distributinon.
##' @param sd The standard deviation
##'
##' @return The entropy
##'
##' @export


entropyTruncNormal = function(a = -Inf, b = Inf, mean = 0, sd = 1){
    alpha = (a - mean)/sd
    beta = (b - mean)/sd
    z = pnorm(beta) - pnorm(alpha)
    alphaTrans = ifelse(a == -Inf, 0, alpha * dnorm(alpha))
    betaTrans = ifelse(b == Inf, 0, beta * dnorm(beta))    
    log(sqrt(2 * pi * exp(1)) * sd * z) + (alphaTrans - betaTrans)/(2 * z)
}
