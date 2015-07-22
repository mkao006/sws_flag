##' The function returns the entropy of a weibull distribution.
##'
##' @param shape The shape parameter of the distribution, usually denoted k
##' @param scale The scale parameter of the distribution, usually
##' denoted lambda
##'
##' @return The entropy
##'
##' @export


entropyWeibull = function(shape, scale){
    -digamma(1) * (1 - 1/shape) + log(scale/shape) + 1
}
