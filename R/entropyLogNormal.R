##' The function returns the entropy of a truncated normal
##' distribution.
##'
##' @param mean mean of the distributinon.
##' @param sd The standard deviation
##'
##' @return The entropy
##'
##' @export

entropyLogNorm = function(mean, sd){
    ## mean + log(sqrt(2 * pi * exp(1)) * sd)
    1/2 + 1/2 * log(2 * pi * sd^2) + mean
}
