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
    mean + log(sqrt(2 * pi * exp(1)) * sd)
}
