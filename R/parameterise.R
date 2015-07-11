##' This function compute the parameters of a distribution given the
##' distribution, observed value and the self information.
##'
##' @param obsValue The observed value
##' @param selfInformation The self information of the value, which is
##' defined as the negative log of the probability of the observed value.
##' @param distribution The distribution to be parameterised.
##'
##' @return The parameter asssociated with the specified distribution.
##'
##' @export

parameterise = function(obsValue, selfInformation, distribution){
    switch(distribution,
           `normal` = {
               mean = obsValue               
               sd = sqrt(exp(2 * selfInformation)/(2 * pi * exp(1)))
               list(mean = mean, sd = sd)
           },
           `cauchy` = {
               location = obsValue
               scale = exp(selfInformation - log(4 * pi))
               list(location = location, scale = scale)
           },
           `truncNorm` = {
               ## The truncated normal here is defined as having lower
               ## bound of zero and upper bound of infinity. That is,
               ## on the positive real line.
               mean = obsValue
               sd = uniroot(
                   function(x){
                       entropyTruncNormal(a = 0, sd = x, mean = obsValue) -
                           selfInformation
                   },
                   interval = c(1e-20, 1e20))$root
               list(mean = mean, sd = sd)
           },
           `logNorm` = {
               sdlog = uniroot(
                   function(x){
                       ## The mean has to be computed first, as the
                       ## observed value assumed to be the mode of the
                       ## distribution.
                       mean = log(obsValue) + x^2
                       entropyLogNorm(mean = mean, sd = x) -
                           selfInformation
                   },
                   interval = c(1e-50, 1e20))$root
               meanlog = log(obsValue) + sdlog^2
               list(meanlog = meanlog, sdlog = sdlog)
           },
           `exponential` = {
               rate = exp(-(selfInformation - 1))
               warning("Exponential distribution has mode at zero")
               list(rate = rate)
           }
           )
}       