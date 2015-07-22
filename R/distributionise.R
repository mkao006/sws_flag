##' This function converts the calculated parameter from the function
##' "paramerise" and create the distribution.
##'
##' @param obsValue The observed value
##' @param selfInformation The self information of the value, which is
##' defined as the negative log of the probability of the observed value.
##' @param distribution The distribution to be parameterised.
##'
##' @return A list containing the distribution function and the
##' parameters associated with the distribution
##'
##' @export

distributionise = function(obsValue, selfInformation, distribution){
    parameters = parameterise(obsValue = obsValue,
        selfInformation = selfInformation, distribution = distribution)
    switch(distribution,
           `normal` = {
               list(pdf = with(parameters,
                        function(x) dnorm(x, mean = mean, sd = sd)),
                    parameters = parameters)
           },
           `cauchy` = {
               list(pdf = with(parameters,
                        function(x) dcauchy(x, location = location,
                                            scale = scale)),
                    parameters = parameters)
           },
           `truncNorm` = {
               list(pdf = with(parameters,
                        function(x) dtruncnorm(x, a = 0, b = Inf, mean = mean,
                                               sd = sd)),
                    parameters = parameters)
           },
           `logNorm` = {
               list(pdf = with(parameters,
                        function(x) dlnorm(x, meanlog = meanlog, sdlog = sdlog)),
                    parameters = parameters)
           },
           `exponential` = {
               list(pdf = with(parameters, function(x) dexp(x, rate = rate)),
                    parameters = parameters)
           },
           `weibull` = {
               list(pdf = with(parameters,
                        function(x) dweibull(x, shape = shape, scale = scale)),
                    parameters = parameters)
           }
           )
}
