##' Computes the flag weights based on the Kullback Leibler Divergence.
##'
##' @param x The vector of new representation (q)
##' @param benchmark The assumed true representation (p)
##'
##' @export


computeEntropyWeights = function(x, benchmark){
    n = length(x)
    k = n/10
    alpha = range(c(x, benchmark))
    x.density =
        hist(x, breaks = seq(alpha[1], alpha[2], length = k))$density
    benchmark.density =
        hist(benchmark, breaks = seq(alpha[1], alpha[2], length = k))$density    
    weights = 1/(1 + KL.empirical(benchmark.density, x.density))
    if(weights == 1)
        weights = 1 - 1e-5

    weights
}
