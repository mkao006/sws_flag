##' Computes the flag weights based on the Kullback Leibler Divergence.
##'
##' @param x The vector of new representation (q)
##' @param benchmark The assumed true representation (p)
##'
##' @importFrom entropy KL.plugin
##'
##' @export


computeEntropyWeights = function (x, benchmark){
    missIndex = union(which(is.na(x)), which(is.na(benchmark)))
    x.nomiss = x[-missIndex]
    benchmark.nomiss = benchmark[-missIndex]
    n = length(x.nomiss)
    alpha = c(x.nomiss, benchmark.nomiss)
    k = round(log(n))
    myBreaks = c(range(alpha), alpha[1:length(alpha) %% k == 1])
    x.freq = hist(x.nomiss, breaks = myBreaks, plot = FALSE)$counts
    benchmark.freq =
        hist(benchmark.nomiss, breaks = myBreaks, plot = FALSE)$counts
    zeroIndex = which(x.freq == 0)
    weights = 1/(1 + entropy::KL.plugin(benchmark.freq[-zeroIndex],
        x.freq[-zeroIndex]))
    if (weights == 1) 
        weights = 1 - 1e-05
    weights
}
