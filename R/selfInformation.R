##' This function computes the self-information of an observed value
##' provided the probability of the event.
##'
##' @param prob A vector representing the probability of each event.
##'
##' @return Vector of the same length as the input representing the
##' self information of the event.
##'
##' @export

selfInformation = function(prob){
    -log(prob)
}
