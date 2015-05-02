##' Function to convert information weight to flag.
##'
##' The function converts a numeric information weight back to a flag.
##'
##' @param flagObservationWeight The information weight of an observation.
##' @param flagTable The table mapping flags and weights.
##'
##' @export
##' 

weight2flag = function(flagObservationWeight,
                       flagTable = faoswsFlagTable){
    index = match(flagObservationWeight, flagTable$flagObservationWeights)
    as.character(flagTable$flagObservationStatus[index])
}
