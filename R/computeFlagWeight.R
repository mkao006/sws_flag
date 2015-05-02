##' Function to compute the weights of each flag based on the history
##'
##' @param history The history of data queried from the api
##' @param officialFlag The character string representing official
##' figure.
##' @param method The method to be used to calculate the weights.
##'
##' @export
##' 


computeFlagWeight = function(history, officialFlag = "",
    method = "entropy"){

    ## Remove flags for missing values.
    removeMhistory = history[flagObservationStatus != "M", ]

    ## Calculate the number of history for each entry, only entries
    ## with more than two history can be used for the calculation of
    ## weights.    
    removeMhistory[, `:=`(numberOfHistory,
                          length(unique(flagObservationStatus))), 
                   by = c("geographicAreaM49", "measuredElement",
                       "measuredItemCPC",  "timePointYears")]

    ## Average the history if there is more than one symble for each
    ## history. The MSE would be the same, but the entropy will
    ## decrease. This is a temporary solution.    
    removeMhistory[, `:=`(Value, mean(Value)),
                   by = c("geographicAreaM49",  "measuredElement",
                       "measuredItemCPC", "timePointYears", 
                       "flagObservationStatus")]

    ## Subset the data which has more than 2 history.
    finalHistory.dt =
        unique(removeMhistory[numberOfHistory >=  2,
                              list(geographicAreaM49, measuredElement,
                                   measuredItemCPC, timePointYears,
                                   Value, flagObservationStatus)])

    ## Change the name of the history    
    finalHistory.dt[, `:=`(flagObservationStatus,
                           paste0("Flag_", flagObservationStatus))]

    ## Cast the data
    castedHistory =
        data.table(dcast(finalHistory.dt[, list(geographicAreaM49, 
        measuredElement, measuredItemCPC, timePointYears, Value, 
        flagObservationStatus)], geographicAreaM49 + measuredElement + 
        measuredItemCPC + timePointYears ~ flagObservationStatus, 
        value.var = "Value"))
    cat("Number of entries for intersect history: ",
        NROW(castedHistory),  "\n")
    
    symbNames = colnames(castedHistory)[grepl("Flag_",
        colnames(castedHistory))]
    official = paste0("Flag_", officialFlag)

    ## Compute the weights
    finalWeights = apply(data.matrix(castedHistory[, 
        symbNames[symbNames != official], with = FALSE]), 
        2, FUN = computeEntropyWeights,
        benchmark = unlist(castedHistory[, 
            official, with = FALSE]))
    finalWeights = sort(finalWeights, decreasing = TRUE)

    ## Construct the weight table
    weightTable =
        data.frame(flagObservationStatus = c(officialFlag, 
                       gsub("Flag_", "", names(finalWeights)), "M"),
                   flagObservationWeights = c(1, finalWeights, 0),
                   row.names = NULL)
    weightTable
}
