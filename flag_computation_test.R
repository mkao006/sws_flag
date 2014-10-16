## load the library
library(faosws)
library(faoswsUtil)
library(faoswsFlag)
library(faoswsProductionImputation)
library(data.table)
library(FAOSTAT)
library(lattice)
library(reshape2)
library(entropy)
library(Amelia)

## Set up for the test environment
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "7bc2ab09-72b9-4e54-b79f-d9132a4e72fe"
        )
}

newPivot = c(
    Pivoting(code= "geographicAreaM49", ascending = TRUE),
    Pivoting(code= "measuredItemCPC", ascending = TRUE),
    Pivoting(code = "timePointYears", ascending = FALSE),
    Pivoting(code= "measuredElement", ascending = TRUE)
    )

newKey = swsContext.datasets

getAllCountryCode = function(){
    ## 1062 is geographical world
    keyTree =
        unique(GetCodeTree(domain = swsContext.datasets[[1]]@domain,
                           dataset = swsContext.datasets[[1]]@dataset,
                           dimension = "geographicAreaM49",
                           roots = "1062")
               )    
    allCountryCode =
        unique(adjacent2edge(keyTree)$children)
    allCountryCode[allCountryCode %in% FAOcountryProfile$UN_CODE]
}

## Create new key and download data
newKey[[1]]@dimensions$geographicAreaM49@keys = getAllCountryCode()
newKey[[1]]@dimensions$timePointYears@keys = as.character(1970:2013)

## Primary crops (T, , E)
newKey[[1]]@dimensions$measuredItemCPC@keys = "0111"
newKey[[1]]@dimensions$measuredItemCPC@keys = "0112"
newKey[[1]]@dimensions$measuredItemCPC@keys = "0113"
newKey[[1]]@dimensions$measuredItemCPC@keys = "0133"

## Cabage (T, ,E)
newKey[[1]]@dimensions$measuredItemCPC@keys = "01212"

## Asparagus (T, , E)
newKey[[1]]@dimensions$measuredItemCPC@keys = "01211"

## Chili (E, T, )
newKey[[1]]@dimensions$measuredItemCPC@keys = "01231"

## tomatoes (E, , T)
newKey[[1]]@dimensions$measuredItemCPC@keys = "01234"

## bananas (, E, T)
newKey[[1]]@dimensions$measuredItemCPC@keys = "01312"


## Pineapples (, E, T)
newKey[[1]]@dimensions$measuredItemCPC@keys = "01318"

## Olives (, E, T)
newKey[[1]]@dimensions$measuredItemCPC@keys = "01450"

## Potatoes (, E, T)
newKey[[1]]@dimensions$measuredItemCPC@keys = "01510"

## Ginger (E, , T)
newKey[[1]]@dimensions$measuredItemCPC@keys = "01657"

## cattle (E, , T)
newKey[[1]]@dimensions$measuredItemCPC@keys = "02111"

## Everything
tmp = GetCodeList("agriculture", "agriculture", "measuredItemCPC")
newKey[[1]]@dimensions$measuredItemCPC@keys =
    tmp[!is.na(as.numeric(code)), code]


testHistory = GetHistory(newKey[[1]], newPivot)
testHistory[, timePointYears := as.numeric(timePointYears)]
(testTable = computeFlagWeight(testHistory, method = "entropy"))



xyplot(log(Symb_) + log(Symb_E) + log(Symb_T) ~
       timePointYears|geographicAreaM49,
       data = check[check$measuredElement == 5510, ], type = c("g", "l"),
       auto.key = TRUE)


commodityHistory = data.table(merge(data.frame(tmp),
    FAOcountryProfile[, c("UN_CODE", "FAO_TABLE_NAME")],
    by.x = "geographicAreaM49", by.y = "UN_CODE"))
commodityHistory[, timePointYears := as.numeric(timePointYears)]
commodityHistory = commodityHistory[flagObservationStatus != "M", ]

## Calculate the number of history
commodityHistory[, numberOfHistory :=
                 length(unique(flagObservationStatus)),
            by = c("geographicAreaM49", "measuredElement",
                "measuredItemCPC", "timePointYears")]

## Average the history if there is more than one symble for each
## history. The MSE would be the same, but the entropy will
## decrease. This is a temporary solution.
commodityHistory[, Value := mean(Value),
            by = c("geographicAreaM49", "measuredElement",
                "measuredItemCPC", "timePointYears",
                "flagObservationStatus")]

## Subset the data which has more than history.
commodityFinalHistory.dt =
    unique(commodityHistory[numberOfHistory >= 2,
                       list(geographicAreaM49, FAO_TABLE_NAME,
                            measuredElement, measuredItemCPC,
                            timePointYears, Value,
                            flagObservationStatus)])

## Change the name of the history
commodityFinalHistory.dt[, flagObservationStatus :=
                    paste0("Symb_", flagObservationStatus)]

## Caste the data
commodityCasted.dt = data.table(dcast(data = commodityFinalHistory.dt,
    geographicAreaM49 + FAO_TABLE_NAME +
    measuredElement + measuredItemCPC +
    timePointYears ~ flagObservationStatus, value.var = "Value"))

## Plot the percentage difference
par(mfrow = c(2, 1))
hist(commodityCasted.dt[, (Symb_ - Symb_E)/Symb_ * 100],
     breaks = seq(-10000, 10000, by = 1),
     xlim = c(-50 ,50), ylim = c(0, 0.2),
     freq = FALSE,
     main = "Percentage difference distribution of FAO Estimates (E)",
     xlab = "")
hist(commodityCasted.dt[, (Symb_ - Symb_T)/Symb_ * 100],
     breaks = seq(-10000, 10000, by = 1),
     xlim = c(-50, 50), ylim = c(0, 0.2),
     freq = FALSE,
     main = "Percentage difference distribution of Interpolation (T)",
     xlab = "")


## Take only the subset which has all 3 values
commodityNoMiss.dt =
    commodityCasted.dt[!is.na(Symb_) &
                       !is.na(Symb_E) &
                       !is.na(Symb_T) &
                       Symb_ >= 10, ]

## Plot the percentage difference
par(mfrow = c(2, 1))
hist(commodityNoMiss.dt[, (Symb_ - Symb_E)/Symb_ * 100],
     freq = FALSE, breaks = 50,
     main = "Percentage difference distribution of FAO Estimates (E)",
     xlim = c(-200, 200),
     xlab = "")
hist(commodityNoMiss.dt[, (Symb_ - Symb_T)/Symb_ * 100],
     freq = FALSE, breaks = 50,
     main = "Percentage difference distribution of Interpolation (T)",
     xlim = c(-200, 200),
     xlab = "")



crossEntropy = function(x, y){
    entropy(x) + KL.empirical(x, y)
}

commodityNoMiss.dt[, crossEntropy(Symb_, Symb_)]
commodityNoMiss.dt[, crossEntropy(Symb_, Symb_E)]
commodityNoMiss.dt[, crossEntropy(Symb_, Symb_T)]


commodityNoMiss.dt[, KL.empirical(Symb_, Symb_)]
commodityNoMiss.dt[, KL.empirical(Symb_, Symb_E)]
commodityNoMiss.dt[, KL.empirical(Symb_, Symb_T)]


## It has to be done on the same probability measure, this is the
## deciding evidence that T is better than E.
with(commodityNoMiss.dt,
     KL.empirical(Symb_, Symb_E)
     )
with(commodityNoMiss.dt,
     KL.empirical(Symb_, Symb_T)
     )



test = apply(data.matrix(commodityNoMiss.dt[, list(Symb_E, Symb_T)]), 2,
      FUN = function(x){
          KL.empirical(commodityNoMiss.dt[, Symb_], x)
      }
  )


computeFlagWeight = function(history, method = "similarity"){
    removeMhistory = history[flagObservationStatus != "M", ]

    ## Calculate the number of history
    removeMhistory[, numberOfHistory :=
                   length(unique(flagObservationStatus)),
                   by = c("geographicAreaM49", "measuredElement",
                       "measuredItemCPC", "timePointYears")]

    ## Average the history if there is more than one symble for each
    ## history. The MSE would be the same, but the entropy will
    ## decrease. This is a temporary solution.
    removeMhistory[, Value := mean(Value),
                   by = c("geographicAreaM49", "measuredElement",
                       "measuredItemCPC", "timePointYears",
                       "flagObservationStatus")]

    ## Subset the data which has more than history.
    finalHistory.dt =
        unique(removeMhistory[numberOfHistory >= 2,
                                list(geographicAreaM49, 
                                     measuredElement, measuredItemCPC,
                                     timePointYears, Value,
                                     flagObservationStatus)])

    ## Change the name of the history
    finalHistory.dt[, flagObservationStatus :=
                    paste0("Symb_", flagObservationStatus)]

    castedHistory =
        dcast(finalHistory.dt[, list(geographicAreaM49, measuredElement,
                             measuredItemCPC, timePointYears, Value,
                             flagObservationStatus)],
              geographicAreaM49 + measuredElement + measuredItemCPC +
              timePointYears ~ flagObservationStatus,
              value.var = "Value")
    ## print(str(castedHistory))

    if(method == "similarity"){
        symbNames =
            colnames(castedHistory)[grepl("Symb_", colnames(castedHistory))]

        

        imputedHistory =
            amelia(castedHistory, m = 100, ts = "timePointYears",
                   cs = "geographicAreaM49",
                   logs = symbNames,
                   idvars = c("measuredElement", "measuredItemCPC"),
                   p2s = 0)
   
        computeCentroidWeights = function(x){
            similarity = 1/rowSums(as.matrix(dist(t(x))))
            weights = similarity/sum(similarity)
            weights
        }
        finalWeights =
            rowMeans(sapply(imputedHistory$imputation,
                            FUN = function(x)
                                computeCentroidWeights(data.matrix(x[, symbNames])))
                     )
        weightTable =
            data.frame(flagObservationStatus =
                       c(gsub("Symb_", "",
                              names(sort(finalWeights,
                                         decreasing = TRUE)
                                    )
                              ), "M"
                         ),
                       flagObservationWeights =
                           c(sort(finalWeights, decreasing = TRUE), 0),
                       row.names = NULL)
    } else if(method == "entropy"){
        symbNames =
            colnames(castedHistory)[grepl("Symb_", colnames(castedHistory))]        
        condition = paste0("!is.na(castedHistory$",
            symbNames, ")", collapse = " & ")
        commodityNoMiss.dt =
            castedHistory[eval(parse(text = condition)), ]

        weightTable = with(commodityNoMiss.dt,
            data.frame(flagObservationStatus = c("", "T", "E", "M"),
                       flagObservationWeights =
                       c(1, 1/(1 + KL.empirical(Symb_, Symb_T)),
                         1/(1 + KL.empirical(Symb_, Symb_E)), 0))
            )
    }                
    weightTable
}



## Estimating centroid.
testimpute = amelia(commodityCasted.dt, m = 100, ts = "timePointYears",
    cs = "geographicAreaM49", logs = c("Symb_", "Symb_E", "Symb_T"),
    idvars = c("FAO_TABLE_NAME", "measuredElement", "measuredItemCPC"))


rowMeans(testimpute$mu)
rowMeans(testimpute$covMatrices)
tmp = matrix(nc = 3, nr = length(testimpute$imputation))
for(i in 1:length(testimpute$imputation)){
    check = testimpute$imputations[[i]]


    similarity =
        1/rowSums(as.matrix(dist(t(data.matrix(check[ , list(Symb_, Symb_E, Symb_T)])),
                                 diag = TRUE,  upper = TRUE)))^2
    weights = similarity/sum(similarity)
    tmp[i, ] = weights
}

finalWeights = colMeans(tmp)

check[, centroid :=
      Symb_ * finalWeights[1] +
      Symb_E * finalWeights[2] +
      Symb_T* finalWeights[3]]


check[, lcentroid :=
      log(Symb_) * finalWeights[1] +
      log(Symb_E) * finalWeights[2] +
      log(Symb_T) * finalWeights[3]]


xyplot(Symb_ + Symb_E + Symb_T + centroid ~
       timePointYears|FAO_TABLE_NAME,
       data = check[measuredElement == 5510, ], type = c("g", "l"),
       auto.key = TRUE)


xyplot(log(Symb_) + log(Symb_E) + log(Symb_T) + lcentroid ~
       timePointYears|FAO_TABLE_NAME,
       data = check[measuredElement == 5510, ], type = c("g", "l"),
       auto.key = TRUE)

commodityCasted.dt[Symb_ >= 6e07, ]


ggplot(commodityHistory[geographicAreaM49 == "356" &
                   measuredElement == 5510, ],
       aes(x = timePointYears, y = Value, col = flagObservationStatus)) +
    geom_point() +
    geom_line()




commodityFinalHistory.dt[geographicAreaM49 == "104" &
               timePointYears == 2009 &
               measuredElement == 5312, ]

xyplot(Value ~ timePointYears|FAO_TABLE_NAME,
       data = commodityFinalHistory.dt[measuredElement == 5510, ],
       type = c("g", "p"), group = flagObservationStatus,
       auto.key = TRUE)


test3 = dcast(test2[, list(geographicAreaM49, measuredElement, measuredItemCPC, timePointYears, Value, flagObservationStatus)], geographicAreaM49 + measuredElement + measuredItemCPC + timePointYears ~ flagObservationStatus, value.var = "Value")



## Benfords Distribution
dbenford = function(x){
    log(1 + 1/x, 10)
}

par(mfrow = c(3, 1))
firstDigitTable = data.frame(table(as.numeric(substr(as.character(na.omit(commodityCasted.dt[Symb_ != 0, Symb_])),1, 1))))
firstDigitTable$density = firstDigitTable$Freq/sum(firstDigitTable$Freq)
plot(firstDigitTable[, c("Var1", "density")])
points(1:9, dbenford(1:9), pch = 19)
lines(1:9, dbenford(1:9))


firstDigitTable = data.frame(table(as.numeric(substr(as.character(na.omit(commodityCasted.dt$Symb_E)),1, 1))))
firstDigitTable$density = firstDigitTable$Freq/sum(firstDigitTable$Freq)
plot(firstDigitTable[, c("Var1", "density")])
points(1:9, dbenford(1:9), pch = 19)
lines(1:9, dbenford(1:9))


firstDigitTable = data.frame(table(as.numeric(substr(as.character(na.omit(commodityCasted.dt$Symb_T)),1, 1))))
firstDigitTable$density = firstDigitTable$Freq/sum(firstDigitTable$Freq)
plot(firstDigitTable[, c("Var1", "density")])
points(1:9, dbenford(1:9), pch = 19)
lines(1:9, dbenford(1:9))

