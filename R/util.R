# from gradDescent
getMinMax <- function(dataSet) {
    #create result matrix
    result <- matrix(dataSet, nrow = 2, ncol(dataSet))
    #loop the data column
    for (column in 1:ncol(dataSet)) {
        result[1 , column] <- min(unlist(dataSet[column]))
        result[2 , column] <- max(unlist(dataSet[column])) - min(unlist(dataSet[column]))
    }
    return(result)
}





minmaxScaling <- function(dataSet){
    columnLength <- ncol(dataSet)
    minmaxParameter <- getMinMax(dataSet)
    minValue <- minmaxParameter[1,]
    maxminValue <- minmaxParameter[2,]
    scaledDataSet <- (dataSet-minValue)/(maxminValue)
    result <- list()
    result$scaledDataSet <- scaledDataSet
    result$scalingParameter <- minmaxParameter
    return(result)
}





minmaxDescaling <- function(dataSet, minmaxParameter) {
    columnLength <- ncol(dataSet)
    minValue <- minmaxParameter[1,]
    maxminValue <- minmaxParameter[2,]
    descaledDataSet <- (dataSet * maxminValue) + minValue
    result <- descaledDataSet
    return(result)
}
