##Write a function that reads a directory full of files and reports the number of 
##completely observed cases in each data file. The function should return a 
##data frame where the first column is the name of the file and the second column 
##is the number of complete cases.

complete <- function(directory, id = 1:332){
    nobs <- vector()
    for(i in 1:length(id)){
        if(id[i] < 10){
            rawdata <- read.csv(paste0(directory, "/00",id[i],".csv", sep =""))
            rawdata <- rawdata[complete.cases(rawdata),]
            nobs[i] <- nrow(rawdata)
        }else if(id[i] >= 10 & id[i] <= 99){
            rawdata <- read.csv(paste0(directory, "/0",id[i],".csv", sep =""))
            rawdata <- rawdata[complete.cases(rawdata),]
            nobs[i] <- nrow(rawdata)
        }else{
            rawdata <- read.csv(paste0(directory, "/",id[i],".csv", sep =""))
            rawdata <- rawdata[complete.cases(rawdata),]
            nobs[i] <- nrow(rawdata)
        }
    }
    CompCount <- cbind.data.frame(id,nobs)
    return(CompCount)
}

