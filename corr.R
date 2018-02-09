#Write a function that takes a directory of data files and a threshold for 
#complete cases and calculates the correlation between sulfate and nitrate for 
#monitor locations where the number of completely observed cases 
#(on all variables) is greater than the threshold. The function should return 
#a vector of correlations for the monitors that meet the threshold requirement. 
#If no monitors meet the threshold requirement, then the function should return 
#a numeric vector of length 0. A prototype of this function follows.

corr <- function(directory, threshold = 0){
    source("complete.R")
    cc <- complete(directory, id = 1:332)
    cc <- cc[(cc$nobs > threshold),]
    corr_data <- vector()
    for(i in 1:nrow(cc)){
        if(cc$id[i] < 10){
            rawdata <- read.csv(paste0(directory, "/00",cc$id[i],".csv", sep =""))
            rawdata <- rawdata[complete.cases(rawdata),]
            corr_data[i] <- cor(rawdata$sulfate, rawdata$nitrate)
        }else if(cc$id[i] >= 10 & cc$id[i] <= 99){
            rawdata <- read.csv(paste0(directory, "/0",cc$id[i],".csv", sep =""))
            rawdata <- rawdata[complete.cases(rawdata),]
            corr_data[i] <- cor(rawdata$sulfate, rawdata$nitrate)
        }else{
            rawdata <- read.csv(paste0(directory, "/",cc$id[i],".csv", sep =""))
            rawdata <- rawdata[complete.cases(rawdata),]
            corr_data[i] <- cor(rawdata$sulfate, rawdata$nitrate)
        }
    }
    return(corr_data)
}