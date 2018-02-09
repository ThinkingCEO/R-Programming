## The fuction below calculate the mean of a pollutant (sulfate or nitrate)
## across a specific list of monitors.
## The function takes three arguments: "directory", "pollutant" and id
## Given a vector monitor ID numbers


pollutantmean <- function(directory,pollutant, id = 1:332){
    rawdata <- data.frame()
    for(i in 1:length(id)){
        if(id[i] < 10){
            rawdata <- rbind(rawdata, read.csv(paste0(directory, "/00",id[i],".csv", sep ="")))
        }else if(id[i] >= 10 & id[i] <= 99){
            rawdata <- rbind(rawdata, read.csv(paste0(directory, "/0",id[i],".csv", sep ="")))
        }else{
            rawdata <- rbind(rawdata, read.csv(paste0(directory, "/",id[i],".csv", sep ="")))
        }
    }
    rawdata <- rawdata[complete.cases(rawdata),]
    
    return(colMeans(rawdata[pollutant]))
}


