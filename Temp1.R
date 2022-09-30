pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    setwd(directory)
    
    # 'directory' is a character vectory of length 1 indicating the location
    # of the CSV files
    if (pollutant == "sulfate") {
        colNum <- 2
    } else if (pollutant == "nitrate") {
        colNum <- 3
    } else {
        print("enter sulfate or nitrate for pollutant")
    }
    
    # 'pollutant' is a character vector of length 1 indicating the name
    # of the pollutant for which we will calculate the mean; either "sulfate"
    # or nitrate". 
    dir <- dir()
    dirSlice <- dir[id]
    ExtMon <- list("")
    print(dirSlice)
    for(i in 1:length(dirSlice)) {
        print(dirSlice[i])
        ExtMon[i] <- list(read.csv(dirSlice[i]))
        # puts all files into 1 list ExtMon
    } 
    
    SumOfFiles <- 1
    LengthOfFiles <- 1
    for(i in 1:length(ExtMon)){
        TempDF <- as.data.frame(ExtMon[i])
        NAremove <- TempDF[,colNum] 
        Bad <- is.na(NAremove)
        NAremove <- NAremove[!Bad] # removes NAs from string
        SumOfFiles[i] <- sum(NAremove)
        LengthOfFiles[i] <- length(NAremove)
        
        
    }
    TotalMean <- sum(SumOfFiles)/sum(LengthOfFiles)
    print(TotalMean)
    ## 'id' is an integer vector indicating the monitor ID numbers to be used 
    
    ##return the mean of the pollutant across all motor list 
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: do not round result
}
pollutantmean("D:/GitRepositories/AirPollutant/specdata", "sulfate", 1)
    