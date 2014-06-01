corr <- function(directory, threshold = 0) {
  filename <- list.files(directory)
  x <- paste(directory, "/", sep="")
  monitor_list <- paste(x, filename, sep="")
  completeLocations <- NULL
  corrData <- NULL
  
  
  for (monitor in monitor_list) {
    locationInfo <- read.csv(monitor)
    completeLocations <- as.data.frame(rbind(completeLocations, sum(complete.cases(locationInfo))))
    corrData <- data.frame(rbind(corrData, cor(locationInfo$sulfate, locationInfo$nitrate, use = "na.or.complete")))
    
    
  }
  
  
  allData <- cbind(completeLocations, corrData)
  colnames(allData) <- c("observations", "correlation" )
  greater <- allData$observations > threshold
  print(allData$correlation[greater])
  
  
  
}
