pollutantmean <- function(directory, pollutant, id = 1:332) {
                 filename <- list.files(directory)
                 x <- paste(directory, "/", sep="")
                 list <- paste(x, filename, sep="")
                 temp <- NULL
                 
                 
                 for (i in id) {
                   get_info <- data.frame(read.csv(list[i]))
                   temp <- rbind.data.frame(temp, get_info)
                   
                   
                 }
                 
                 pollutant_range <- temp[,pollutant]
                 all_numbers <- pollutant_range[!is.na(pollutant_range)]
                 print (mean(all_numbers))
                   
                 }