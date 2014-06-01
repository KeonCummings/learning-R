complete <- function(directory, id = 1:332) {
                 filename <- list.files(directory)
                 x <- paste(directory, "/", sep="")
                 list <- paste(x, filename, sep="")
                 temp <- NULL
                 temp2 <- NULL
                 
              
                 for (i in id) {
                   get_info <- data.frame(read.csv(list[i]))
                   good <- complete.cases(get_info)
                   all <- get_info[good,][1:4]
                   nobs <- data.frame(all)
                   nobs <- nrow(nobs)
                   id = i
                   count2 <- cbind(id, nobs)
                   temp <- rbind(temp, count2)
                   temp <- data.frame(temp)
              
                  
                 }
                   
                 print(temp)

            
                 
}