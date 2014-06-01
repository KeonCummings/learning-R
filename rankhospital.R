library (plyr)

rankhospital <- function(state, illness, rank = "best") {
	outcome <- read.csv("data3/outcome.csv")

	types <- c("heart attack", "heart failure", "pneumonia")

	 if(!state %in% outcome$State){
        return(stop("invalid state"))
    } else if(!illness %in% types){
        return(stop("invalid outcome"))
    }

	hA <- cbind(as.numeric(as.character(outcome[,11][outcome$State == state])), 
				as.character(outcome$Hospital.Name[outcome$State == state]))
	hF <- cbind(as.numeric(as.character(outcome[,17][outcome$State == state])), 
				as.character(outcome$Hospital.Name[outcome$State == state]))
	pN <- cbind(as.numeric(as.character(outcome[,23][outcome$State == state])), 
				as.character(outcome$Hospital.Name[outcome$State == state]))


	condition <- ifelse(illness == "heart attack", list(hA), ifelse(illness == "heart failure", list(hF), 
													ifelse(illness == "pneumonia", list(pN), "not a valid input")))	

	outcomeData <- as.data.frame(condition)

	rates <- as.numeric(as.character(outcomeData[,2]))
	hospitals <- outcomeData[,1]

	orderedData = outcomeData[do.call(order, outcomeData),]
	orderedData <- data.frame(lapply(orderedData, as.character.factor), stringsAsFactors=FALSE)
	orderedData <- na.omit(orderedData)
	orderedData$rank = rank(as.numeric(as.character(orderedData[,1])), ties.method = c("first"))

	if (rank == "best") { 
				return(as.character(orderedData[,2][orderedData$rank == 1]))
			}
	else if (rank == "worst") {
				return(as.character(orderedData[,2][max(orderedData$rank)]))
			}
	else if (!rank %in% orderedData$rank) {
				rank = NA
				return(rank)
	}
	else {
				return(as.character(orderedData[,2][orderedData$rank == rank]))
			}


}


