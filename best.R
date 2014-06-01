best <- function(state, illness) {
	outcome <- read.csv("data3/outcome.csv")

	types <- c("heart attack", "heart failure", "pneumonia")

	if(!state %in% outcome$State){
        return(stop("invalid state"))
    }
	if(!illness %in% types){
        return(stop("invalid outcome"))
    }

	
	hA <- cbind(as.numeric(as.character(outcome[,11][outcome$State == state])), as.character(outcome$Hospital.Name[outcome$State == state]))
	hF <- cbind(as.numeric(as.character(outcome[,17][outcome$State == state])), as.character(outcome$Hospital.Name[outcome$State == state]))
	pN <- cbind(as.numeric(as.character(outcome[,23][outcome$State == state])), as.character(outcome$Hospital.Name[outcome$State == state]))
	
	## Check that state and outcome are valid

	
	
	condition <- ifelse(illness == "heart attack", list(hA), ifelse(illness == "heart failure", list(hF), 
													ifelse(illness == "pneumonia", list(pN), "not a valid input")))
	

	data <- condition[[1]]
	orderedValues <- order(as.numeric(data[,1]), decreasing = FALSE)
	orderedData <- data[orderedValues,]

	return(orderedData[1,2])

	#return(head(orderedData))

## Return hospital name in that state with lowest 30-day death
## rate
}
