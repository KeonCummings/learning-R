rankall <- function(illness, rank = "best") {
	outcome <- read.csv("data3/outcome.csv")

	types <- c("heart attack", "heart failure", "pneumonia")

	if(!illness %in% types){
        return(stop("invalid outcome"))
    }

    data <- cbind(as.character(outcome$Hospital.Name), as.character(outcome$State),as.numeric(as.character(outcome[,11])), as.numeric(as.character(outcome[,17])), 
     													as.numeric(as.character(outcome[,23])))
    data <- as.data.frame(data)
    colnames(data) <- c("name", "state", "hA", "hF", "pN")


	outcomeData <- as.data.frame(data)
	orderedData = outcomeData[do.call(order, outcomeData),]
	orderedData <- data.frame(lapply(orderedData, as.character.factor), stringsAsFactors=FALSE)
	#orderedData <- na.omit(orderedData)
	orderedData$hArank = rank(as.numeric(as.character(orderedData$hA)), ties.method = c("first"))
	orderedData$hFrank = rank(as.numeric(as.character(orderedData$hF)), ties.method = c("first"))
	orderedData$pNrank = rank(as.numeric(as.character(orderedData$pN)), ties.method = c("first"))


	splitData <- split(orderedData, orderedData$state)
	splitRank = list(lapply(splitData, function (x) {
					x$localhArank = rank(as.numeric(as.character(x$hArank)))
					x$localhFrank = rank(as.numeric(as.character(x$hFrank)))
					x$localpNrank = rank(as.numeric(as.character(x$pNrank)))
					cbind(x)	
				}))

	if(illness == "heart attack") {
		results = list(lapply(splitRank[[1]], function(x) { 
			hospitals <- cbind(x$name[x$localhArank == ifelse(rank == "best", 1, 
													   ifelse(rank == "worst", max(x$localhArank),
													   rank))])
			}))
	}

	else if (illness == "heart failure") {
		results = list(lapply(splitRank[[1]], function(x) { 
			hospitals <- cbind(x$name[x$localhFrank == ifelse(rank == "best", 1, 
													   ifelse(rank == "worst", max(x$localhFrank),
													   rank))])
			}))
	}

	else if (illness == "pneumonia") {
		results = list(lapply(splitRank[[1]], function(x) { 
			hospitals <- cbind(x$name[x$localpNrank == ifelse(rank == "best", 1, 
													   ifelse(rank == "worst", max(x$localpNrank),
													   rank))])
			}))
	}

	finalData = as.data.frame(cbind(results[[1]]))
	finalData$state = rownames(finalData)
	colnames(finalData) = c("hospital", "state")

	return(finalData)


}


