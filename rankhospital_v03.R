rankhospital <- function(state, outcome, num = "best") {
## Read outcome data
	outcomeData <- read.csv("outcome-of-care-measures.csv",colClasses = "character")

## Check that state and outcome and num are valid
	states_l <- levels(factor(outcomeData[[7]]))
	if (sum(states_l==state)==0) stop("invalid state")
	
	outcomes <- c("heart attack","heart failure","pneumonia")
	if (sum(outcomes==outcome)==0) stop("invalid outcome")
	
	if (!is.numeric(num) & num!="best" & num!="worst"){
			stop("invalid num")
	}
	
## Identifica columna a analizar	
	if (outcome == "heart attack"){
		col <- 11
	} else if (outcome == "heart failure"){
		col <- 17
	} else{
		col <- 23
	}

## Return hospital name in that state with the given rank
## 30-day death rate

## Filtro outcomeData
	ha <- outcomeData[outcomeData[[col]]!="Not Available" & outcomeData[[7]] == state,c(2,col)]
	
## Vector numeric	
	ha_num <- as.numeric(ha[[2]])
	
## Con columna numeric
	ha <- cbind(ha,ha_num)

## Ordenado	
	ha <- ha[order(ha[[3]],ha[[1]]),c(1,3)]
	
## Con ranking
	m <- length(ha[[1]])
	ha <- cbind(ha,rank=c(1:m))

## Parameters and resultado
	resultado <- character(1)
	
	if (num == "worst") {
		resultado <- tail(ha,1)[[1]]
	} else {
		if (num == "best") num <- 1
		resultado <- ha[ha$rank==num,1]
	}
	
	resultado[1]
}