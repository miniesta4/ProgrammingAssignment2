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
	ha <- outcomeData[outcomeData[[col]]!="Not Available",c(2,7,col)]
	
## Vector numeric	
	ha_num <- as.numeric(ha[[3]])
	
## Con columna numeric
	ha <- cbind(ha,ha_num)

## Ordenado	
	ha <- ha[order(ha[[4]],ha[[1]]),c(1,2,4)]
	
## Splitted
	ha_sp <- split(ha,ha[[2]])
	
## Con ranking
	m <- length(ha_sp)
	for (i in 1:m) {
		n <- length(ha_sp[[i]][[1]])
		ha_sp[[i]] <- cbind(ha_sp[[i]],rank=c(1:n))
	}

## Parameters and resultado
	resultado <- character(1)
	
	if (num == "worst") {
		resultado <- tail(ha_sp[[state]],1)[[1]]
	} else {
		num_i <- num
		if (num == "best") num_i <- 1
		resultado <- ha_sp[[state]][ha_sp[[state]]$rank==num_i,1]
	}
	
	resultado[1]
}