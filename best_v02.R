best <- function(state, outcome) {
## Read outcome data
	outcomeData <- read.csv("outcome-of-care-measures.csv",colClasses = "character")

## Check that state and outcome are valid
	states_l <- levels(factor(outcomeData[[7]]))
	if (sum(states_l==state)==0) stop("invalid state")
	
	outcomes <- c("heart attack","heart failure","pneumonia")
	if (sum(outcomes==outcome)==0) stop("invalid outcome")

## Selección de columna outcome
	if (outcome == "heart attack"){
		col <- 11
	} else if (outcome == "heart failure") {
		col <- 17
	} else if (outcome == "pneumonia") {
		col <- 23
	}

	## Filtra dataset por estado y outcome
	h <- outcomeData[outcomeData[[7]] == state & outcomeData[[col]] != "Not Available",c(2,col)]
	
	## Añade columna numérica
	f_v <- as.numeric(h[[2]])
	h <- cbind(h,f_v)
	
	## Ordena
	h <- h[order(h[[3]],h[[1]]),c(1,3)]
	
	## Return hospital name in that state with lowest 30-day death rate
	h[[1]][1]
	
}

