best <- function(state, outcome) {
## Read outcome data
	outcomeData <- read.csv("outcome-of-care-measures.csv",colClasses = "character")

## Check that state and outcome are valid
	states_l <- levels(factor(outcomeData[,7]))
	if (sum(states_l==state)==0) stop("invalid state")
	
	outcomes <- c("heart attack","heart failure","pneumonia")
	if (sum(outcomes==outcome)==0) stop("invalid outcome")

## Return hospital name in that state with lowest 30-day death
## rate
	hosp <- outcomeData[[2]]
	est <- outcomeData[[7]]
	ataque <- as.numeric(outcomeData[[11]])
	fallo <- as.numeric(outcomeData[[17]])
	pneu <- as.numeric(outcomeData[[23]])
	
	outcome_i <- 0
	if (outcome == "heart attack"){
		outcome_i <- 3
	} else if (outcome == "heart failure") {
		outcome_i <- 4
	} else if (outcome == "pneumonia") {
		outcome_i <- 5
	}
	datos <- data.frame(hosp,est,ataque,fallo,pneu)
	minimo <- min((datos[datos[[2]]==state,outcome_i]),na.rm=TRUE)
	hosp <- datos[datos[[2]]==state & !is.na(datos[[outcome_i]]) & datos[[outcome_i]] == minimo,1]
	hosp_v <- as.vector(hosp)
	hosp_v[1]
}

