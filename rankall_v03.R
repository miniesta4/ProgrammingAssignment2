rankall <- function(outcome, num = "best") {

	## Read outcome data
	outcomeData <- read.csv("outcome-of-care-measures.csv",colClasses = "character")

	## Check that state and outcome are valid
	outcomes <- c("heart attack","heart failure","pneumonia")
	if (sum(outcomes==outcome)==0) stop("invalid outcome")
	
	if (!is.numeric(num) & num!="best" & num!="worst") stop("invalid num")
		
	## For each state, find the hospital of the given rank
	
	## Selección de columna a analizar
	if (outcome == outcomes[1]){
		col <- 11
	} else if (outcome == outcomes[2]){
		col <- 17
	} else {
		col <- 23
	}

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
	ha_sp <- lapply(ha_sp, function(i){
			n <- nrow(i)
			i <- cbind(i,rank=c(1:n))
		})	
	
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	
	state <- character(0)
	hospital <- character(0)
	
	if (num == "worst"){
		lapply(ha_sp, function(i) {
			state_i <- i[1,2]
			hospital_i <- tail(i,1)[[1]]
			state <<- c(state,state_i[1])
			hospital <<- c(hospital,hospital_i[1])
		})
	} else {
		if (num == "best") num <- 1
		lapply(ha_sp,function(i) {
			state_i <- i[1,2]
			hospital_i <- i[i$rank==num,1]
			state <<- c(state,state_i[1])
			hospital <<- c(hospital,hospital_i[1])
		})
	}
	
	data.frame(hospital,state)
}
