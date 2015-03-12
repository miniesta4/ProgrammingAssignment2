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

## Return hospital name in that state with the given rank
## 30-day death rate

## Filtro outcomeData
	ha <- outcomeData[outcomeData[[11]]!="Not Available",c(2,7,11)]
	hf <- outcomeData[outcomeData[[17]]!="Not Available",c(2,7,17)]
	pn <- outcomeData[outcomeData[[23]]!="Not Available",c(2,7,23)]

## Vector numeric	
	ha_num <- as.numeric(ha[[3]])
	hf_num <- as.numeric(hf[[3]])
	pn_num <- as.numeric(pn[[3]])

## Con columna numeric
	ha <- cbind(ha,ha_num)
	hf <- cbind(hf,hf_num)
	pn <- cbind(pn,pn_num)

## Ordenado	
	ha <- ha[order(ha[[4]],ha[[1]]),c(1,2,4)]
	hf <- hf[order(hf[[4]],hf[[1]]),c(1,2,4)]
	pn <- pn[order(pn[[4]],pn[[1]]),c(1,2,4)]
	
## Splitted
	ha_sp <- split(ha,ha[[2]])
	hf_sp <- split(hf,hf[[2]])
	pn_sp <- split(pn,pn[[2]])
	
## Con ranking
	m <- length(ha_sp)
	for (i in 1:m) {
		n <- length(ha_sp[[i]][[1]])
		ha_sp[[i]] <- cbind(ha_sp[[i]],rank=c(1:n))
	}
	
	m <- length(hf_sp)
	for (i in 1:m) {
		n <- length(hf_sp[[i]][[1]])
		hf_sp[[i]] <- cbind(hf_sp[[i]],rank=c(1:n))
	}
	
	m <- length(pn_sp)
	for (i in 1:m) {
		n <- length(pn_sp[[i]][[1]])
		pn_sp[[i]] <- cbind(pn_sp[[i]],rank=c(1:n))
	}
	
	
## Parameters and resultado
	resultado <- character(1)
	
	if (num == "worst") {
		if (outcome == "heart attack"){
			resultado <- tail(ha_sp[[state]],1)[[1]]
		} else if (outcome == "heart failure") {
			resultado <- tail(hf_sp[[state]],1)[[1]]
		} else if (outcome == "pneumonia") {
			resultado <- tail(pn_sp[[state]],1)[[1]]
		}
	} else {
		num_i <- num
		if (num == "best"){
			num_i <- 1
		}
		if (outcome == "heart attack"){
			resultado <- ha_sp[[state]][ha_sp[[state]]$rank==num_i,1]
		} else if (outcome == "heart failure") {
			resultado <- hf_sp[[state]][hf_sp[[state]]$rank==num_i,1]
		} else if (outcome == "pneumonia") {
			resultado <- pn_sp[[state]][pn_sp[[state]]$rank==num_i,1]
		}
	}

	resultado[1]
	
}