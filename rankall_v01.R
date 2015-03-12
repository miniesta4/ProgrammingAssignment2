rankall <- function(outcome, num = "best") {

	## Read outcome data
	outcomeData <- read.csv("outcome-of-care-measures.csv",colClasses = "character")

	## Check that state and outcome are valid
	outcomes <- c("heart attack","heart failure","pneumonia")
	
	if (sum(outcomes==outcome)==0) stop("invalid outcome")
	if (!is.numeric(num) & num!="best" & num!="worst") stop("invalid num")
		
	## For each state, find the hospital of the given rank

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
	m1 <- length(ha_sp)
	for (i in 1:m1) {
		n <- length(ha_sp[[i]][[1]])
		ha_sp[[i]] <- cbind(ha_sp[[i]],rank=c(1:n))
	}
	
	m2 <- length(hf_sp)
	for (i in 1:m2) {
		n <- length(hf_sp[[i]][[1]])
		hf_sp[[i]] <- cbind(hf_sp[[i]],rank=c(1:n))
	}
	
	m3 <- length(pn_sp)
	for (i in 1:m3) {
		n <- length(pn_sp[[i]][[1]])
		pn_sp[[i]] <- cbind(pn_sp[[i]],rank=c(1:n))
	}
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	
	state <- character(0)
	hospital <- character(0)
	
	if (num == "worst"){
		if (outcome == "heart attack"){
			for (i in 1:m1) {
				state_i <- names(ha_sp[i])
				hospital_i <- tail(ha_sp[[i]],1)[[1]]
				state <- c(state,state_i)
				hospital <- c(hospital,hospital_i[1])
			}
		}
		if (outcome == "heart failure"){
			for (i in 1:m2) {
				state_i <- names(hf_sp[i])
				hospital_i <- tail(hf_sp[[i]],1)[[1]]
				state <- c(state,state_i)
				hospital <- c(hospital,hospital_i[1])
			}
		}
		if (outcome == "pneumonia"){
			for (i in 1:m3) {
				state_i <- names(pn_sp[i])
				hospital_i <- tail(pn_sp[[i]],1)[[1]]
				state <- c(state,state_i)
				hospital <- c(hospital,hospital_i[1])
			}
		}
	} else {
		if (num == "best") num <- 1
		
		if (outcome == "heart attack"){
			for (i in 1:m1) {
				state_i <- names(ha_sp[i])
				hospital_i <- ha_sp[[i]][ha_sp[[i]]$rank==num,1]
				state <- c(state,state_i)
				hospital <- c(hospital,hospital_i[1])
			}
		}
		if (outcome == "heart failure"){
			for (i in 1:m2) {
				state_i <- names(hf_sp[i])
				hospital_i <- hf_sp[[i]][hf_sp[[i]]$rank==num,1]
				state <- c(state,state_i)
				hospital <- c(hospital,hospital_i[1])
			}
		}
		if (outcome == "pneumonia"){
			for (i in 1:m3) {
				state_i <- names(pn_sp[i])
				hospital_i <- pn_sp[[i]][pn_sp[[i]]$rank==num,1]
				state <- c(state,state_i)
				hospital <- c(hospital,hospital_i[1])
			}
		}
	}
	
	data.frame(hospital,state)
}