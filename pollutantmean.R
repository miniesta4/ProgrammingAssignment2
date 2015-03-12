pollutantmean <- function(directory, pollutant, id = 1:332) {
		as <- list.files(directory,full.names = TRUE)
		vs <- numeric(0)
		for (i in id) {
			a <- read.csv(as[i])
			v <- a[,pollutant]
			vs <- c(vs,v)
		}
		mean(vs,na.rm = TRUE)
}
