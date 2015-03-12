complete <- function(directory, id = 1:332) {
		archivos <- list.files(directory, full.names = TRUE)
		ids <- numeric(0)
		nobss <- numeric(0)
		for (i in id) {
			a <- read.csv(archivos[i])
			a_c <- a[complete.cases(a),]
			a_cn <- nrow(a_c)
			ids <- c(ids,i)
			nobss <- c(nobss,a_cn)
		}
		data.frame(id=ids,nobs=nobss)
}
