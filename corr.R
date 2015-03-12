corr <- function(directory, threshold = 0) {
        completos <- complete(directory)
		completos_t_id <- completos[completos$nobs>threshold,1]
		corrs <- numeric(0)
		as <- list.files(directory,full.names = TRUE)
		for (i in completos_t_id) {
			a <- read.csv(as[i])
			a_c <- a[complete.cases(a),]
			corr_i <- cor(a_c[,2], a_c[,3])
			corrs <- c(corrs,corr_i)
		}
		corrs
}
