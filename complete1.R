complete <- function(directory, id = 1:332) {
  archivos <- list.files(directory,full.names=TRUE)
  id_v <- numeric(0)
  nobs_v <- numeric(0)
  j <- 1
  for (i in id) {
    id_v[j] <- i
    arch <- read.csv(archivos[i])
    nobs_v[j] <- nrow(arch[complete.cases(arch),])
    j <- j+1
  }
  data.frame(id=id_v,nobs=nobs_v)
}