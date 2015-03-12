complete <- function(directory, id = 1:332) {
  archivos <- list.files(directory,full.names=TRUE)
  id_v <- numeric(0)
  nobs_v <- numeric(0)
  for (i in id) {
    id_v <- c(id_v,i)
    arch <- read.csv(archivos[i])
    nobs_v <- c(nobs_v,nrow(arch[complete.cases(arch),]))
  }
  data.frame(id=id_v,nobs=nobs_v)
}