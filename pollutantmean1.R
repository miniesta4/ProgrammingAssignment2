pollutantmean <- function(directory, pollutant, id = 1:332) {
  archivos <- list.files(directory,full.names=TRUE)
  tabla <- data.frame()
  for (i in id) {
    tabla <- rbind(tabla,read.csv(archivos[i]))
  }
  tabla_sel <- tabla[,pollutant]
  mean (tabla_sel,na.rm=TRUE)
}
