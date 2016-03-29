pollutantmean <- function (directory, pollutant, id = 1:332) {
  monitorPmMean <- rep(NA, length(id))
  monitorPmCt   <- rep(NA, length(id))
  for (i in 1:length(id)) {
    filename <- paste0(directory, "/", sprintf("%03d", id[i]), ".csv")
    pm <- read.table(file = filename, header = TRUE, sep = ",", na.strings = "NA",
                     colClasses = c("character", "numeric", "numeric", "integer"))
    monitorPmMean[i] <- mean(pm[, pollutant], na.rm = TRUE)
    monitorPmCt[i] <- dim(pm)[1] - sum(is.na(pm[, pollutant]))
    print(paste("ID:", id[i], " | Mean:", monitorPmMean[i], " | Count:", monitorPmCt[i]))
  }

  pmMean <- sum(monitorPmMean * monitorPmCt, na.rm = TRUE) / sum(monitorPmCt)
  pmMean <- round(pmMean, digits = 3)
  return(pmMean)
}


