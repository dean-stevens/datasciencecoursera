completeObs <- function (directory, id = 1:332) {
  monitorNObs <- as.integer(rep(NA, length(id)))
  monitorId   <- as.integer(rep(NA, length(id)))
  for (i in 1:length(id)) {
    filename <- paste0(directory, "/", sprintf("%03d", id[i]), ".csv")
    pm <- read.table(file = filename, header = TRUE, sep = ",", na.strings = "NA",
                     colClasses = c("character", "numeric", "numeric", "integer"))
    monitorNObs[i] <- sum(!is.na(pm[, "sulfate"]) & !is.na(pm[, "nitrate"]))
    monitorId[i]   <- pm[1, "ID"]
  }

  nObsAll <- data.frame(id = monitorId, nobs = monitorNObs)

  return(nObsAll)
}


