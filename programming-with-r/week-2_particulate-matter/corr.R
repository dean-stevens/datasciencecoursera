corr <- function(directory, threshold = 0) {
  obsRaw <- completeObs(directory)
  obsFilt <- obsRaw[obsRaw$nobs > threshold, ]
  # obsFilt <- obsFilt[1,]
  pmCor <- numeric(0)

  if (dim(obsFilt)[1] > 0) {
    for (i in 1:dim(obsFilt)[1]) {
      # print(paste(i, obsFilt[i, ]))
      filename <- paste0(directory, "/", sprintf("%03d", obsFilt$id[i]), ".csv")
      pm <- read.table(file = filename, header = TRUE, sep = ",", na.strings = "NA",
                       colClasses = c("character", "numeric", "numeric", "integer"))
      obsRows <- !is.na(pm[, "sulfate"]) & !is.na(pm[, "nitrate"])
      obsrows <- rowSums(!is.na(pm)) == 4
      pmCor[i] <- cor(pm[obsRows, "nitrate"], pm[obsRows, "sulfate"])
    }
  }
  return(pmCor)
}

