# === Load packages ============================================================================================================================================

.libPaths("C:/win-library/")

library(spatstat)
library(dplyr)
library(landscapemetrics)
library(lidR, lib.loc = "\\\\storage-ume.slu.se/home$/nsha0003/My Documents/R/win-library/4.1")

source("lanscape_metrics_simulation_function.R")

# === Simulation ===============================================================================================================================================

Rs <- 1:7
dens <- seq(50, 1200, 50)
iter <- 2500
vprint <- c(1, seq(500, 2500, 500))
set.seed(123456)

for (k in 1:7) {
  R <- Rs[k]

  for (j in 1:24) {
    den <- dens[j]

    Results <- list()

    for (i in 1:iter) {
     
      if (i %in% vprint) {
        print(c(k, j, i))
      }

      b <- simulate_boolean(
        x = (190 * 2.5),
        y = (280 * 2.5),
        nd = den,
        R = R,
        res = 2.5
      )

      Results[[i]] <- b$m
    }
  

  myfile <- paste(paste(paste("boolean_simu", k, sep = "_"), j, sep = "_"), "Rdata", sep = ".")
  save(Results, file = myfile)

  rm(Results, myfile)
  
  }
}
