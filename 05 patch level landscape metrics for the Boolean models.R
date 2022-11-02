# === Load packages ============================================================================================================================================

.libPaths("C:/win-library/")

library(spatstat)
library(dplyr)
library(logKDE) # function logdensity()
library(landscapemetrics)
library(lidR, lib.loc = "\\\\storage-ume.slu.se/home$/nsha0003/My Documents/R/win-library/4.1")
library(GET)

source("lanscape_metrics_simulation_function.R")

set.seed(12345)

# === Compute landscape metrics ================================================================================================================================

areas <- list(c(151, 228), c(141, 181), c(140, 164), c(74, 138))

Rs <- c(3,4,5)

for (j in 1:4) {

  AREA <- areas[[j]]

  AREA %>% paste("Area:", .) %>% print()

  for (k in 1:3) {

    R <- Rs[k]

    enn <- density_envelope(metric = lsm_p_enn,
                             R = R,
                             point_density_index = c(6, 20),
                             path = "",
                             name = "boolean_simu",
                             iterations = 2500,
                             log = TRUE,
                             verbose = TRUE,
                             bandwidth = 0.2,
                             res = 2.5,
                             area = AREA,
                             xlim = c(0, 12),
                             Kernel = "gaussian",
                             output = "both")

    myfile1 <- "enn_Loc" %>% paste(.,j, sep = "") %>% paste(.,k, sep = "_")  %>% paste(.,".Rdata", sep = "")
    save(enn, file = myfile1)

    p_area <- density_envelope(metric = lsm_p_area,
                            R = R,
                            point_density_index = c(6, 20),
                            path = "",
                            name = "boolean_simu",
                            iterations = 2500,
                            log = TRUE,
                            verbose = TRUE,
                            bandwidth = 0.4,
                            res = 2.5,
                            area = AREA,
                            xlim = c(0, 0.2),
                            Kernel = "gaussian",
                            output = "both")

    myfile2 <- "gap_area_Loc" %>% paste(.,j, sep = "") %>% paste(.,k, sep = "_")  %>% paste(.,".Rdata", sep = "")
    save(p_area, file = myfile2)
  }
}
