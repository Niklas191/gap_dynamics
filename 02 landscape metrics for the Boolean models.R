# === Load packages ============================================================================================================================================

.libPaths("C:/win-library/")

library(spatstat)
library(dplyr)
library(landscapemetrics)
library(lidR, lib.loc = "\\\\storage-ume.slu.se/home$/nsha0003/My Documents/R/win-library/4.1")

source("lanscape_metrics_simulation_function.R")

# === Setup ====================================================================================================================================================

metrics <- as.data.frame(matrix(NA, ncol = 10, nrow = (2500 * 24 * 7)))
names(metrics) <- c("R", "density", "i", "lsm_c_pland", "lsm_c_area_mn", "lsm_l_np", "lsm_c_clumpy", "lsm_c_nlsi", "lsm_l_division", "lsm_c_frac_mn")

metrics$R <- rep(1:7, each = 24 * 2500)
metrics$density <- rep(rep(seq(50, 1200, 50), each = 2500), times = 7)
metrics$i <- rep(1:2500, times = 168)

Rs <- 1:7
dens <- seq(50, 1200, 50)
iter <- 2500
vprint <- c(1, seq(500, 2500, 500))

# === For which area? ==========================================================================================================================================

# Area 1
# window_rows <- 26:253 
# window_cols <- 19:169 

# Area 2
# window_rows <- 51:231  
# window_cols <- 26:166  

# Area 3
# window_rows <- 58:221
# window_cols <- 25:164

# Area 4
# window_rows <- 71:208
# window_cols <- 58:131

# === Computation ==============================================================================================================================================

set.seed(123456)

for (k in 1:7) {
  R <- Rs[k]
  
  for (j in 1:24) {
    den <- dens[j]
    
    myfile <- paste(paste(paste("boolean_simu", k, sep = "_"), j, sep = "_"), "Rdata", sep = ".")
    load(file = myfile)    
    
    for (i in 1:iter){ 
      
      if(i %in% vprint) {
        print(c(k, j, i))
      }
      
      ind <- (k-1) * length(dens) * iter + (j-1) * iter + i
      
      bm <- (Results[[i]])[window_rows, window_cols]
      
      b <- raster(bm, 
                  xmn = 1, 
                  xmx	= (ncol(bm) * 2.5), 
                  ymn = 1,
                  ymx = (nrow(bm) * 2.5))
      
      if(length(table(b@data@values)) > 1){
        
        m1 <- lsm_c_pland(b) 
        m2 <- lsm_c_area_mn(b)
        m3 <- lsm_l_np(b)
        m4 <- lsm_c_clumpy(b)
        m5 <- lsm_c_nlsi(b) #m7
        m6 <- lsm_l_division(b) #m11
        m7 <- lsm_c_frac_mn(b) #13
        
        m1 <- m1$value[m1$class == 0]
        m2 <- m2$value[m2$class == 0]
        m3 <- m3$value
        m4 <- m4$value[m4$class == 0]
        m5 <- m5$value[m5$class == 0]
        m6 <- m6$value
        m7 <- m7$value[m7$class == 0]

        
        metrics[ind, 4] <- m1
        metrics[ind, 5] <- m2
        metrics[ind, 6] <- m3
        metrics[ind, 7] <- m4
        metrics[ind, 8] <- m5
        metrics[ind, 9] <- m6
        metrics[ind, 10] <- m7
        
      }
    }
  }
}

# save(metrics, file = "Metrics_LocX.Rdata")
