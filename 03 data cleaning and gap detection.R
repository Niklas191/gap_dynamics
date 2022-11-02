# === Load packages =========================================================================================================================================================

.libPaths("C:/win-library/")

library(spatstat)
library(dplyr)
library(logKDE) # function logdensity()
library(landscapemetrics)
library(lidR, lib.loc = "\\\\storage-ume.slu.se/home$/nsha0003/My Documents/R/win-library/4.1")
library(igraph) # function clumps()
library(ineq) # function Gini() an Lc()

source("lanscape_metrics_simulation_function.R")

set.seed(12345)

# === Load data =============================================================================================================================================================


las__Loc1_2015 <- readLAS("C:/Krycklan/Locations/Loc1_2015_SWEREF.las", filter = "-keep_xy 731467 7133155 731842 7133725")
las__Loc1_2006 <- readLAS("C:/Krycklan/Locations/Loc1_2006_SWEREF.las", filter = "-keep_xy 731467 7133155 731842 7133725")

las__Loc2_2015 <- readLAS("C:/Krycklan/Locations/Loc2_2015_SWEREF.las", filter = "-keep_xy 732882 7132943 733232 7133393")
las__Loc2_2006 <- readLAS("C:/Krycklan/Locations/Loc2_2006_SWEREF.las", filter = "-keep_xy 732882 7132943 733232 7133393")

las__Loc3_2015 <- readLAS("C:/Krycklan/Locations/Loc3_2015_SWEREF.las", filter = "-keep_xy 730250 7133395 730600 7133805")
las__Loc3_2006 <- readLAS("C:/Krycklan/Locations/Loc3_2006_SWEREF.las", filter = "-keep_xy 730250 7133395 730600 7133805")

las__Loc4_2015 <- readLAS("C:/Krycklan/Locations/Loc4_2015_SWEREF.las", filter = "-keep_xy 731975 7131010 732320 7131195")
las__Loc4_2006 <- readLAS("C:/Krycklan/Locations/Loc4_2006_SWEREF.las", filter = "-keep_xy 731975 7131010 732320 7131195")

# === Create gap-map based on quantiles =====================================================================================================================================

# Loc 1
data__Loc1_2015 <- grid_metrics(las__Loc1_2015, func = quantile(Z, 0.8), res = 2.5) 
data__Loc1_2015@data@values[data__Loc1_2015@data@values < 0] <- 0 
data__Loc1_2015@data@values[! is.na(data__Loc1_2015@data@values) & data__Loc1_2015@data@values <= 2] <- 0
data__Loc1_2015@data@values[! is.na(data__Loc1_2015@data@values) & data__Loc1_2015@data@values  > 2] <- 1

data__Loc1_2006 <- grid_metrics(las__Loc1_2006, func = quantile(Z, 0.8), res = 2.5) 
data__Loc1_2006@data@values[data__Loc1_2006@data@values < 0] <- 0 
data__Loc1_2006@data@values[! is.na(data__Loc1_2006@data@values) & data__Loc1_2006@data@values <= 2] <- 0
data__Loc1_2006@data@values[! is.na(data__Loc1_2006@data@values) & data__Loc1_2006@data@values  > 2] <- 1

# Loc 2
data__Loc2_2015 <- grid_metrics(las__Loc2_2015, func = quantile(Z, 0.8), res = 2.5) 
data__Loc2_2015@data@values[data__Loc2_2015@data@values < 0] <- 0 
data__Loc2_2015@data@values[! is.na(data__Loc2_2015@data@values) & data__Loc2_2015@data@values <= 2] <- 0
data__Loc2_2015@data@values[! is.na(data__Loc2_2015@data@values) & data__Loc2_2015@data@values  > 2] <- 1

data__Loc2_2006 <- grid_metrics(las__Loc2_2006, func = quantile(Z, 0.8), res = 2.5) 
data__Loc2_2006@data@values[data__Loc2_2006@data@values < 0] <- 0 
data__Loc2_2006@data@values[! is.na(data__Loc2_2006@data@values) & data__Loc2_2006@data@values <= 2] <- 0
data__Loc2_2006@data@values[! is.na(data__Loc2_2006@data@values) & data__Loc2_2006@data@values  > 2] <- 1

# Loc 3
data__Loc3_2015 <- grid_metrics(las__Loc3_2015, func = quantile(Z, 0.8), res = 2.5) 
data__Loc3_2015@data@values[data__Loc3_2015@data@values < 0] <- 0 
data__Loc3_2015@data@values[! is.na(data__Loc3_2015@data@values) & data__Loc3_2015@data@values <= 2] <- 0
data__Loc3_2015@data@values[! is.na(data__Loc3_2015@data@values) & data__Loc3_2015@data@values  > 2] <- 1

data__Loc3_2006 <- grid_metrics(las__Loc3_2006, func = quantile(Z, 0.8), res = 2.5) 
data__Loc3_2006@data@values[data__Loc3_2006@data@values < 0] <- 0 
data__Loc3_2006@data@values[! is.na(data__Loc3_2006@data@values) & data__Loc3_2006@data@values <= 2] <- 0
data__Loc3_2006@data@values[! is.na(data__Loc3_2006@data@values) & data__Loc3_2006@data@values  > 2] <- 1

# Loc 4
data__Loc4_2015 <- grid_metrics(las__Loc4_2015, func = quantile(Z, 0.8), res = 2.5) 
data__Loc4_2015@data@values[data__Loc4_2015@data@values < 0] <- 0 
data__Loc4_2015@data@values[! is.na(data__Loc4_2015@data@values) & data__Loc4_2015@data@values <= 2] <- 0
data__Loc4_2015@data@values[! is.na(data__Loc4_2015@data@values) & data__Loc4_2015@data@values  > 2] <- 1

data__Loc4_2006 <- grid_metrics(las__Loc4_2006, func = quantile(Z, 0.8), res = 2.5) 
data__Loc4_2006@data@values[data__Loc4_2006@data@values < 0] <- 0 
data__Loc4_2006@data@values[! is.na(data__Loc4_2006@data@values) & data__Loc4_2006@data@values <= 2] <- 0
data__Loc4_2006@data@values[! is.na(data__Loc4_2006@data@values) & data__Loc4_2006@data@values  > 2] <- 1


data <- list(data__Loc1_2015, data__Loc1_2006, data__Loc2_2015, data__Loc2_2006, data__Loc3_2015, data__Loc3_2006, data__Loc4_2015, data__Loc4_2006)

rm("las__Loc1_2006", "las__Loc1_2015", "las__Loc2_2006", "las__Loc2_2015", "las__Loc3_2006", "las__Loc3_2015", "las__Loc4_2006", "las__Loc4_2015")

# === Check for NA and remove few NAs by knn (manually =====================================================================================================================

for (i in 1:8) {
  data[[i]] %>% raster::getValues() %>% is.na() %>% table() %>% print()
}

m_1_2015 <- raster::getValues(data__Loc1_2015, format = "matrix")
m_2_2015 <- raster::getValues(data__Loc2_2015, format = "matrix")

m_1_2015[is.na(m_1_2015)] <- 1
m_2_2015[is.na(m_2_2015)] <- 1

data__Loc1_2015@data@values <- t(m_1_2015)
data__Loc2_2015@data@values <- t(m_2_2015)

plot(data__Loc1_2015, col = c("red", "green"), main = "data__Loc1_2015")
plot(data__Loc2_2015, col = c("red", "green"), main = "data__Loc2_2015")

rm(data, m_1_2015, m_2_2015, i)