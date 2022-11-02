# === Load packages ============================================================================================================================================

.libPaths("C:/win-library/")

library(spatstat)
library(dplyr)
library(logKDE) # function logdensity()
library(landscapemetrics)
library(lidR, lib.loc = "\\\\storage-ume.slu.se/home$/nsha0003/My Documents/R/win-library/4.1")

source("lanscape_metrics_simulation_function.R")

# Define datasets ==============================================================================================================================================

ENNs <- c("enn_Loc1_1.Rdata", "enn_Loc1_2.Rdata", "enn_Loc1_3.Rdata",
          "enn_Loc2_1.Rdata", "enn_Loc2_2.Rdata", "enn_Loc2_3.Rdata",
          "enn_Loc3_1.Rdata", "enn_Loc3_2.Rdata", "enn_Loc3_3.Rdata",
          "enn_Loc4_1.Rdata", "enn_Loc4_2.Rdata", "enn_Loc4_3.Rdata")

p_areas <- c("gap_area_Loc1_1.Rdata", "gap_area_Loc1_2.Rdata", "gap_area_Loc1_3.Rdata",
             "gap_area_Loc2_1.Rdata", "gap_area_Loc2_2.Rdata", "gap_area_Loc2_3.Rdata",
             "gap_area_Loc3_1.Rdata", "gap_area_Loc3_2.Rdata", "gap_area_Loc3_3.Rdata",
             "gap_area_Loc4_1.Rdata", "gap_area_Loc4_2.Rdata", "gap_area_Loc4_3.Rdata")

# Area 1 =======================================================================================================================================================

load(file = ENNs[1])
plot_envelope(envelope_data = enn$short, 
              metric = lsm_p_enn, 
              R = 3, 
              intensity_index = c(15, 18), 
              log = TRUE, 
              bandwidth = 0.2,
              xlim = c(0, 12), 
              ylim = c(0,0.3),
              xlab = "", 
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(1,1), 
              Kernel = "gaussian", 
              plot_legend = FALSE, 
              PDF_file = "ENN_Loc1_R3",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6)) 

load(file = ENNs[2])
plot_envelope(envelope_data = enn$short, 
              metric = lsm_p_enn, 
              R = 4, 
              intensity_index = c(6, 7, 9), 
              log = TRUE, 
              bandwidth = 0.2, 
              xlim = c(0, 12), 
              ylim = c(0, 0.3),
              xlab = "", 
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(3,5), 
              Kernel = "gaussian", 
              plot_legend = FALSE, 
              PDF_file = "ENN_Loc1_R4",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))  

load(file = ENNs[3])
plot_envelope(envelope_data = enn$short, 
              metric = lsm_p_enn, 
              R = 5, 
              intensity_index = c(6), 
              log = TRUE, 
              bandwidth = 0.2, 
              xlim = c(0, 12), 
              ylim = c(0, 0.3),
              xlab = "", 
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(3,5), 
              Kernel = "gaussian", 
              plot_legend = FALSE, 
              PDF_file = "ENN_Loc1_R5",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))  
  
load(file = p_areas[1])
plot_envelope(envelope_data = p_area$short, 
              metric = lsm_p_area, 
              R = 3, 
              intensity_index = c(16, 17), 
              log = TRUE, 
              bandwidth = 0.4, 
              xlim = c(0, 0.2), 
              ylim = c(0,10),
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(1,1), 
              Kernel = "gaussian", 
              plot_legend = FALSE,
              xlim_plot = c(0, 0.08), 
              PDF_file = "gap_area_Loc1_R3",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))
  
load(file = p_areas[2])
plot_envelope(envelope_data = p_area$short, 
              metric = lsm_p_area, 
              R = 4, 
              intensity_index = c(11,12), 
              log = TRUE, 
              bandwidth = 0.4, 
              xlim = c(0, 0.2), 
              ylim = c(0,10),
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(1,1), 
              Kernel = "gaussian", 
              plot_legend = FALSE,
              xlim_plot = c(0, 0.08), 
              PDF_file = "gap_area_Loc1_R4",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))
  
load(file = p_areas[3])
plot_envelope(envelope_data = p_area$short, 
              metric = lsm_p_area, 
              R = 5, 
              intensity_index = c(9), 
              log = TRUE, 
              bandwidth = 0.4, 
              xlim = c(0, 0.2), 
              ylim = c(0,10),
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(1,1), 
              Kernel = "gaussian", 
              plot_legend = FALSE,
              xlim_plot = c(0, 0.08), 
              PDF_file = "gap_area_Loc1_R5",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))

# Area 2 =======================================================================================================================================================

load(file = ENNs[4])
plot_envelope(envelope_data = enn$short, 
              metric = lsm_p_enn, 
              R = 3, 
              intensity_index = c(14, 18), 
              log = TRUE, 
              bandwidth = 0.2,
              xlim = c(0, 12), 
              ylim = c(0,0.3),
              xlab = "", 
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(1,1), 
              Kernel = "gaussian", 
              plot_legend = FALSE, 
              PDF_file = "ENN_Loc2_R3",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6)) 

load(file = ENNs[5])
plot_envelope(envelope_data = enn$short, 
              metric = lsm_p_enn, 
              R = 4, 
              intensity_index = c(7,9), 
              log = TRUE, 
              bandwidth = 0.2, 
              xlim = c(0, 12), 
              ylim = c(0, 0.3),
              xlab = "", 
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(3,5), 
              Kernel = "gaussian", 
              plot_legend = FALSE, 
              PDF_file = "ENN_Loc2_R4",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))  

load(file = ENNs[6])
plot_envelope(envelope_data = enn$short, 
              metric = lsm_p_enn, 
              R = 5, 
              intensity_index = c(6), 
              log = TRUE, 
              bandwidth = 0.2, 
              xlim = c(0, 12), 
              ylim = c(0, 0.3),
              xlab = "", 
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(3,5), 
              Kernel = "gaussian", 
              plot_legend = FALSE, 
              PDF_file = "ENN_Loc2_R5",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))  

load(file = p_areas[4])
plot_envelope(envelope_data = p_area$short, 
              metric = lsm_p_area, 
              R = 3, 
              intensity_index = c(16, 17), 
              log = TRUE, 
              bandwidth = 0.4, 
              xlim = c(0, 0.2), 
              ylim = c(0,10),
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(1,1), 
              Kernel = "gaussian", 
              plot_legend = FALSE,
              xlim_plot = c(0, 0.08), 
              PDF_file = "gap_area_Loc2_R3",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))

load(file = p_areas[5])
plot_envelope(envelope_data = p_area$short, 
              metric = lsm_p_area, 
              R = 4, 
              intensity_index = c(11,12), 
              log = TRUE, 
              bandwidth = 0.4, 
              xlim = c(0, 0.2), 
              ylim = c(0,10),
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(1,1), 
              Kernel = "gaussian", 
              plot_legend = FALSE,
              xlim_plot = c(0, 0.08), 
              PDF_file = "gap_area_Loc2_R4",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))

load(file = p_areas[6])
plot_envelope(envelope_data = p_area$short, 
              metric = lsm_p_area, 
              R = 5, 
              intensity_index = c(8, 9), 
              log = TRUE, 
              bandwidth = 0.4, 
              xlim = c(0, 0.2), 
              ylim = c(0,10),
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(1,1), 
              Kernel = "gaussian", 
              plot_legend = FALSE,
              xlim_plot = c(0, 0.08), 
              PDF_file = "gap_area_Loc2_R5",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))

# Area 3 =======================================================================================================================================================

load(file = ENNs[7])
plot_envelope(envelope_data = enn$short, 
              metric = lsm_p_enn, 
              R = 3, 
              intensity_index = c(15, 18), 
              log = TRUE, 
              bandwidth = 0.2,
              xlim = c(0, 12), 
              ylim = c(0,0.3),
              xlab = "", 
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(1,1), 
              Kernel = "gaussian", 
              plot_legend = FALSE, 
              PDF_file = "ENN_Loc3_R3",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6)) 

load(file = ENNs[8])
plot_envelope(envelope_data = enn$short, 
              metric = lsm_p_enn, 
              R = 4, 
              intensity_index = c(7,9), 
              log = TRUE, 
              bandwidth = 0.2, 
              xlim = c(0, 12), 
              ylim = c(0, 0.3),
              xlab = "", 
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(3,5), 
              Kernel = "gaussian", 
              plot_legend = FALSE, 
              PDF_file = "ENN_Loc3_R4",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))  

load(file = ENNs[9])
plot_envelope(envelope_data = enn$short, 
              metric = lsm_p_enn, 
              R = 5, 
              intensity_index = c(6), 
              log = TRUE, 
              bandwidth = 0.2, 
              xlim = c(0, 12), 
              ylim = c(0, 0.3),
              xlab = "", 
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(3,5), 
              Kernel = "gaussian", 
              plot_legend = FALSE, 
              PDF_file = "ENN_Loc3_R5",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))  

load(file = p_areas[7])
plot_envelope(envelope_data = p_area$short, 
              metric = lsm_p_area, 
              R = 3, 
              intensity_index = c(16, 17), 
              log = TRUE, 
              bandwidth = 0.4, 
              xlim = c(0, 0.2), 
              ylim = c(0,10),
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(1,1), 
              Kernel = "gaussian", 
              plot_legend = FALSE,
              xlim_plot = c(0, 0.08), 
              PDF_file = "gap_area_Loc3_R3",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))

load(file = p_areas[8])
plot_envelope(envelope_data = p_area$short, 
              metric = lsm_p_area, 
              R = 4, 
              intensity_index = c(11,12), 
              log = TRUE, 
              bandwidth = 0.4, 
              xlim = c(0, 0.2), 
              ylim = c(0,10),
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(1,1), 
              Kernel = "gaussian", 
              plot_legend = FALSE,
              xlim_plot = c(0, 0.08), 
              PDF_file = "gap_area_Loc3_R4",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))

load(file = p_areas[9])
plot_envelope(envelope_data = p_area$short, 
              metric = lsm_p_area, 
              R = 5, 
              intensity_index = c(8, 9), 
              log = TRUE, 
              bandwidth = 0.4, 
              xlim = c(0, 0.2), 
              ylim = c(0,10),
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(1,1), 
              Kernel = "gaussian", 
              plot_legend = FALSE,
              xlim_plot = c(0, 0.08), 
              PDF_file = "gap_area_Loc3_R5",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))

# Area 4 =======================================================================================================================================================

load(file = ENNs[10])
plot_envelope(envelope_data = enn$short, 
              metric = lsm_p_enn, 
              R = 3, 
              intensity_index = c(15, 19), 
              log = TRUE, 
              bandwidth = 0.2,
              xlim = c(0, 12), 
              ylim = c(0,0.3),
              xlab = "", 
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(1,1), 
              Kernel = "gaussian", 
              plot_legend = FALSE, 
              PDF_file = "ENN_Loc4_R3",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6)) 

load(file = ENNs[11])
plot_envelope(envelope_data = enn$short, 
              metric = lsm_p_enn, 
              R = 4, 
              intensity_index = c(6,9), 
              log = TRUE, 
              bandwidth = 0.2, 
              xlim = c(0, 12), 
              ylim = c(0, 0.3),
              xlab = "", 
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(3,5), 
              Kernel = "gaussian", 
              plot_legend = FALSE, 
              PDF_file = "ENN_Loc4_R4",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))  

load(file = ENNs[12])
plot_envelope(envelope_data = enn$short, 
              metric = lsm_p_enn, 
              R = 5, 
              intensity_index = c(6), 
              log = TRUE, 
              bandwidth = 0.2, 
              xlim = c(0, 12), 
              ylim = c(0, 0.3),
              xlab = "", 
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(3,5), 
              Kernel = "gaussian", 
              plot_legend = FALSE, 
              PDF_file = "ENN_Loc4_R5",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))  

load(file = p_areas[10])
plot_envelope(envelope_data = p_area$short, 
              metric = lsm_p_area, 
              R = 3, 
              intensity_index = c(15, 17), 
              log = TRUE, 
              bandwidth = 0.4, 
              xlim = c(0, 0.2), 
              ylim = c(0,10),
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(1,1), 
              Kernel = "gaussian", 
              plot_legend = FALSE,
              xlim_plot = c(0, 0.08), 
              PDF_file = "gap_area_Loc4_R3",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))

load(file = p_areas[11])
plot_envelope(envelope_data = p_area$short, 
              metric = lsm_p_area, 
              R = 4, 
              intensity_index = c(11,12), 
              log = TRUE, 
              bandwidth = 0.4, 
              xlim = c(0, 0.2), 
              ylim = c(0,10),
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(1,1), 
              Kernel = "gaussian", 
              plot_legend = FALSE,
              xlim_plot = c(0, 0.08), 
              PDF_file = "gap_area_Loc4_R4",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))

load(file = p_areas[12])
plot_envelope(envelope_data = p_area$short, 
              metric = lsm_p_area, 
              R = 5, 
              intensity_index = c(8, 9), 
              log = TRUE, 
              bandwidth = 0.4, 
              xlim = c(0, 0.2), 
              ylim = c(0,10),
              data_2006 = data__Loc1_2006, 
              data_2015 = data__Loc1_2015, 
              mfrow = c(1,1), 
              Kernel = "gaussian", 
              plot_legend = FALSE,
              xlim_plot = c(0, 0.08), 
              PDF_file = "gap_area_Loc4_R5",
              pdf_size = c(6,6),
              MAR = c(2.1, 3.9, 1.6, 1.6))
