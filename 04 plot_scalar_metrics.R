# === Load packages ============================================================================================================================================

.libPaths("C:/win-library/")

library(spatstat)
library(dplyr)
library(landscapemetrics)
library(lidR, lib.loc = "\\\\storage-ume.slu.se/home$/nsha0003/My Documents/R/win-library/4.1")

source("lanscape_metrics_simulation_function.R")

set.seed(12345)

# === Area 1 ===================================================================================================================================================

data__2006 <- data__Loc1_2006
data__2015 <- data__Loc1_2015
loc_name <- "Area 1"
load(file = "Metrics_Loc1.Rdata")

metrics2 <- metrics %>% filter(R < 5 | (R == 5 & density < 850) | (R == 6 & density < 600) | (R == 7 & density < 450)) %>% filter(R < 6 & R > 2)

metrics3 <- metrics2 %>% group_by(R, density) %>% summarise(
  mean_lsm_c_pland = mean(lsm_c_pland),
  mean_lsm_c_area_mn = mean(lsm_c_area_mn),
  mean_lsm_l_np = mean(lsm_l_np),
  mean_lsm_c_clumpy = mean(lsm_c_clumpy),
  mean_lsm_c_nlsi = mean(lsm_c_nlsi),
  mean_lsm_l_division = mean(lsm_l_division),
  mean_lsm_c_frac_mn = mean(lsm_c_frac_mn),
  
  q1_lsm_c_pland = quantile(lsm_c_pland, 0.025),
  q1_lsm_c_area_mn = quantile(lsm_c_area_mn, 0.025),
  q1_lsm_l_np = quantile(lsm_l_np, 0.025),
  q1_lsm_c_clumpy = quantile(lsm_c_clumpy, 0.025),
  q1_lsm_c_nlsi = quantile(lsm_c_nlsi, 0.025),
  q1_lsm_l_division = quantile(lsm_l_division, 0.025),
  q1_lsm_c_frac_mn = quantile(lsm_c_frac_mn, 0.025),
  
  q2_lsm_c_pland = quantile(lsm_c_pland, 0.975),
  q2_lsm_c_area_mn = quantile(lsm_c_area_mn, 0.975),
  q2_lsm_l_np = quantile(lsm_l_np, 0.975),
  q2_lsm_c_clumpy = quantile(lsm_c_clumpy, 0.975),
  q2_lsm_c_nlsi = quantile(lsm_c_nlsi, 0.975),
  q2_lsm_l_division = quantile(lsm_l_division, 0.975),
  q2_lsm_c_frac_mn = quantile(lsm_c_frac_mn, 0.975)
)


plot_CI(mean_var = metrics3$mean_lsm_c_pland, 
        q1_var = metrics3$q1_lsm_c_pland, 
        q2_var = metrics3$q2_lsm_c_pland, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_c_pland(data__2015)$value[lsm_c_pland(data__2015)$class == 0], 
        value_2006 = lsm_c_pland(data__2006)$value[lsm_c_pland(data__2006)$class == 0],
        header = NULL, #paste(loc_name, "Gap Area Proportion", sep = ": "),
        n_lim = c(50,1200),
        Ylim = c(5,20),
        set_par = FALSE, 
        plot_legend = FALSE, 
        PDF_file = "PLAND_Loc1",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
) 

plot_CI(mean_var = (metrics3$mean_lsm_c_area_mn * 10000),
        q1_var = (metrics3$q1_lsm_c_area_mn * 10000), 
        q2_var = (metrics3$q2_lsm_c_area_mn * 10000), 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = (lsm_c_area_mn(data__2015)$value[lsm_c_area_mn(data__2015)$class == 0] * 10000), 
        value_2006 = (lsm_c_area_mn(data__2006)$value[lsm_c_area_mn(data__2006)$class == 0] * 10000),
        header = NULL, # paste(loc_name, "Mean Gap Area", sep = ": "),
        n_lim = c(50,1200), Ylim = c(7, 27.3),
        set_par = FALSE, 
        plot_legend = FALSE, 
        PDF_file = "AREA_Loc1",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

plot_CI(mean_var = metrics3$mean_lsm_c_clumpy, 
        q1_var = metrics3$q1_lsm_c_clumpy, 
        q2_var = metrics3$q2_lsm_c_clumpy, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_c_clumpy(data__2015)$value[lsm_c_clumpy(data__2015)$class == 0], 
        value_2006 = lsm_c_clumpy(data__2006)$value[lsm_c_clumpy(data__2006)$class == 0],
        header = NULL, #paste(loc_name, "Clumpiness Index (of Gaps)", sep = ": "),
        n_lim = c(50,1200), 
        Ylim = c(0.25, 0.4),
        set_par = FALSE,
        plot_value = FALSE, 
        plot_dashed = TRUE, 
        plot_legend = FALSE, 
        PDF_file = "CLUMPY_Loc1",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

plot_CI(mean_var = metrics3$mean_lsm_c_nlsi, 
        q1_var = metrics3$q1_lsm_c_nlsi, 
        q2_var = metrics3$q2_lsm_c_nlsi, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_c_nlsi(data__2015)$value[lsm_c_nlsi(data__2015)$class == 0], 
        value_2006 = lsm_c_nlsi(data__2006)$value[lsm_c_nlsi(data__2006)$class == 0],
        header = NULL, #paste(loc_name, "nLSI", sep = ": "),
        n_lim = c(50,1200), 
        Ylim = c(0.5, 0.7),
        set_par = FALSE,
        plot_legend = FALSE,  
        PDF_file = "nLSI_Loc1",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

plot_CI(mean_var = metrics3$mean_lsm_l_division, 
        q1_var = metrics3$q1_lsm_l_division, 
        q2_var = metrics3$q2_lsm_l_division, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_l_division(data__2015)$value, 
        value_2006 = lsm_l_division(data__2006)$value,
        header = NULL, #paste(loc_name, "Landscape division index", sep = ": "),
        n_lim = c(50,1200),
        Ylim = c(0.05, 0.35),
        set_par = FALSE,
        plot_legend = FALSE,  
        PDF_file = "DIVISION_Loc1",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

plot_CI(mean_var = metrics3$mean_lsm_c_frac_mn, 
        q1_var = metrics3$q1_lsm_c_frac_mn, 
        q2_var = metrics3$q2_lsm_c_frac_mn, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_c_frac_mn(data__2015)$value[lsm_c_frac_mn(data__2015)$class == 0], 
        value_2006 = lsm_c_frac_mn(data__2006)$value[lsm_c_frac_mn(data__2006)$class == 0],
        header = NULL, #paste(loc_name, "Mean fractal dimension index", sep = ": "),
        n_lim = c(50,1200), 
        Ylim = c(1.05, 1.16),
        set_par = FALSE, 
        plot_legend = FALSE,  
        PDF_file = "FRAC_Loc1",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

# === Area 2 ===================================================================================================================================================

data__2006 <- data__Loc2_2006
data__2015 <- data__Loc2_2015
loc_name <- "Area 2"
load(file = "Metrics_Loc2.Rdata")

metrics2 <- metrics %>% filter(R < 5 | (R == 5 & density < 850) | (R == 6 & density < 600) | (R == 7 & density < 450)) %>% filter(R < 6 & R > 2)

metrics3 <- metrics2 %>% group_by(R, density) %>% summarise(
  mean_lsm_c_pland = mean(lsm_c_pland),
  mean_lsm_c_area_mn = mean(lsm_c_area_mn),
  mean_lsm_l_np = mean(lsm_l_np),
  mean_lsm_c_clumpy = mean(lsm_c_clumpy),
  mean_lsm_c_nlsi = mean(lsm_c_nlsi),
  mean_lsm_l_division = mean(lsm_l_division),
  mean_lsm_c_frac_mn = mean(lsm_c_frac_mn),
  
  q1_lsm_c_pland = quantile(lsm_c_pland, 0.025),
  q1_lsm_c_area_mn = quantile(lsm_c_area_mn, 0.025),
  q1_lsm_l_np = quantile(lsm_l_np, 0.025),
  q1_lsm_c_clumpy = quantile(lsm_c_clumpy, 0.025),
  q1_lsm_c_nlsi = quantile(lsm_c_nlsi, 0.025),
  q1_lsm_l_division = quantile(lsm_l_division, 0.025),
  q1_lsm_c_frac_mn = quantile(lsm_c_frac_mn, 0.025),
  
  q2_lsm_c_pland = quantile(lsm_c_pland, 0.975),
  q2_lsm_c_area_mn = quantile(lsm_c_area_mn, 0.975),
  q2_lsm_l_np = quantile(lsm_l_np, 0.975),
  q2_lsm_c_clumpy = quantile(lsm_c_clumpy, 0.975),
  q2_lsm_c_nlsi = quantile(lsm_c_nlsi, 0.975),
  q2_lsm_l_division = quantile(lsm_l_division, 0.975),
  q2_lsm_c_frac_mn = quantile(lsm_c_frac_mn, 0.975)
)

par(mfrow = c(1,3))
plot_CI(mean_var = metrics3$mean_lsm_c_pland, 
        q1_var = metrics3$q1_lsm_c_pland, 
        q2_var = metrics3$q2_lsm_c_pland, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_c_pland(data__2015)$value[lsm_c_pland(data__2015)$class == 0], 
        value_2006 = lsm_c_pland(data__2006)$value[lsm_c_pland(data__2006)$class == 0],
        header = NULL, #paste(loc_name, "Gap Area Proportion", sep = ": "),
        n_lim = c(50,1200),
        Ylim = c(5, 20),
        set_par = FALSE, 
        plot_legend = FALSE, 
        PDF_file = "PLAND_Loc2",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
) 

plot_CI(mean_var = (metrics3$mean_lsm_c_area_mn * 10000),
        q1_var = (metrics3$q1_lsm_c_area_mn * 10000), 
        q2_var = (metrics3$q2_lsm_c_area_mn * 10000), 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = (lsm_c_area_mn(data__2015)$value[lsm_c_area_mn(data__2015)$class == 0] * 10000), 
        value_2006 = (lsm_c_area_mn(data__2006)$value[lsm_c_area_mn(data__2006)$class == 0] * 10000),
        header = NULL, # paste(loc_name, "Mean Gap Area", sep = ": "),
        n_lim = c(50,1200), Ylim = c(7, 27.3),
        set_par = FALSE, 
        plot_legend = FALSE, 
        PDF_file = "AREA_Loc2",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

plot_CI(mean_var = metrics3$mean_lsm_c_clumpy, 
        q1_var = metrics3$q1_lsm_c_clumpy, 
        q2_var = metrics3$q2_lsm_c_clumpy, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_c_clumpy(data__2015)$value[lsm_c_clumpy(data__2015)$class == 0], 
        value_2006 = lsm_c_clumpy(data__2006)$value[lsm_c_clumpy(data__2006)$class == 0],
        header = NULL, #paste(loc_name, "Clumpiness Index (of Gaps)", sep = ": "),
        n_lim = c(50,1200), 
        Ylim = c(0.12, 0.28),
        set_par = FALSE,
        # plot_value = TRUE, 
        # plot_dashed = FALSE, 
        plot_legend = FALSE, 
        PDF_file = "CLUMPY_Loc2",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

plot_CI(mean_var = metrics3$mean_lsm_c_nlsi, 
        q1_var = metrics3$q1_lsm_c_nlsi, 
        q2_var = metrics3$q2_lsm_c_nlsi, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_c_nlsi(data__2015)$value[lsm_c_nlsi(data__2015)$class == 0], 
        value_2006 = lsm_c_nlsi(data__2006)$value[lsm_c_nlsi(data__2006)$class == 0],
        header = NULL, #paste(loc_name, "nLSI", sep = ": "),
        n_lim = c(50,1200), 
        Ylim = c(0.65, 0.8),
        set_par = FALSE,
        plot_legend = FALSE,  
        PDF_file = "nLSI_Loc2",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

plot_CI(mean_var = metrics3$mean_lsm_l_division, 
        q1_var = metrics3$q1_lsm_l_division, 
        q2_var = metrics3$q2_lsm_l_division, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_l_division(data__2015)$value, 
        value_2006 = lsm_l_division(data__2006)$value,
        header = NULL, #paste(loc_name, "Landscape division index", sep = ": "),
        n_lim = c(50,1200),
        Ylim = c(0.05, 0.35),
        set_par = FALSE,
        plot_legend = FALSE,  
        PDF_file = "DIVISION_Loc2",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

plot_CI(mean_var = metrics3$mean_lsm_c_frac_mn, 
        q1_var = metrics3$q1_lsm_c_frac_mn, 
        q2_var = metrics3$q2_lsm_c_frac_mn, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_c_frac_mn(data__2015)$value[lsm_c_frac_mn(data__2015)$class == 0], 
        value_2006 = lsm_c_frac_mn(data__2006)$value[lsm_c_frac_mn(data__2006)$class == 0],
        header = NULL, #paste(loc_name, "Mean fractal dimension index", sep = ": "),
        n_lim = c(50,1200), 
        Ylim = c(1.05, 1.161),
        set_par = FALSE, 
        plot_legend = FALSE,  
        PDF_file = "FRAC_Loc2",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

# === Area 3 ===================================================================================================================================================

data__2006 <- data__Loc3_2006
data__2015 <- data__Loc3_2015
loc_name <- "Area 3"
load(file = "Metrics_Loc3.Rdata")

metrics2 <- metrics %>% filter(R < 5 | (R == 5 & density < 850) | (R == 6 & density < 600) | (R == 7 & density < 450)) %>% filter(R < 6 & R > 2)

metrics3 <- metrics2 %>% group_by(R, density) %>% summarise(
  mean_lsm_c_pland = mean(lsm_c_pland),
  mean_lsm_c_area_mn = mean(lsm_c_area_mn),
  mean_lsm_l_np = mean(lsm_l_np),
  mean_lsm_c_clumpy = mean(lsm_c_clumpy),
  mean_lsm_c_nlsi = mean(lsm_c_nlsi),
  mean_lsm_l_division = mean(lsm_l_division),
  mean_lsm_c_frac_mn = mean(lsm_c_frac_mn),
  
  q1_lsm_c_pland = quantile(lsm_c_pland, 0.025),
  q1_lsm_c_area_mn = quantile(lsm_c_area_mn, 0.025),
  q1_lsm_l_np = quantile(lsm_l_np, 0.025),
  q1_lsm_c_clumpy = quantile(lsm_c_clumpy, 0.025),
  q1_lsm_c_nlsi = quantile(lsm_c_nlsi, 0.025),
  q1_lsm_l_division = quantile(lsm_l_division, 0.025),
  q1_lsm_c_frac_mn = quantile(lsm_c_frac_mn, 0.025),
  
  q2_lsm_c_pland = quantile(lsm_c_pland, 0.975),
  q2_lsm_c_area_mn = quantile(lsm_c_area_mn, 0.975),
  q2_lsm_l_np = quantile(lsm_l_np, 0.975),
  q2_lsm_c_clumpy = quantile(lsm_c_clumpy, 0.975),
  q2_lsm_c_nlsi = quantile(lsm_c_nlsi, 0.975),
  q2_lsm_l_division = quantile(lsm_l_division, 0.975),
  q2_lsm_c_frac_mn = quantile(lsm_c_frac_mn, 0.975)
)

plot_CI(mean_var = metrics3$mean_lsm_c_pland, 
        q1_var = metrics3$q1_lsm_c_pland, 
        q2_var = metrics3$q2_lsm_c_pland, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_c_pland(data__2015)$value[lsm_c_pland(data__2015)$class == 0], 
        value_2006 = lsm_c_pland(data__2006)$value[lsm_c_pland(data__2006)$class == 0],
        header = NULL, #paste(loc_name, "Gap Area Proportion", sep = ": "),
        n_lim = c(50,1200),
        Ylim = c(5, 20),
        set_par = FALSE, 
        plot_legend = FALSE, 
        PDF_file = "PLAND_Loc3",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
) 

plot_CI(mean_var = (metrics3$mean_lsm_c_area_mn * 10000),
        q1_var = (metrics3$q1_lsm_c_area_mn * 10000), 
        q2_var = (metrics3$q2_lsm_c_area_mn * 10000), 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = (lsm_c_area_mn(data__2015)$value[lsm_c_area_mn(data__2015)$class == 0] * 10000), 
        value_2006 = (lsm_c_area_mn(data__2006)$value[lsm_c_area_mn(data__2006)$class == 0] * 10000),
        header = NULL, # paste(loc_name, "Mean Gap Area", sep = ": "),
        n_lim = c(50,1200), Ylim = c(7, 27.3),
        set_par = FALSE, 
        plot_legend = FALSE, 
        PDF_file = "AREA_Loc3",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

plot_CI(mean_var = metrics3$mean_lsm_c_clumpy, 
        q1_var = metrics3$q1_lsm_c_clumpy, 
        q2_var = metrics3$q2_lsm_c_clumpy, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_c_clumpy(data__2015)$value[lsm_c_clumpy(data__2015)$class == 0], 
        value_2006 = lsm_c_clumpy(data__2006)$value[lsm_c_clumpy(data__2006)$class == 0],
        header = NULL, #paste(loc_name, "Clumpiness Index (of Gaps)", sep = ": "),
        n_lim = c(50,1200), 
        Ylim = c(0.15, 0.36),
        set_par = FALSE,
        # plot_value = FALSE, 
        # plot_dashed = TRUE, 
        plot_legend = FALSE, 
        PDF_file = "CLUMPY_Loc3",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

plot_CI(mean_var = metrics3$mean_lsm_c_nlsi, 
        q1_var = metrics3$q1_lsm_c_nlsi, 
        q2_var = metrics3$q2_lsm_c_nlsi, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_c_nlsi(data__2015)$value[lsm_c_nlsi(data__2015)$class == 0], 
        value_2006 = lsm_c_nlsi(data__2006)$value[lsm_c_nlsi(data__2006)$class == 0],
        header = NULL, #paste(loc_name, "nLSI", sep = ": "),
        n_lim = c(50,1200), 
        Ylim = c(0.57, 0.7),
        set_par = FALSE,
        plot_legend = FALSE,  
        PDF_file = "nLSI_Loc3",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

plot_CI(mean_var = metrics3$mean_lsm_l_division, 
        q1_var = metrics3$q1_lsm_l_division, 
        q2_var = metrics3$q2_lsm_l_division, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_l_division(data__2015)$value, 
        value_2006 = lsm_l_division(data__2006)$value,
        header = NULL, #paste(loc_name, "Landscape division index", sep = ": "),
        n_lim = c(50,1200),
        Ylim = c(0.05, 0.35),
        set_par = FALSE,
        plot_legend = FALSE,  
        PDF_file = "DIVISION_Loc3",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

plot_CI(mean_var = metrics3$mean_lsm_c_frac_mn, 
        q1_var = metrics3$q1_lsm_c_frac_mn, 
        q2_var = metrics3$q2_lsm_c_frac_mn, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_c_frac_mn(data__2015)$value[lsm_c_frac_mn(data__2015)$class == 0], 
        value_2006 = lsm_c_frac_mn(data__2006)$value[lsm_c_frac_mn(data__2006)$class == 0],
        header = NULL, #paste(loc_name, "Mean fractal dimension index", sep = ": "),
        n_lim = c(50,1200), 
        Ylim = c(1.05, 1.16),
        set_par = FALSE, 
        plot_legend = FALSE,  
        PDF_file = "FRAC_Loc3",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

# === Area 4 ===================================================================================================================================================

data__2006 <- data__Loc4_2006
data__2015 <- data__Loc4_2015
loc_name <- "Area 4"
load(file = "Metrics_Loc4.Rdata")

metrics2 <- metrics %>% filter(R < 5 | (R == 5 & density < 850) | (R == 6 & density < 600) | (R == 7 & density < 450)) %>% filter(R < 6 & R > 2)

metrics3 <- metrics2 %>% group_by(R, density) %>% summarise(
  mean_lsm_c_pland = mean(lsm_c_pland),
  mean_lsm_c_area_mn = mean(lsm_c_area_mn),
  mean_lsm_l_np = mean(lsm_l_np),
  mean_lsm_c_clumpy = mean(lsm_c_clumpy),
  mean_lsm_c_nlsi = mean(lsm_c_nlsi),
  mean_lsm_l_division = mean(lsm_l_division),
  mean_lsm_c_frac_mn = mean(lsm_c_frac_mn),
  
  q1_lsm_c_pland = quantile(lsm_c_pland, 0.025),
  q1_lsm_c_area_mn = quantile(lsm_c_area_mn, 0.025),
  q1_lsm_l_np = quantile(lsm_l_np, 0.025),
  q1_lsm_c_clumpy = quantile(lsm_c_clumpy, 0.025),
  q1_lsm_c_nlsi = quantile(lsm_c_nlsi, 0.025),
  q1_lsm_l_division = quantile(lsm_l_division, 0.025),
  q1_lsm_c_frac_mn = quantile(lsm_c_frac_mn, 0.025),
  
  q2_lsm_c_pland = quantile(lsm_c_pland, 0.975),
  q2_lsm_c_area_mn = quantile(lsm_c_area_mn, 0.975),
  q2_lsm_l_np = quantile(lsm_l_np, 0.975),
  q2_lsm_c_clumpy = quantile(lsm_c_clumpy, 0.975),
  q2_lsm_c_nlsi = quantile(lsm_c_nlsi, 0.975),
  q2_lsm_l_division = quantile(lsm_l_division, 0.975),
  q2_lsm_c_frac_mn = quantile(lsm_c_frac_mn, 0.975)
)

plot_CI(mean_var = metrics3$mean_lsm_c_pland, 
        q1_var = metrics3$q1_lsm_c_pland, 
        q2_var = metrics3$q2_lsm_c_pland, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_c_pland(data__2015)$value[lsm_c_pland(data__2015)$class == 0], 
        value_2006 = lsm_c_pland(data__2006)$value[lsm_c_pland(data__2006)$class == 0],
        header = NULL, #paste(loc_name, "Gap Area Proportion", sep = ": "),
        n_lim = c(50,1200),
        Ylim = c(5, 20),
        set_par = FALSE, 
        plot_legend = FALSE, 
        PDF_file = "PLAND_Loc4",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
) 

plot_CI(mean_var = (metrics3$mean_lsm_c_area_mn * 10000),
        q1_var = (metrics3$q1_lsm_c_area_mn * 10000), 
        q2_var = (metrics3$q2_lsm_c_area_mn * 10000), 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = (lsm_c_area_mn(data__2015)$value[lsm_c_area_mn(data__2015)$class == 0] * 10000), 
        value_2006 = (lsm_c_area_mn(data__2006)$value[lsm_c_area_mn(data__2006)$class == 0] * 10000),
        header = NULL, # paste(loc_name, "Mean Gap Area", sep = ": "),
        n_lim = c(50,1200), Ylim = c(7, 27.3),
        set_par = FALSE, 
        plot_legend = FALSE, 
        PDF_file = "AREA_Loc4",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

plot_CI(mean_var = metrics3$mean_lsm_c_clumpy, 
        q1_var = metrics3$q1_lsm_c_clumpy, 
        q2_var = metrics3$q2_lsm_c_clumpy, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_c_clumpy(data__2015)$value[lsm_c_clumpy(data__2015)$class == 0], 
        value_2006 = lsm_c_clumpy(data__2006)$value[lsm_c_clumpy(data__2006)$class == 0],
        header = NULL, #paste(loc_name, "Clumpiness Index (of Gaps)", sep = ": "),
        n_lim = c(50,1200), 
        Ylim = c(0.12, 0.3),
        set_par = FALSE,
        # plot_value = FALSE, 
        # plot_dashed = TRUE, 
        plot_legend = FALSE, 
        PDF_file = "CLUMPY_Loc4",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

plot_CI(mean_var = metrics3$mean_lsm_c_nlsi, 
        q1_var = metrics3$q1_lsm_c_nlsi, 
        q2_var = metrics3$q2_lsm_c_nlsi, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_c_nlsi(data__2015)$value[lsm_c_nlsi(data__2015)$class == 0], 
        value_2006 = lsm_c_nlsi(data__2006)$value[lsm_c_nlsi(data__2006)$class == 0],
        header = NULL, #paste(loc_name, "nLSI", sep = ": "),
        n_lim = c(50,1200), 
        Ylim = c(0.7, 0.8),
        set_par = FALSE,
        plot_legend = FALSE,  
        PDF_file = "nLSI_Loc4",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

plot_CI(mean_var = metrics3$mean_lsm_l_division, 
        q1_var = metrics3$q1_lsm_l_division, 
        q2_var = metrics3$q2_lsm_l_division, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_l_division(data__2015)$value, 
        value_2006 = lsm_l_division(data__2006)$value,
        header = NULL, #paste(loc_name, "Landscape division index", sep = ": "),
        n_lim = c(50,1200),
        Ylim = c(0.05, 0.35),
        set_par = FALSE,
        plot_legend = FALSE,  
        PDF_file = "DIVISION_Loc4",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)

plot_CI(mean_var = metrics3$mean_lsm_c_frac_mn, 
        q1_var = metrics3$q1_lsm_c_frac_mn, 
        q2_var = metrics3$q2_lsm_c_frac_mn, 
        n_var = metrics3$density, 
        R_var = metrics3$R, 
        value_2015 = lsm_c_frac_mn(data__2015)$value[lsm_c_frac_mn(data__2015)$class == 0], 
        value_2006 = lsm_c_frac_mn(data__2006)$value[lsm_c_frac_mn(data__2006)$class == 0],
        header = NULL, #paste(loc_name, "Mean fractal dimension index", sep = ": "),
        n_lim = c(50,1200), 
        Ylim = c(1.05, 1.16),
        set_par = FALSE, 
        plot_legend = FALSE,  
        PDF_file = "FRAC_Loc4",
        pdf_size = c(6,6),
        MAR = c(2.1, 3.9, 1.6, 1.6)
)
