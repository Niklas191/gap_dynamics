# === Load packages =========================================================================================================================================================

.libPaths("C:/win-library/")

library(spatstat)
library(dplyr)
library(logKDE) # function logdensity()
library(landscapemetrics)
library(lidR, lib.loc = "\\\\storage-ume.slu.se/home$/nsha0003/My Documents/R/win-library/4.1")
library(GET)

source("lanscape_metrics_simulation_function.R")

set.seed(12345)

# === Load data (ENN) ==========================================================================================================================================

load(file = "enn_Loc1_1.Rdata")
ENN_Loc1_1 <- enn
load(file = "enn_Loc1_2.Rdata")
ENN_Loc1_2 <- enn

load(file = "ENN_Loc2_1.Rdata")
ENN_Loc2_1 <- enn
load(file = "ENN_Loc2_2.Rdata")
ENN_Loc2_2 <- enn
load(file = "ENN_Loc2_3.Rdata")

ENN_Loc2_3 <- enn
load(file = "ENN_Loc3_1.Rdata")
ENN_Loc3_1 <- enn
load(file = "ENN_Loc3_2.Rdata")
ENN_Loc3_2 <- enn

load(file = "ENN_Loc4_1.Rdata")
ENN_Loc4_1 <- enn
load(file = "ENN_Loc4_2.Rdata")
ENN_Loc4_2 <- enn

# === Load data (AREA) =========================================================================================================================================

load(file = "gap_area_Loc1_1.Rdata")
area_Loc1_1 <- p_area
load(file = "gap_area_Loc1_2.Rdata")
area_Loc1_2 <- p_area

load(file = "gap_area_Loc2_1.Rdata")
area_Loc2_1 <- p_area
load(file = "gap_area_Loc2_2.Rdata")
area_Loc2_2 <- p_area

load(file = "gap_area_Loc3_1.Rdata")
area_Loc3_1 <- p_area
load(file = "gap_area_Loc3_2.Rdata")
area_Loc3_2 <- p_area

load(file = "gap_area_Loc4_1.Rdata")
area_Loc4_1 <- p_area
load(file = "gap_area_Loc4_2.Rdata")
area_Loc4_2 <- p_area

# GET: Location 1 ==============================================================================================================================================

obs2006 <- data__Loc1_2006 %>% 
  lsm_p_enn() %>% 
  filter(class == 0) %>% 
  .$value %>%  
  round(5) %>% 
  logdensity(., 
             kernel = "gaussian", 
             bw = 0.2, 
             from =0,
             to = 12) %>% 
  .$y

obs2006 <- obs2006[2:length(obs2006)] 

obs2015 <- data__Loc1_2015 %>% 
  lsm_p_enn() %>% 
  filter(class == 0) %>% 
  .$value %>%  
  round(5) %>% 
  logdensity(., 
             kernel = "gaussian", 
             bw = 0.2, 
             from =0, 
             to = 12) %>% 
  .$y
obs2015 <- obs2015[2:length(obs2015)] 

p_matrix_Loc1_2006 <- p_matrix_GET(Rs = c(3,4), 
                                   ns = seq(300,1000,50),
                                   outputs = list(ENN_Loc1_1$long, ENN_Loc1_2$long), 
                                   obs = obs2006)

p_matrix_Loc1_2015 <- p_matrix_GET(Rs = c(3,4), 
                                   ns = seq(300,1000,50),
                                   outputs = list(ENN_Loc1_1$long, ENN_Loc1_2$long), 
                                   obs = obs2015)


obs2006 <- data__Loc1_2006 %>% 
  lsm_p_area() %>% 
  filter(class == 0) %>% 
  .$value %>%  
  round(5) %>% 
  logdensity(., 
             kernel = "gaussian", 
             bw = 0.4, 
             from =0, 
             to = 0.02) %>% 
  .$y

obs2006 <- obs2006[2:length(obs2006)] 

obs2015 <- data__Loc1_2015 %>% 
  lsm_p_area() %>% 
  filter(class == 0) %>% 
  .$value %>%  
  round(5) %>% 
  logdensity(., 
             kernel = "gaussian", 
             bw = 0.4, 
             from =0, 
             to = 0.02) %>% 
  .$y
obs2015 <- obs2015[2:length(obs2015)] 

a_matrix_Loc1_2006 <- p_matrix_GET(Rs = c(3,4), 
                                   ns = seq(300,1000,50),
                                   outputs = list(area_Loc1_1$long, area_Loc1_2$long), 
                                   obs = obs2006)

a_matrix_Loc1_2015 <- p_matrix_GET(Rs = c(3,4), 
                                   ns = seq(300,1000,50),
                                   outputs = list(area_Loc1_1$long, area_Loc1_2$long), 
                                   obs = obs2015)


# GET: Location 2 ==============================================================================================================================================

obs2006 <- data__Loc2_2006 %>% 
  lsm_p_enn() %>% 
  filter(class == 0) %>% 
  .$value %>%  
  round(5) %>% 
  logdensity(., 
             kernel = "gaussian", 
             bw = 0.2, 
             from =0,
             to = 12) %>% 
  .$y

obs2006 <- obs2006[2:length(obs2006)] 

obs2015 <- data__Loc2_2015 %>% 
  lsm_p_enn() %>% 
  filter(class == 0) %>% 
  .$value %>%  
  round(5) %>% 
  logdensity(., 
             kernel = "gaussian", 
             bw = 0.2, 
             from =0, 
             to = 12) %>% 
  .$y
obs2015 <- obs2015[2:length(obs2015)] 

p_matrix_Loc2_2006 <- p_matrix_GET(Rs = c(3,4), 
                                   ns = seq(300,1000,50),
                                   outputs = list(ENN_Loc2_1$long, ENN_Loc2_2$long), 
                                   obs = obs2006)

p_matrix_Loc2_2015 <- p_matrix_GET(Rs = c(3,4), 
                                   ns = seq(300,1000,50),
                                   outputs = list(ENN_Loc2_1$long, ENN_Loc2_2$long), 
                                   obs = obs2015)


obs2006 <- data__Loc2_2006 %>% 
  lsm_p_area() %>% 
  filter(class == 0) %>% 
  .$value %>%  
  round(5) %>% 
  logdensity(., 
             kernel = "gaussian", 
             bw = 0.4, 
             from =0, 
             to = 0.02) %>% 
  .$y

obs2006 <- obs2006[2:length(obs2006)] 

obs2015 <- data__Loc2_2015 %>% 
  lsm_p_area() %>% 
  filter(class == 0) %>% 
  .$value %>%  
  round(5) %>% 
  logdensity(., 
             kernel = "gaussian", 
             bw = 0.4, 
             from =0, 
             to = 0.02) %>% 
  .$y
obs2015 <- obs2015[2:length(obs2015)] 

a_matrix_Loc2_2006 <- p_matrix_GET(Rs = c(3,4), 
                                   ns = seq(300,1000,50),
                                   outputs = list(area_Loc2_1$long, area_Loc2_2$long), 
                                   obs = obs2006)

a_matrix_Loc2_2015 <- p_matrix_GET(Rs = c(3,4), 
                                   ns = seq(300,1000,50),
                                   outputs = list(area_Loc2_1$long, area_Loc2_2$long), 
                                   obs = obs2015)


# GET: Location 3 ==============================================================================================================================================


obs2006 <- data__Loc3_2006 %>% 
  lsm_p_enn() %>% 
  filter(class == 0) %>% 
  .$value %>%  
  round(5) %>% 
  logdensity(., 
             kernel = "gaussian", 
             bw = 0.2, 
             from =0,
             to = 12) %>% 
  .$y

obs2006 <- obs2006[2:length(obs2006)] 

obs2015 <- data__Loc3_2015 %>% 
  lsm_p_enn() %>% 
  filter(class == 0) %>% 
  .$value %>%  
  round(5) %>% 
  logdensity(., 
             kernel = "gaussian", 
             bw = 0.2, 
             from =0, 
             to = 12) %>% 
  .$y
obs2015 <- obs2015[2:length(obs2015)] 

p_matrix_Loc3_2006 <- p_matrix_GET(Rs = c(3,4), 
                                   ns = seq(300,1000,50),
                                   outputs = list(ENN_Loc3_1$long, ENN_Loc3_2$long), 
                                   obs = obs2006)

p_matrix_Loc3_2015 <- p_matrix_GET(Rs = c(3,4), 
                                   ns = seq(300,1000,50),
                                   outputs = list(ENN_Loc3_1$long, ENN_Loc3_2$long), 
                                   obs = obs2015)


obs2006 <- data__Loc3_2006 %>% 
  lsm_p_area() %>% 
  filter(class == 0) %>% 
  .$value %>%  
  round(5) %>% 
  logdensity(., 
             kernel = "gaussian", 
             bw = 0.4, 
             from =0, 
             to = 0.02) %>% 
  .$y

obs2006 <- obs2006[2:length(obs2006)] 

obs2015 <- data__Loc3_2015 %>% 
  lsm_p_area() %>% 
  filter(class == 0) %>% 
  .$value %>%  
  round(5) %>% 
  logdensity(., 
             kernel = "gaussian", 
             bw = 0.4, 
             from =0, 
             to = 0.02) %>% 
  .$y
obs2015 <- obs2015[2:length(obs2015)] 

a_matrix_Loc3_2006 <- p_matrix_GET(Rs = c(3,4,5), 
                                   ns = seq(300,1000,50),
                                   outputs = list(area_Loc3_1$long, area_Loc3_2$long), 
                                   obs = obs2006)

a_matrix_Loc3_2015 <- p_matrix_GET(Rs = c(3,4,5), 
                                   ns = seq(300,1000,50),
                                   outputs = list(area_Loc3_1$long, area_Loc3_2$long), 
                                   obs = obs2015)

# GET: Location 4 ==============================================================================================================================================

obs2006 <- data__Loc4_2006 %>% 
  lsm_p_enn() %>% 
  filter(class == 0) %>% 
  .$value %>%  
  round(5) %>% 
  logdensity(., 
             kernel = "gaussian", 
             bw = 0.2, 
             from =0,
             to = 12) %>% 
  .$y

obs2006 <- obs2006[2:length(obs2006)] 

obs2015 <- data__Loc4_2015 %>% 
  lsm_p_enn() %>% 
  filter(class == 0) %>% 
  .$value %>%  
  round(5) %>% 
  logdensity(., 
             kernel = "gaussian", 
             bw = 0.2, 
             from =0, 
             to = 12) %>% 
  .$y
obs2015 <- obs2015[2:length(obs2015)] 

p_matrix_Loc4_2006 <- p_matrix_GET(Rs = c(3,4), 
                                   ns = seq(300,1000,50),
                                   outputs = list(ENN_Loc4_1$long, ENN_Loc4_2$long), 
                                   obs = obs2006)

p_matrix_Loc4_2015 <- p_matrix_GET(Rs = c(3,4), 
                                   ns = seq(300,1000,50),
                                   outputs = list(ENN_Loc4_1$long, ENN_Loc4_2$long), 
                                   obs = obs2015)


obs2006 <- data__Loc4_2006 %>% 
  lsm_p_area() %>% 
  filter(class == 0) %>% 
  .$value %>%  
  round(5) %>% 
  logdensity(., 
             kernel = "gaussian", 
             bw = 0.4, 
             from =0, 
             to = 0.02) %>% 
  .$y

obs2006 <- obs2006[2:length(obs2006)] 

obs2015 <- data__Loc4_2015 %>% 
  lsm_p_area() %>% 
  filter(class == 0) %>% 
  .$value %>%  
  round(5) %>% 
  logdensity(., 
             kernel = "gaussian", 
             bw = 0.4, 
             from =0, 
             to = 0.02) %>% 
  .$y
obs2015 <- obs2015[2:length(obs2015)] 

a_matrix_Loc4_2006 <- p_matrix_GET(Rs = c(3,4), 
                                   ns = seq(300,1000,50),
                                   outputs = list(area_Loc4_1$long, area_Loc4_2$long), 
                                   obs = obs2006)

a_matrix_Loc4_2015 <- p_matrix_GET(Rs = c(3,4), 
                                   ns = seq(300,1000,50),
                                   outputs = list(area_Loc4_1$long, area_Loc4_2$long), 
                                   obs = obs2015)


# Results ======================================================================================================================================================

p_matrix_Loc1_2006 %>% round(.,4) %>% print()
p_matrix_Loc2_2015 %>% round(.,4) %>% print()

p_matrix_Loc2_2006 %>% round(.,4) %>% print()
p_matrix_Loc2_2015 %>% round(.,4) %>% print()

p_matrix_Loc3_2006 %>% round(.,4) %>% print()
p_matrix_Loc3_2015 %>% round(.,4) %>% print()

p_matrix_Loc4_2006 %>% round(.,4) %>% print()
p_matrix_Loc4_2015 %>% round(.,4) %>% print()


a_matrix_Loc1_2006 %>% round(.,4) %>% print()
a_matrix_Loc2_2015 %>% round(.,4) %>% print()

a_matrix_Loc2_2006 %>% round(.,4) %>% print()
a_matrix_Loc2_2015 %>% round(.,4) %>% print()

a_matrix_Loc3_2006 %>% round(.,4) %>% print()
a_matrix_Loc3_2015 %>% round(.,4) %>% print()

a_matrix_Loc4_2006 %>% round(.,4) %>% print()
a_matrix_Loc4_2015 %>% round(.,4) %>% print()
