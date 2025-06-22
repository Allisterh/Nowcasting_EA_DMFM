# ==============================================================================
#               Dynamic Matrix Factor Models and the EM Algorithm: 
#          A Nowcasting Framework for Mixed Frequency Data in Euro Area
#                            - Descriptive Script - 
# ==============================================================================
# This script performs descriptive analyses to support a DMFM-based nowcasting 
# framework for GDP growth in the Euro Area. It provides visualizations of 
# GDP contributions of individual countries and an overview of country-specific
# coverage within the EA dataset.
#
# Main Objectives:
#   1. Compute the contribution of individual countries to the Euro Area GDP
#   2. Visualize the GDP breakdown for a given quarter
#
# ==============================================================================
# Author      : Davide Delfino
# Institution : Alma Mater Studiorum - University of Bologna
# Dataset     : - “EA-MD-QD” by M. Barigozzi and Lissona (2024)
#               - "namq_10_gdp" by Eurostat

# ==============================================================================
# Script Type : Descriptive / Visualization
# ==============================================================================

# ==============================================================================
# SET WORKING DIRECTORY
# ==============================================================================
path <- "C:/Users/david/Desktop/University/Master's Thesis/Nowcasting_EA_DMFM/DMFM"
setwd(path)

# ==============================================================================
# LOAD REQUIRED LIBRARIES
# ==============================================================================

# Data Manipulation
library(tidyverse)
library(lubridate)
library(abind)

# Time Series Analysis
library(tseries)
library(zoo)
library(fBasics)
library(vars)

# Linear Algebra & Matrix Operations
library(Matrix)
library(RSpectra)
library(MASS)
library(pracma)

# Input / Output
library(writexl)
library(readxl)
library(xtable)

# Visualization
library(ggplot2)
library(reshape2)
library(patchwork)

# ==============================================================================
# LOAD USER-DEFINED FUNCTIONS
# ==============================================================================
# GDP visualizations and contribution plots
source("functions/R/dmfm.visualization.R")

# Data preparation & transformation
source("functions/R/dmfm.preparation.R")
# ==============================================================================
# DEFINE PARAMETERS
# ==============================================================================

countries <- c("DE", "FR", "IT", "ES")  # Countries to include in the model

# Configuration parameters
P <- list(
  modelM = "Large",                     # Monthly model size
  modelQ = "Small",                     # Quarterly model size
  covid_start = as.Date("2020-01-01"),  # covid_start = as.Date("2020-03-01"),
  covid_end   = as.Date("2021-07-01"),  # covid_end   = as.Date("2020-12-01"),
  startEV     = as.Date("2017-01-01"),
  endEV       = as.Date("2025-01-01"),
  Tmax        = 300                     # Max time dimension
)

# ==============================================================================
# GDP CONTRIBUTIONS TO THE EURO AREA
# ==============================================================================
# Compute GDP weight of each country relative to Euro Area (EA)
# Output: Bar chart with relative contributions (in % or EUR millions)

GDP_com <- GDP_communality(c("EA", "DE", "FR", "IT", "ES"))


wide_gdp <- gdp_results$combined %>%
  filter(country != "EA") %>%
  tidyr::pivot_wider(names_from = country, values_from = gdp)

lagged_corrs <- GDP_lagged_correlation(wide_gdp, max_lag = 12)

plot_lagged_heatmap_grid(lagged_corrs, focus_countries = c("DE", "FR", "IT", "ES"))


# ==============================================================================
# EURO AREA COMPOSITION VISUALIZATION
# ==============================================================================
# Plot GDP breakdown for a specific quarter using national accounts data
# Input Excel must include GDP levels per country and quarter
# Custom quarter (e.g., Q4 2024)

plot_euro_area_gdp(file_path = "Data/GDP_millionsEA.xlsx", quarter = "2024-Q4")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================

