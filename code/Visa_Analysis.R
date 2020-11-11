# Visa waiver dataset
# Analysis

# Data
# year: 2020
# created in Visa_Scraper.R
# edited in Create_VisaNetworkData2020.R

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "patchwork", "statnet")

# Load data
### ------------------------------------------------------------------------###
visa.df <- import("./data/visa_2020.rds")