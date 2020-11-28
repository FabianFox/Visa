# GMP: Global Visa Cost Dataset

# Data
# year: 2019
# source: https://cadmus.eui.eu/handle/1814/66583

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode")

# Load data
### ------------------------------------------------------------------------###
cost.df <- import("./data/GMP Visa Cost/GMP_GlobalVisaCostDataset_v1.0.xlsx") %>%
  select(source_iso3, target_iso3, tourist_visa)
