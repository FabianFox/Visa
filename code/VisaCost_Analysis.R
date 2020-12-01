# GMP: Global Visa Cost Dataset

# Data
# year: 2019
# source: https://cadmus.eui.eu/handle/1814/66583

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "lubridate","countrycode", "states")

# Load data
### ------------------------------------------------------------------------###
visa_cost.df <- import("./data/GMP Visa Cost/GMP_GlobalVisaCostDataset_v1.0.xlsx") %>%
  select(source_iso3, target_iso3, tourist_visa)

# Filter to independent states
### ------------------------------------------------------------------------###
# Independent states as defined by Gleditsch & Ward (1999) 
# data: http://ksgleditsch.com/data-4.html
# Note: excluding microstates
# Custom matches, i.e. 347 (Kosovo) = XKX
custom.match <- c("260" = "DEU" ,"340" = "SRB", "347" = "RKI", "678" = "YEM")

# Data
states.df <- gwstates %>%
  filter(year(end) == 9999 & microstate == FALSE) %>%
  mutate(iso3c = countrycode(gwcode, "cown", "iso3c",     # original ISO3 is out-of-date
                             custom_match = custom.match))

# Subset
visa_cost.df <- visa_cost.df %>%
  filter(source_iso3 %in% states.df$iso3c &
           target_iso3 %in% states.df$iso3c)
