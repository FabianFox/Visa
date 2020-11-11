# Visa Network Data: 2020
# Append indendent variables

# Data
# year: 2020
# created in Visa_Scraper.R
# edited in Create_VisaNetworkData2020.R

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode")

# Load data
### ------------------------------------------------------------------------###
visa.df <- import("./data/visa_2020.rds")

# Load data:
# - Direct Contiguity
# - Accessed: 2020/11/11
## -------------------------------------------------------------------------- ##
# Directed dyads retrieved from http://www.correlatesofwar.org/data-sets/direct-contiguity
# contdird.csv - A directed dyad-year-level version of the master data

# Latest observation: 2016
contdird.df <- import(file = "./data/independent variables/contdird.csv", 
                   header = TRUE, stringsAsFactors = FALSE) %>%
  filter(year == 2016) %>%
  select(destination_iso3 = state1no, nationality_iso3 = state2no, contiguity = conttype) %>%
  filter(contiguity <= 3)

# Turn Correlates of War IDs into ISO3 codes
# (1) Some custom matches, i.e. 347 (Kosovo) = XKX, 345 (Serbia) = SRB 
custom.match <- c("345" = "SRB", "347" = "RKI")

# (2) Transform
contdird.df <- contdird.df %>%
  mutate(destination_iso3 = countrycode(sourcevar = destination_iso3, origin = "cown", 
                                        destination = "iso3c", 
                                        custom_match = custom.match),
         nationality_iso3 = countrycode(sourcevar = nationality_iso3, 
                                        origin = "cown", 
                                        destination = "iso3c", 
                                        custom_match = custom.match)) 

# Join to visa.df
visa.df <- visa.df %>%
  left_join(y = contdird.df)

# Load data:
# - cshapes
# see: http://nils.weidmann.ws/projects/cshapes/r-package.html
# - Accessed: 2020/11/11
## -------------------------------------------------------------------------- ##

# Distance between capitals
# cap_dist.df <- cshapes::distlist(date("2015-01-01"), type = "capdist")

# Export/import
# export(cap_dist.df, "./data/capital_distances.rds")

# Custom dict
# Note: Serbia has a different cown in GW than in COW
custom.match <- c("260" = "DEU" ,"340" = "SRB", "347" = "RKI", "678" = "YEM")

# Turn gwcode into iso3c
cap_dist.df <- import("./data/capital_distances.rds") %>%
  filter(ccode1 != ccode2) %>% # no-self ties
  mutate(destination_iso3 = countrycode(sourcevar = ccode1, origin = "cown", 
                                        destination = "iso3c", 
                                        custom_match = custom.match),
         nationality_iso3 = countrycode(sourcevar = ccode2, origin = "cown", 
                                        destination = "iso3c", 
                                        custom_match = custom.match)) %>%
  select(-ccode1, -ccode2)

# Join to visa.df
visa.df <- visa.df %>%
  left_join(y = cap_dist.df)
