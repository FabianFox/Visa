# Visa Network Data: 2020

# Data
# year: 2020 scraped in Visa_Scraper.R

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "lubridate", "rio", "countrycode", "igraph", "readxl", "states")

                      #############################
                      #   Visa Network Data 2020  #
                      #############################

# Load raw data
### ------------------------------------------------------------------------###
visa_2020.df <- import("./data/VWP_07_2020.RDS") %>%
  mutate(requirement = flatten_chr(requirement)) %>%
  ungroup()

# Data cleaning
### ------------------------------------------------------------------------###
# (1) Split requirement into information on visa and passport requirements
# (2) Create a binary indicator on visa waivers (1 = visa req. waived; 0 = visa req)
visa_2020.df <- visa_2020.df %>%
  mutate(
    passport_requirement = str_extract(requirement, "(?<=\\n).+"),
    visa_requirement = str_extract(requirement, ".+(?=\\n)"),
    visa_requirement_binary = case_when(
    str_detect(requirement, 
               "Visa is required|Es ist ein Visum erforderlich|Visa may be obtained on arrival") ~ 0,
    str_detect(requirement, 
               "Visa is not required") ~ 1,
    TRUE ~ NA_real_),
    across(c("destination_iso3", "nationality_iso3"), ~str_replace(.x, "\\bD\\b", "DEU")))

# Independent states as defined by Gleditsch & Ward (1999) 
# data: http://ksgleditsch.com/data-4.html
# Note: excluding microstates
# Custom matches, i.e. 347 (Kosovo) = RKS
custom.match <- c("260" = "DEU" ,"340" = "SRB", "347" = "RKS", "678" = "YEM")

# Data
states.df <- gwstates %>%
  filter(year(end) == 9999 & microstate == FALSE) %>%
  mutate(iso3c = countrycode(gwcode, "cown", "iso3c",     # original ISO3 is out-of-date
                            custom_match = custom.match))

# Subset
# also exclude Taiwan (TWN) due to many missing values
visa_2020.df <- visa_2020.df %>%
  filter(destination_iso3 %in% states.df$iso3c &
           nationality_iso3 %in% states.df$iso3c,
         !destination_iso3 == "TWN",
         !nationality_iso3 == "TWN")


# Manually add missing information on a few dyads in 2020
### ------------------------------------------------------------------------###
# CAN -> SOM (visa required)
visa_2020.df[visa_2020.df$destination_iso3 == "CAN" & 
               visa_2020.df$nationality_iso3 == "SOM",
             c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)

# BRA -> CAF (visa required)
visa_2020.df[visa_2020.df$destination_iso3 == "BRA" & 
               visa_2020.df$nationality_iso3 == "CAF",
             c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)

# ARE -> ISR (visa required / not applicable)
visa_2020.df[visa_2020.df$destination_iso3 == "ARE" & 
               visa_2020.df$nationality_iso3 == "ISR",
             c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)
  
# GBR -> NAM (no visa required)
visa_2020.df[visa_2020.df$destination_iso3 == "GBR" & 
               visa_2020.df$nationality_iso3 == "NAM",
             c("visa_requirement", "visa_requirement_binary")] <- list("Visa is not required.", 1)

# BLZ -> PAN (visa required)
visa_2020.df[visa_2020.df$destination_iso3 == "BLZ" & 
               visa_2020.df$nationality_iso3 == "PAN",
             c("visa_requirement", "visa_requirement_binary")] <- list("Visa is not required.", 1)

# ARE -> QAT (visa required)
visa_2020.df[visa_2020.df$destination_iso3 == "ARE" & 
               visa_2020.df$nationality_iso3 == "QAT",
             c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)

# Transform into  a network format
# Create an igraph graph from data frame
visa_2020.graph <- graph_from_data_frame(visa_2020.df[,c("destination_iso3",
                                                         "nationality_iso3",
                                                         "visa_requirement_binary")], 
                                         directed = TRUE)

# Transform into a matrix
visa_2020.mat <- get.adjacency(visa_2020.graph, sparse = FALSE, 
                          attr = "visa_requirement_binary")

# Export
export(visa_2020.df, "./data/visa_2020.rds")
