# Visa Network Data: 2020

# Data
# year: 2020 scraped in Visa_Scraper.R

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "igraph", "readxl")

                      #############################
                      #   Visa Network Data 2020  #
                      #############################

# Load raw data
### ------------------------------------------------------------------------###
visa_2020.df <- import("./data/VWP_06_2020.RDS") %>%
  mutate(requirement = flatten_chr(requirement)) %>%
  ungroup()

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

# Transform into  a network format
# Create an igraph graph from data frame
visa_2020.graph <- graph_from_data_frame(visa_2020.df[,c(1,2,6)], directed = TRUE)

# Transform into a matrix
visa_2020.mat <- get.adjacency(visa_2020.graph, sparse = FALSE, 
                          attr = "visa_requirement_binary")