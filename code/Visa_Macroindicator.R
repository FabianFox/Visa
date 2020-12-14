# Visa Network Data: 2020
# Append independent variables

# Data
# year: 2020
# created in Visa_Scraper.R
# edited in Create_VisaNetworkData2020.R

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "wbstats", "igraph")

# Load data
### ------------------------------------------------------------------------ ###
visa.df <- import("./data/visa_2020.rds")

# Merge EFTA/EU into a single node
### ------------------------------------------------------------------------ ###
# Custom match for EFTA/EU member states
custom.match <- codelist$iso3c[!is.na(codelist$eu28 == "EU")] 
# Add EFTA countries in data set
custom.match <- c(custom.match, "CHE", "ISL", "LIE","NOR")

# Merge EFTA/EU member states
visa_eu.df <- visa.df %>%
  mutate(destination_iso3 = if_else(destination_iso3 %in% custom.match, 
                                    "EU", destination_iso3),
         nationality_iso3 = if_else(nationality_iso3 %in% custom.match, 
                                    "EU", nationality_iso3)) %>%
  distinct(destination_iso3, nationality_iso3, .keep_all = TRUE) %>%
  filter(!(destination_iso3 == "EU" & nationality_iso3 == "EU")) 

                        ## ---------------------- ##
                        ##    EDGE ATTRIBUTES     ##
                        ## ---------------------- ##

# Edge attributes
### ------------------------------------------------------------------------ ###

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
  select(destination_iso3 = state1no, 
         nationality_iso3 = state2no, 
         contiguity = conttype) %>%
  filter(contiguity <= 3)

# Turn Correlates of War IDs into ISO3 codes
# (1) Some custom matches, i.e. 347 (Kosovo) = XKX, 345 (Serbia) = SRB 
custom.cow <- c("345" = "SRB", "347" = "RKI")

# (2) Transform
contdird.df <- contdird.df %>%
  mutate(destination_iso3 = countrycode(sourcevar = destination_iso3, origin = "cown", 
                                        destination = "iso3c", 
                                        custom_match = custom.cow),
         nationality_iso3 = countrycode(sourcevar = nationality_iso3, 
                                        origin = "cown", 
                                        destination = "iso3c", 
                                        custom_match = custom.cow),
         destination_iso3 = if_else(destination_iso3 %in% custom.match, 
                                    "EU", destination_iso3),
         nationality_iso3 = if_else(nationality_iso3 %in% custom.match, 
                                    "EU", nationality_iso3)) %>%
  distinct(destination_iso3, nationality_iso3, .keep_all = TRUE) %>%
  filter(!(destination_iso3 == "EU" & nationality_iso3 == "EU")) 

# Join to visa.df
visa_eu.df <- visa_eu.df %>%
  left_join(y = contdird.df) %>%
  mutate(contiguity = replace_na(contiguity, 0))

# Contiguity in network format
# Create an igraph graph from data frame
contiguity.graph <- graph_from_data_frame(visa_eu.df %>%
                                            filter(contiguity == 1) %>%
                                            select(from = destination_iso3, 
                                                   to = nationality_iso3),
                                          vertices = visa_eu.df %>%
                                            pull(destination_iso3) %>%
                                            unique(), 
                                          directed = FALSE)

# Transform into a matrix
contiguity.mat <- get.adjacency(contiguity.graph, sparse = FALSE) 

# Directed format sums contiguity scores
contiguity.mat[contiguity.mat == 2] <- 1

# COW: Trade v4.0
# Variable: flow1, flow2
# Year: 2014
# retrieved from https://correlatesofwar.org/data-sets/bilateral-trade
## -------------------------------------------------------------------------- ##
# Dyadic trade
trade_dyad.df <- import("./data/independent variables/Dyadic_COW_4.0.csv") %>%
  select(state1 = ccode1, state2 = ccode2, year, import = flow1, export = flow2) %>%
  filter(year == 2014) %>%
  mutate(state1 = countrycode(sourcevar = state1, 
                              origin = "cown", 
                              destination = "iso3c", 
                              custom_match = custom.cow),
         state2 = countrycode(sourcevar = state2, 
                              origin = "cown", 
                              destination = "iso3c", 
                              custom_match = custom.cow),
         import = na_if(import, -9),
         export = na_if(export, -9)) %>%
  filter(state1 %in% unique(visa.df$destination_iso3), # subset to countries in visa.df
         state2 %in% unique(visa.df$destination_iso3))

# Make the dataset (long) dyadic
# (1) Duplicate dataset, swap country identifiers and rename them
swap.df <- trade_dyad.df %>%
  rename(
    state1 = state2,
    state2 = state1,
  ) %>%
  rename(export = import,
         import = export
  )

# (2) Merge trade.df and swap.df
trade_dyad.df <- trade_dyad.df %>%
  bind_rows(., swap.df)

# (3) Combine EFTA/EU
trade_dyad.df <- trade_dyad.df %>%
  mutate(state1 = if_else(state1 %in% custom.match, "EU", state1),
         state2 = if_else(state2 %in% custom.match, "EU", state2)) 

# (3a) Trade in EU
trade_eu.df <- trade_dyad.df %>%
  filter(state1 == "EU" & state2 == "EU") %>%
  summarise(import = sum(import, na.rm = TRUE),
            export = sum(export, na.rm = TRUE))

# (3b) Trade with EFTA/EU as single actor
trade_dyad.df <- trade_dyad.df %>%
  filter(!(state1 == "EU" & state2 == "EU")) %>%
  group_by(state1, state2) %>%
  mutate(import = sum(import),
         export = sum(export)) %>%
  ungroup() %>%
  distinct(state1, state2, .keep_all = TRUE)

# Monadic trade
## -------------------------------------------------------------------------- ##
trade_monadic.df <- import("./data/independent variables/National_COW_4.0.csv") %>%
  select(state = ccode, year, import_total = imports, export_total = exports) %>%
  filter(year == 2014) %>%
  mutate(state = countrycode(sourcevar = state, 
                             origin = "cown", 
                             destination = "iso3c",
                             custom_match = custom.cow), 
         import_total = na_if(import_total, -9),
         export_total = na_if(export_total, -9)) %>%
  filter(state %in% unique(visa.df$destination_iso3)) %>% # subset to countries in visa.df
  mutate(state = if_else(state %in% custom.match, "EU", state)) %>% 
  select(-year) %>%
  group_by(state) %>%
  summarize(import_total = sum(import_total),
            export_total = sum(export_total)) %>%
  ungroup()

# Subtract intra-EU trade
trade_monadic.df[trade_monadic.df$state == "EU", c("import_total", "export_total")] <- trade_monadic.df[trade_monadic.df$state == "EU", c("import_total", "export_total")] - trade_eu.df

# Economic interdependence 
# Share imports / total imports
# Share exports / total exports
# Trade interdependence (see Avdan 2019: 88)
## -------------------------------------------------------------------------- ##
trade.df <- trade_dyad.df %>%
  left_join(y = trade_monadic.df, by = c("state1" = "state")) %>%
  mutate(import_share = import / import_total * 100,
         export_share = export / export_total * 100,
         trade_interdependence = (import + export) / (import_total + export_total))

# World Refugee Dataset
# Variable:
# Year: 2015 (latest)
## -------------------------------------------------------------------------- ##
# retrieved from: Marbach (2018), Link: https://github.com/sumtxt/wrd

# COW -> iso3c
custom.wrd <- c("345" = "SRB", "347" = "RKI", "667" = "PSE")

# Load data and prepare variables: 
# refugees from neighboring country, total N of refugees hosted
wrd.df <- import("https://raw.githubusercontent.com/sumtxt/wrd/master/usedata/wrd_1.1.0.csv") %>%
  filter(year == 2015,
         asylum_ccode != 667) %>% # drop Palestine as host country
  mutate(state1 = countrycode(sourcevar = asylum_ccode, origin = "cown", 
                              destination = "iso3c", custom_match = custom.match),
         state2 = countrycode(sourcevar = origin_ccode, origin = "cown", 
                              destination = "iso3c", custom_match = custom.match)) %>%
  group_by(state1) %>%
  mutate(rfgs_incoming_agg = sum(ylinpol, na.rm = TRUE)) %>%
  ungroup() %>%
  select(state1, state2, rfgs_incoming = ylinpol, rfgs_incoming_agg) 

# Join to border.df and compute refugees hosted per capita
visa.df <- visa.df %>%
  left_join(y = wrd.df, by = c("destination_iso3" = "state1", 
                               "nationality_iso3" = "state2"))

# Create column for refugees sent and total number of refugees in neigbouring country
wrd.df <- wrd.df %>%
  select(rfgs_outgoing = rfgs_incoming,
         rfgs_outgoing_agg = rfgs_incoming_agg, 
         state1 = state2,
         state2 = state1)

# Join to border.df and compute refugees hosted per capita
visa.df <- visa.df %>%
  left_join(y = wrd.df, by = c("destination_iso3" = "state1", 
                               "nationality_iso3" = "state2"))

# Load data:
# - cshapes
# see: http://nils.weidmann.ws/projects/cshapes/r-package.html
# - Accessed: 2020/11/11
## -------------------------------------------------------------------------- ##
# Distance between capitals
# cap_dist.df <- cshapes::distlist(date("2015-01-01"), type = "capdist")

# Export/import
# export(cap_dist.df, "./data/capital_distances.rds")

# Custom dictionary
# Note: Serbia has a different cown in GW than in COW
custom.gw <- c("260" = "DEU" ,"340" = "SRB", "347" = "RKI", "678" = "YEM")
custom.eu <- custom.match[-2] # Remove BEL

# Turn gwcode into iso3c
cap_dist.df <- import("./data/capital_distances.rds") %>%
  filter(ccode1 != ccode2) %>% # no-self ties
  mutate(destination_iso3 = countrycode(sourcevar = ccode1, origin = "cown", 
                                        destination = "iso3c", 
                                        custom_match = custom.gw),
         nationality_iso3 = countrycode(sourcevar = ccode2, origin = "cown", 
                                        destination = "iso3c", 
                                        custom_match = custom.gw),
         destination_iso3 = if_else(destination_iso3 %in% custom.eu, 
                                    "EU", destination_iso3),
         nationality_iso3 = if_else(nationality_iso3 %in% custom.eu, 
                                    "EU", nationality_iso3)) %>%
  distinct(destination_iso3, nationality_iso3, .keep_all = TRUE) %>%
  filter(!(destination_iso3 == "EU" | nationality_iso3 == "EU")) %>%
  mutate(destination_iso3 = if_else(destination_iso3 == "BEL", "EU", destination_iso3),
         nationality_iso3 = if_else(nationality_iso3 == "BEL", "EU", nationality_iso3)) %>%
  select(-ccode1, -ccode2) 

# Join to visa.df
visa_eu.df <- visa_eu.df %>%
  left_join(y = cap_dist.df)

# Network format
cap_dist.graph <- graph_from_data_frame(visa_eu.df %>%
                                          select(from = destination_iso3, 
                                                 to = nationality_iso3,
                                                 weight = capdist),
                                        vertices = visa_eu.df %>%
                                          pull(destination_iso3) %>%
                                          unique(), 
                                        directed = FALSE)

# Transform into a matrix
cap_dist.mat <- get.adjacency(cap_dist.graph, sparse = FALSE, attr = "weight") 

                          ## ---------------------- ##
                          ##    NODE ATTRIBUTES     ##
                          ## ---------------------- ##

# Node attributes
### ------------------------------------------------------------------------ ###
states.df <- visa_eu.df %>%
  distinct(destination_iso3)

# PolityV data
### ------------------------------------------------------------------------ ###
# PolityV project
# Year: 2018 
# Custom match for PolityV data
custom.pol <- c("342" = "SRB", "348" = "MNE", "525" = "SSD", "529" = "ETH", 
                "818" = "VNM")
# Load data
polity.df <- import("http://www.systemicpeace.org/inscr/p5v2018.sav") %>%
  filter(year == 2018) %>%
  mutate(iso3c = countrycode(ccode, "cown", "iso3c", 
                             custom_match = custom.pol)) %>%
  select(iso3c, polity, polity2) %>%
  mutate(iso3c = if_else(iso3c %in% custom.match, "EU", iso3c)) %>%
  group_by(iso3c) %>%
  # ISL and MLT are missing
  mutate(n = n(),
         polity = sum(polity, na.rm = TRUE) / n,
         polity2 = sum(polity2, na.rm = TRUE) / n) %>%
  ungroup() %>%
  distinct(iso3c, .keep_all = TRUE) %>% 
  select(-n)

# Join to visa.df 
states.df <- states.df %>%
  left_join(y = polity.df, by = c("destination_iso3" = "iso3c"))

# Find missing values (N = 6 + MLT & ISL)
states.df %>% 
  filter(across(c(polity, polity2), ~is.na(.)))

# World Bank Indicator
# GDP per capita, PPP (current international $) - "NY.GDP.PCAP.PP.CD"
# Total Population - "SP.POP.TOTL"
# Year: Mean/Median (2015-2019)
## -------------------------------------------------------------------------- ##
# (1) Download WB data
# Download data (mrv = newest available)
wb.info <- wb_data(country = unique(visa.df$destination_iso3),
                   indicator = c("NY.GDP.PCAP.PP.CD", "SP.POP.TOTL"), 
                   start_date = 2015, end_date = 2019, return_wide = TRUE) %>%
  group_by(iso3c) %>%
  summarise(pop_median = median(SP.POP.TOTL, na.rm = TRUE),
            pop_mean = mean(SP.POP.TOTL, na.rm = TRUE),
            gdp_median = median(NY.GDP.PCAP.PP.CD, na.rm = TRUE),
            gdp_mean = mean(NY.GDP.PCAP.PP.CD, na.rm = TRUE)) %>%
  ungroup() %>%
  select(starts_with(c("pop", "gdp")), iso3c)

# Find missing values (N = 8)
wb.info %>% 
  filter_all(any_vars(is.na(.)))

# Replace missing values (from CIA World Factbook, accessed: 2020/12/11)
# CUB: GDP: 12300 (2016) / 12200 (2015)
wb.info[wb.info$iso3c == "CUB", c("gdp_median", "gdp_mean")] <- list(12300, 12300)
# ERI: Population: 6081196 
#      GDP: 1600 (2017) / 1500 (2016) / 1500 (2015)
wb.info[wb.info$iso3c == "ERI", c("pop_median", "pop_mean", "gdp_median", "gdp_mean")] <- list(6081196, 6081196, 1600, 1600)
# PRK: GDP: 1700 (2015.) 
wb.info[wb.info$iso3c == "PRK", c("gdp_median", "gdp_mean")] <- list(1700, 1700)
# SOM
# NA
# SSD: GDP: 1600 (2017) / 1700 (2016) / 2100 (2015)
wb.info[wb.info$iso3c == "SSD", c("gdp_median", "gdp_mean")] <- list(1600, 1600)
# SYR: GDP: 2900 (2015)
wb.info[wb.info$iso3c == "SYR", c("gdp_median", "gdp_mean")] <- list(2900, 2900)
# VEN: GDP: 12500 (2017) / 14400 (2016) / 17300 (2015) 
wb.info[wb.info$iso3c == "VEN", c("gdp_median", "gdp_mean")] <- list(12500, 12500)
# YEM: GDP: 2500 (2017) / 2700 (2016) / 3200 (2015) 
wb.info[wb.info$iso3c == "YEM", c("gdp_median", "gdp_mean")] <- list(2500, 2500)
# No data on TWN
# TWN: Population: 23603049
#      GDP: 50500 (2017) / 49100 (2016) / 48500 (2015) 
wb.info <- wb.info %>%
  add_row(pop_median = 23603049, pop_mean = 23603049, 
          gdp_median = 50500, gdp_mean = 50500, iso3c = "TWN")

# Create variables for EU
wb.info <- wb.info %>%
  mutate(iso3c = if_else(iso3c %in% custom.match, "EU", iso3c)) %>%
  group_by(iso3c) %>%
  mutate(n = n(),
         pop_median = sum(pop_median),
         pop_mean = sum(pop_mean),
         gdp_median = sum(gdp_median) / n,
         gdp_mean = sum(gdp_mean) / n) %>%
  ungroup() %>%
  distinct(iso3c, .keep_all = TRUE) %>% 
  select(-n)

# Join to visa.df
states.df <- states.df %>%
  left_join(y = wb.info, by = c("destination_iso3" = "iso3c"))

## -------------------------------------------------------------------------- ##
##                                 MOBILITY                                   ##
## -------------------------------------------------------------------------- ##

# Global Transnational Mobility
# Variable: Estimated trips
# Year: 2016
# retrieved from Recchi et al. (2019) "Estimating Transnational Human Mobility 
#                                      on a Global Scale"
# Note: Dataset was transformed from .xlsx to .csv to speed up importing
## -------------------------------------------------------------------------- ##
# Function to create a dyad identifier 
# from: https://stackoverflow.com/questions/52316998/create-unique-id-for-dyads-non-directional

# (1) Load and filter to 2016
gtm.df <- import("./data/independent variables/Global_Transnational_Mobility_dataset_v1.0.csv") %>%
  select(3:6) %>%
  filter(year == 2016) %>%
  select(-year)

# (2) Join to visa.df
# estimated trips are (~nearly) symmetric
visa.df <- visa.df %>%
  # outgoing trips
  left_join(y = gtm.df, by = c("destination_iso3" = "source_iso3", 
                               "nationality_iso3" = "target_iso3")) %>%
  rename(trips_outgoing = estimated_trips) %>%
  # incoming trips
  left_join(y = gtm.df, by = c("destination_iso3" = "target_iso3", 
                               "nationality_iso3" = "source_iso3")) %>%
  rename(trips_incoming = estimated_trips)

## -------------------------------------------------------------------------- ##
##                                 SECURITY                                   ##
## -------------------------------------------------------------------------- ##

# Create a dyad identifier variable
## -------------------------------------------------------------------------- ##
# Function to create a dyad identifier 
# from: https://stackoverflow.com/questions/52316998/create-unique-id-for-dyads-non-directional

dyadId_fun <- function(x,y) paste(sort(c(x, y)), collapse="_")
dyadId_fun <- Vectorize(dyadId_fun)

# Apply the function
visa_eu.df <- visa_eu.df %>% 
  mutate(dyad_name = dyadId_fun(destination_iso3, nationality_iso3)) %>%
  as_tibble()

# Export data
## -------------------------------------------------------------------------- ##
# Main data
export(visa_eu.df, "./data/visa_main.rds")

# Node attributes
export(states.df, "./data/node_attributes.rds")

# Edge attributes
edge.df <- tibble(
  type = c("contiguity", "capdist"),
  network = c(
    list(contiguity.mat),
    list(cap_dist.mat)
  ))

# Export
export(edge.df, "./data/edge_attributes.rds")
