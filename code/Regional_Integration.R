# Regional integration membership

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode")

# Load data
### ------------------------------------------------------------------------###
visa.df <- import("./data/visa_macro.rds")

# Prepare data and join EU
reg_integration.df <- visa.df %>%
  select(state1 = destination_iso3, state2 = nationality_iso3, dyad_name) %>%
  distinct(dyad_name, .keep_all = TRUE) %>%
  arrange(state1) %>%
  mutate(shared_membership = NA_real_,
         organization = NA_character_,
         source = NA_character_,
         name1 = countrycode(state1, "iso3c", "country.name.en"),
         name2 = countrycode(state2, "iso3c", "country.name.en")) %>%
  select(state1, state2, name1, name2, everything())

custom.match <- c("ISL" = "EU", "NOR" = "EU", "CHE" = "EU")

reg_integration.df <- reg_integration.df %>%
  mutate(eu1 = countrycode(state1, "iso3c", "eu28", custom_match = custom.match),
         eu2 = countrycode(state2, "iso3c", "eu28", custom_match = custom.match),
         shared_membership = if_else(eu1 == "EU" & eu2 == "EU", 1, NA_real_),
         organization = if_else(eu1 == "EU" & eu2 == "EU", "EFTA/EU", NA_character_),
         source = if_else(eu1 == "EU" & eu2 == "EU", 
                          "https://europa.eu/european-union/about-eu/countries_en; https://www.efta.int/about-efta/the-efta-states", 
                          NA_character_)) %>%
  select(-eu1, -eu2)

# Export
export(reg_integration.df, "./data/regional_integration.xlsx")
