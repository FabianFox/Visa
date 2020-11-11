# Scrape visa requirements
### ------------------------------------------------------------------------###

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "rvest", "httr", "splashr", "RSelenium", "countrycode")

# Gather information for the scraping procedure
### ------------------------------------------------------------------------###
# URL: 
url <- "http://www.ictseuropesystems.com/Home/TravelDoc"

# Splashr
### ------------------------------------------------------------------------###
# Guide: https://github.com/hrbrmstr/splashr

# Initialize in shell
# docker run -p 5023:5023 -p 8050:8050 -p 8051:8051 scrapinghub/splash:3.0

# Check whether connection has been established
splash_active()

# Render js-injected site
html_splsh <- render_html(url = url)

# Destination countries
destination <- html_splsh %>%
  html_nodes("select[id='traveldoc_destination_dropdown'] > option") %>%
  html_text() %>%
  enframe(name = NULL, value = "destination") %>%
  slice(2:length(destination)) %>%
  pull(destination)

nationality <- html_splsh %>%
  html_nodes("select[id='traveldoc_nationality_dropdown'] > option") %>%
  html_text() %>%
  enframe(name = NULL, value = "nationality") %>%
  slice(2:length(nationality)) %>%
  pull(nationality)

# Turn into tibble and adjust to equal length
travel.df <- tibble(
  destination = destination[which(destination %in% nationality)],
  nationality = nationality[which(nationality %in% destination)]
)

# Save object to disk
### ------------------------------------------------------------------------###
# rio::export(travel.df, file = "C:/Users/guelzauf/Seafile/Meine Bibliothek/Projekte/C01_Grenzen/Data/Miscellaneous/Helpful R-Code/travel_df.RDS")
visa <- import(file = "C:/Users/guelzauf/Seafile/Meine Bibliothek/Projekte/C01_Grenzen/Data/Miscellaneous/Helpful R-Code/travel_df.RDS") %>%
  mutate(destination_iso3 = countrycode(destination, "country.name.en", "iso3c",
                                         custom_match = c("Kosovo" = "RKS")),
         nationality_iso3 = countrycode(nationality, "country.name.en", "iso3c", 
                                        custom_match = c("Kosovo" = "RKS")))

# Use Selenium to interact with the site
### ------------------------------------------------------------------------###
# Start Docker through PowerShell and initialize a selenium/standalone-firefox:
# In PowerShell:
# Command: docker run -d -p 4445:4444  selenium/standalone-firefox:3
# Find: docker ps
# Stop: docker stop 'name'

# Prepare RSelenium to take control of Firefox
# ---------------------------------------------------------------------------- #
# Initialize the RSelenium server running chrome
remDr <- rsDriver(remoteServerAddr = "localhost", port = 4545L, browser = "firefox")

# Open the client to steer the browser
rD <- remDr[["client"]]

# Build the scraper
# ---------------------------------------------------------------------------- #
# Approach:
# (1) Manually manipulate homepage using Selenium
# (2) Turn into functions
# (3) Automate through loops

# Go to the homepage
rD$navigate(url)

# Prepare destination and nationality in the widget
# Destination
# destination_drop <- rD$findElement("css", "#traveldoc_destination_dropdown")
# destination_drop$clickElement()
#option <- rD$findElement(using = "xpath", paste0("//*[@id='traveldoc_destination_dropdown']/option[@value = '", visa$nationality_iso3[[68]],"']"))
#option$clickElement()

# Nationality
# nationality_drop <- rD$findElement("css", "#traveldoc_nationality_dropdown") 
# nationality_drop$clickElement()
#option <- rD$findElement(using = "xpath", paste0("//*[@id='traveldoc_nationality_dropdown']/option[@value = '", visa$nationality_iso3[[15]],"']"))
#option$clickElement()

# Click the submit button
#submit <- rD$findElement(using = "css", "#traveldoc_submit")
#submit$clickElement()

# Save message
#message <- rD$findElement(using = "css", "#traveldoc_messages")
#message$getElementText()[[1]]

# (2)
# Wrap the test case into functions 
prepare_dest_fun <- function(x) {
  option <- rD$findElement(using = "xpath", paste0("//*[@id='traveldoc_destination_dropdown']/option[@value = '", x,"']"))
  option$clickElement()
}

prepare_nat_fun <- function(x) {
  option <- rD$findElement(using = "xpath", paste0("//*[@id='traveldoc_nationality_dropdown']/option[@value = '", x,"']"))
  option$clickElement()
}

submit_fun <- function(x) {
  submit <- rD$findElement(using = "css", "#traveldoc_submit")
  submit$clickElement()
}

get_result_fun <- function(x) {
  message <- rD$findElement(using = "css", "#traveldoc_messages")
  message$getElementText()[[1]]
}

# Wrap as safe-functions
prepare_dest_fun <- possibly(prepare_dest_fun, "NA_character_")
prepare_nat_fun <- possibly(prepare_nat_fun, "NA_character_")
submit_fun <- possibly(submit_fun, "NA_character_")
get_result_fun<- possibly(get_result_fun, "NA_character_")

# (3) Automate through loops
visa_exp <- expand_grid(visa[,3], visa[,4]) %>%
  filter(destination_iso3 != nationality_iso3) %>%
  mutate_all(~if_else(.x == "DEU", "D", .x)) %>%
  arrange(nationality_iso3)

# Actual scraping
# started: 2020/05/04
# Notes:
# 
# ---------------------------------------------------------------------------- #
# Check current state
visa_current <- import("./data/VWP_06_2020.RDS") %>%
  select(destination_iso3, nationality_iso3)

# Remove nationalities already included
visa_step <- visa_exp %>%
  anti_join(y = visa_current)

# Create sample of N = 1500 queries
visa_step <- visa_step %>%
  sample_n(size = 120)

# The actual scraping
visa_added <- visa_step %>%
  mutate(requirement = 
           map2(
             .x = destination_iso3,
             .y = nationality_iso3, 
             ~{
               Sys.sleep(sample(seq(1, 8, 0.5), 1))
               prepare_dest_fun(.x)
               Sys.sleep(sample(seq(1, 2, 0.3), 1))
               prepare_nat_fun(.y)
               submit_fun() 
               Sys.sleep(sample(seq(2, 4, 0.5), 1))
               get_result_fun()
             }))

# Any missings
anyNA(visa_added)

# Combine
visa.df <- import("./data/VWP_06_2020.RDS") %>%
  bind_rows(visa_added)

# Save current result
# export(visa.df, "./data/VWP_06_2020.RDS")

# Keep a save copy
# export(visa.df, "C:/Users/guelzauf/Desktop/Visa_VWP_30-06-2020.RDS")
