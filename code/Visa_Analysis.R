# Visa waiver dataset
# Analysis

# Data
# year: 2020
# created in Visa_Scraper.R
# edited in Create_VisaNetworkData2020.R

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "patchwork", "statnet", "ggraph", "tidygraph",
            "igraph")

# Load data
### ------------------------------------------------------------------------###
visa.df <- import("./data/visa_2020.rds")

# Network format(s)
### ------------------------------------------------------------------------###
# Transform into network format
graph.df <- tibble(
  nodes = list(visa.df %>%
                 pull(destination_iso3) %>%
                 unique()),
  edges = list(visa.df %>% 
                 filter(visa_requirement_binary == 1) %>%
                 select(from = destination_iso3, to = nationality_iso3)))

# Igraph
# Create an igraph graph from data frame
visa.graph <- graph_from_data_frame(graph.df$edges, 
                                    vertices = graph.df$nodes,
                                    directed = TRUE)

# Transform into a matrix
visa.mat <- get.adjacency(visa.graph, sparse = FALSE)

# Tidygraph
visa.tbl <- as_tbl_graph(visa.mat)

# Network
# visa.net <- network(visa.mat)

# Descriptive stats
### ------------------------------------------------------------------------###
# Triad census
visa_triad.df <- triad.census(visa.graph)