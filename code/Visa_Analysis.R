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

# Igraph
# Create an igraph graph from data frame
visa.graph <- graph_from_data_frame(visa.df[,c("destination_iso3", 
                                               "nationality_iso3")],
                                    directed = TRUE)

# Set visa requirement as edge-attribute
visa.graph <- set_edge_attr(visa.graph, "weight", 
                            value = visa.df$visa_requirement_binary)

# Transform into a matrix
visa.mat <- get.adjacency(visa.graph, sparse = FALSE, 
                               attr = "weight")

# Igraph graph
visa.graph <- graph_from_adjacency_matrix(visa.mat, mode = "directed")

# Tidygraph
visa.tbl <- as_tbl_graph(visa.mat)

# Network
visa.net <- network(visa.mat)

# Descriptive stats
### ------------------------------------------------------------------------###
# Triad census
visa_triad.df <- triad.census(visa.graph) %>%
  enframe()