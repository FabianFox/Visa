# Visa waiver dataset
# Analysis

# Data
# year: 2020
# created in Visa_Scraper.R
# edited in Create_VisaNetworkData2020.R

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "patchwork", "statnet", "ggraph", 
            "tidygraph", "igraph", "intergraph")

# Load data
### ------------------------------------------------------------------------###
visa.df <- import("./data/visa_macro.rds")

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
visa.tbl <- as_tbl_graph(visa.graph)

# Add node- and edge-attributes
### ------------------------------------------------------------------------###
visa.tbl <- visa.tbl %>%
  activate(nodes) %>%
  left_join(y = visa.df %>%
              distinct(destination_iso3, .keep_all = TRUE) %>%
              select(destination_iso3, capdist), 
            by = c("name" = "destination_iso3"))

# Descriptive stats
### ------------------------------------------------------------------------###
# Density
visa_density.df <- visa.tbl %>%
  graph.density()

# Triad census
visa_triad.df <- visa.tbl %>%
  triad.census()

# Reciprocity
visa_rcp.df <- visa.tbl %>%
  reciprocity()

# Degree distribution
# Indegree
visa_indegree.df <- visa.tbl %>%
  degree(., mode = "in") %>%
  enframe(name = "country", value = "indegree") %>%
  arrange(desc(indegree))

# Outdegree
visa_outdegree.df <- visa.tbl %>%
  degree(., mode = "out") %>%
  enframe(name = "country", value = "outdegree")

# In- and outdegree
visa_degree.df <- visa_indegree.df %>%
  left_join(y = visa_outdegree.df)

# Mean in- and outdegree
visa.tbl %>% degree(., mode = "in") %>%
  mean()

# Gather descriptive measures
visa_stats.df <- tibble(
  density = visa_density.df,
  triads = list(visa_triad.df),
  reciprocity = list(visa_rcp.df),
  degree = list(visa_degree.df)
) %>%
  mutate(
    degree_mean = mean(degree[[1]]$indegree)
  )

# ERGM
### ------------------------------------------------------------------------###
visa.net <- asNetwork(visa.tbl)

# Model
