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
            "tidygraph", "igraph", "intergraph", "mice")

# Load data
### ------------------------------------------------------------------------###
visa.df <- import("./data/visa_macro.rds")

# Network format(s)
### ------------------------------------------------------------------------###
# Transform into network format
graph.df <- tibble(
  # actors: states
  nodes = list(visa.df %>%
                 pull(destination_iso3) %>%
                 unique()),
  # relationships: visa waivers
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

# Missing data
### ------------------------------- ###
# ERGMs do not deal with missing data satisfactory. For now, quick imputation.
visa_imp.df <- visa.df %>%
  distinct(destination_iso3, .keep_all = TRUE) %>%
  select(destination_iso3, dest_gdp_median, dest_polity2) %>%
  mutate(dest_gdp_median = log(dest_gdp_median),
         dest_polity2 = if_else(!between(dest_polity2, -10, 10), NA_real_, dest_polity2))

# Distribution of NA
visa_imp.df %>%
  summarise_all(~sum(is.na(.)) / length(.) * 100)

# Run an "empty" imputation and adjust elements
mice.mat <- mice(visa_imp.df, maxit = 0)

# Elements that need to be adjusted
# Predictor matrix
pred.mat <- mice.mat$predictorMatrix

# Do not use for imputation
pred.mat[, c("destination_iso3")] <- 0

# Edit imputation method
imp_method <- mice.mat$method

# Create imputed datasets
visa_imp.df <- mice(visa_imp.df, m = 50, predictorMatrix = pred.mat, 
                    method = imp_method, print = FALSE)              # set.seed

# Return first imputed dataset
visa_imp.df <- complete(visa_imp.df)

### ------------------------------- ###

# Add node- and edge-attributes
visa.tbl <- visa.tbl %>%
  activate(nodes) %>%
  left_join(y = visa_imp.df, 
            by = c("name" = "destination_iso3"))

# Descriptive stats
### ------------------------------------------------------------------------###
# Density
visa_density.df <- visa.tbl %>%
  graph.density()

# Triad census
visa_triad.df <- visa.tbl %>%
  triad.census() %>%
  as_tibble() %>%
  mutate(triad = c("003", "012", "102", "021D", "021U", "021C", "111D", "111U", 
                   "030T", "030C", "201", "120D", "120U", "120C", "210", "300"))

# Direct clustering coefficient
visa_cluster.df <- visa.mat %>%
  DirectedClustering::ClustF(., type = "directed") %>%
  bind_rows() %>%
  mutate(country = row.names(visa.mat)) %>%
  select(country, everything())

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
  degree = list(visa_degree.df),
  clustering = list(visa_cluster.df)
) %>%
  mutate(
    degree_mean = mean(degree[[1]]$indegree)
  )

# ERGM
### ------------------------------------------------------------------------###
# Notes:
# - edge/dyadcov need attributes that match the modeled network

# Turn visa data into network-format
visa.net <- asNetwork(visa.tbl)

# Model
model <- ergm(visa.net ~ edges + 
                absdiff("dest_gdp_median") +
                absdiff("dest_polity2") +
                edgecov(contiguity.mat) + 
                gwidegree(decay = .1, fixed = TRUE) + 
                gwodegree(.1, fixed = TRUE) +
                gwesp(.1, fixed = TRUE) +
                mutual + 
                twopath + 
                ctriple +
                gwdsp(.1, fixed = TRUE),
              control = control.ergm(seed = 2020, 
                                     parallel = 3, 
                                     parallel.type = "PSOCK"),
              verbose = TRUE)
  
# ergm controls
# Parallel computing
# ergm(control = control.ergm(seed = 2020, parallel = 3, parallel.type = "PSOCK"))

# Additional controls (with defaults, cf. Goodreau et al. 2008)
# MCMC.interval = 1024
# MCMC.burnin = MCMC.interval * 16
# MCMC.samplesize = 1024