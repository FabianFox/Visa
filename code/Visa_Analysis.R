# Visa waiver dataset
# Analysis

# Data
# year: 2020
# created in Visa_Scraper.R
# edited in Create_VisaNetworkData2020.R
# independent variables from Visa_Macroindicator.R

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "patchwork", "ggraph", 
            "tidygraph", "igraph", "intergraph", "mice")

# Load data
### ------------------------------------------------------------------------ ###
visa.df <- import("./data/visa_main.rds")

# Network format(s)
### ------------------------------------------------------------------------ ###
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
### ------------------------------------------------------------------------ ###

# Load data
node_att.df <- import("./data/node_attributes.rds")
edge_att.df <- import("./data/edge_attributes.rds")

# Missing data
### ------------------------------- ###
# ERGMs do not deal with missing data satisfactory. For now, quick imputation.
visa_imp.df <- node_att.df %>%
  select(destination_iso3, gdp_mean, polity2, nterror) %>%
  mutate(gdp_log = log(gdp_mean),
         nterror_log = log(nterror),
         polity2 = if_else(!between(polity2, -10, 10), NA_real_, polity2)) %>%
  select(-gdp_mean, -nterror)

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
### ------------------------------------------------------------------------ ###
# Density
visa_density.df <- visa.tbl %>%
  graph.density()

# Triad census
visa_triad.df <- visa.tbl %>%
  igraph::triad_census() %>%
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
### ------------------------------------------------------------------------ ###
# Notes:
# - edge/dyadcov need attributes that match the modeled network

# load statnet
pkg_attach2("statnet")

# Turn visa data into network-format
visa.net <- asNetwork(visa.tbl)

# Prepare edge attributes
# Contiguity network
contiguity.mat <- edge_att.df %>%
  filter(type == "contiguity") %>%
  pull(network) %>%
  .[[1]]

# Refugee network
rfgs.mat <- edge_att.df %>%
  filter(type == "refugees") %>%
  pull(network) %>%
  .[[1]]

# Migration network
mig.mat <- edge_att.df %>%
  filter(type == "migration") %>%
  pull(network) %>%
  .[[1]]

# Model
### ------------------------------- ###
# Extract model fit
modelfit_fun <- function(x){
  tibble(
   call = as.character(x$call)[2],
   BIC = BIC(x),
   AIC = AIC(x),
   logLik = logLik(x)[1])
}

# Triad counts
triad_fun <- function(x){
  x %>% 
    imap_dfr(~asIgraph(.x) %>%
               igraph::triad_census() %>%
               as_tibble() %>%
               mutate(triad = c("003", "012", "102", "021D", "021U", "021C", "111D", "111U", 
                                "030T", "030C", "201", "120D", "120U", "120C", "210", "300"),
                      sim_id = .y)) %>% 
    group_by(triad) %>%
    summarize(value = mean(value)) %>%
    ungroup() %>%
    rename(sim_triads = value) %>%
    left_join(y = visa_triad.df, by = "triad")
}

# Existing model statistics 
### ------------------------------- ###
# Model fit
model.fit <- import("./output/model_fit.rds")

# Triad count
triad.count <- import("./output/triad_count.rds")

# Null model (only edge-term)
### ------------------------------- ###
null_model <- ergm(visa.net ~ edges,
                   control = control.ergm(seed = 2020, 
                                          parallel = 3, 
                                          parallel.type = "PSOCK"), 
                   verbose = TRUE)

# Model fit
model.fit <- modelfit_fun(null_model)

# Goodness-of-fit (GOF)
null_model.gof <- gof(null_model)

# Simulate networks
null_model.sim <- simulate(null_model,
                           nsim = 100,
                           control = control.simulate.ergm(MCMC.burnin = 1000,
                                                           MCMC.interval = 1000),
                           seed = 2020)

# Get model statistics and compare to empirical network
null_model.sim %>% 
  map_dbl(~asIgraph(.x) %>% 
        reciprocity()) %>% 
  unlist() %>% 
  mean()

# Mutual model
### ------------------------------- ###
mutual_model <- ergm(visa.net ~ edges + mutual,
                     control = control.ergm(seed = 2020, 
                                            parallel = 3, 
                                            parallel.type = "PSOCK"), 
                     verbose = TRUE)

# Model fit
model.fit <- model.fit %>%
  add_row(modelfit_fun(mutual_model))

# Goodness-of-fit (GOF)
mutual_model.gof <- gof(mutual_model)

# Simulate networks
mutual_model.sim <- simulate(mutual_model,
                             nsim = 100,
                             control = control.simulate.ergm(
                               MCMC.burnin = 1000,
                               MCMC.interval = 1000),
                             seed = 2020)

# Get model statistics and compare to empirical network
# Reciprocity
mutual_model.sim %>%
  map_dbl(~asIgraph(.x) %>% 
            reciprocity()) %>% 
  unlist() %>% 
  mean()

# Triads
mutual_model.triads <- triad_fun(mutual_model.sim)

# Balance
### ------------------------------- ###
balance_model <- ergm(visa.net ~ edges + balance,
                     control = control.ergm(seed = 2020, 
                                            parallel = 3, 
                                            parallel.type = "PSOCK",
                                            MCMC.interval = 10240,
                                            MCMC.samplesize = 10240), 
                     verbose = TRUE)

# Model fit
model.fit <- model.fit %>%
  add_row(modelfit_fun(balance_model))

# Goodness-of-fit (GOF)
balance_model.gof <- gof(balance_model)

# Simulate networks
balance_model.sim <- simulate(balance_model,
                              nsim = 100,
                              control = control.simulate.ergm(
                                MCMC.burnin = 1000,
                                MCMC.interval = 1000),
                              seed = 2020)

# Get model statistics and compare to empirical network
# Reciprocity
balance_model.sim %>%
  map_dbl(~asIgraph(.x) %>% 
            reciprocity()) %>% 
  unlist() %>% 
  mean()

# Triads
balance_model.triads <- triad_fun(balance_model.sim)

# Mutual + attributes
### ------------------------------- ###
mutual_attr_model <- ergm(visa.net ~ edges + mutual +
                         nodeocov("gdp_log") + nodeicov("gdp_log") + absdiff("gdp_log") +
                         nodeocov("polity2") + nodeicov("polity2") + absdiff("polity2") +
                         edgecov(contiguity.mat),
                      control = control.ergm(seed = 2020, 
                                             parallel = 3, 
                                             parallel.type = "PSOCK",
                                             MCMC.burnin = 100000,
                                             MCMC.samplesize = 50000,
                                             MCMLE.maxit = 20), 
                      verbose = TRUE)

# Model fit
model.fit <- model.fit %>%
  add_row(modelfit_fun(mutual_attr_model))

# Goodness-of-fit (GOF)
mutual_attr_model.gof <- gof(mutual_attr_model)

# Simulate networks
mutual_attr_model.sim <- simulate(mutual_attr_model,
                              nsim = 100,
                              control = control.simulate.ergm(
                                MCMC.burnin = 1000,
                                MCMC.interval = 1000),
                              seed = 2020)

# Get model statistics and compare to empirical network
# Reciprocity
mutual_attr_model.sim %>%
  map_dbl(~asIgraph(.x) %>% 
            reciprocity()) %>% 
  unlist() %>% 
  mean()

# Triads
mutual_attr_model.triads <- triad_fun(mutual_attr_model.sim)

# gwesp + attributes
### ------------------------------- ###
gwesp_attr_model <- ergm(visa.net ~ edges + mutual + gwesp(decay = 0, fixed = TRUE) +
                            nodeocov("gdp_log") + nodeicov("gdp_log") + absdiff("gdp_log") +
                            nodeocov("polity2") + nodeicov("polity2") + absdiff("polity2") +
                            edgecov(contiguity.mat),
                          control = control.ergm(seed = 2020, 
                                                 parallel = 3, 
                                                 parallel.type = "PSOCK",
                                                 MCMC.burnin = 100000,
                                                 MCMC.samplesize = 50000,
                                                 MCMLE.maxit = 20), 
                          verbose = TRUE)

# Model fit
model.fit <- model.fit %>%
  add_row(modelfit_fun(gwesp_attr_model))

# Goodness-of-fit (GOF)
gwesp_attr_model.gof <- gof(gwesp_attr_model)

# Simulate networks
gwesp_attr_model.sim <- simulate(gwesp_attr_model,
                                  nsim = 100,
                                  control = control.simulate.ergm(
                                    MCMC.burnin = 1000,
                                    MCMC.interval = 1000),
                                  seed = 2020)

# Get model statistics and compare to empirical network
# Reciprocity
gwesp_attr_model.sim %>%
  map_dbl(~asIgraph(.x) %>% 
            reciprocity()) %>% 
  unlist() %>% 
  mean()

# Triads
gwesp_attr_model.triads <- triad_fun(gwesp_attr_model.sim)

# gwesp (RTP) + gwodegree +  attributes
### ------------------------------- ###
gwdsp_attr_model <- ergm(visa.net ~ edges + mutual + 
                              gwdsp(0.1, fixed = TRUE) +
                              gwodegree(decay = .5, fixed = TRUE) + 
                              gwidegree(0, fixed = TRUE) +
                              nodeocov("gdp_log") + nodeicov("gdp_log") + absdiff("gdp_log") +
                              nodeocov("polity2") + nodeicov("polity2") + absdiff("polity2") +
                              nodeocov("nterror_log") + nodeicov("nterror_log") +
                              edgecov(contiguity.mat),
                            control = control.ergm(seed = 2020, 
                                                parallel = 3, 
                                                parallel.type = "PSOCK",
                                                MCMC.burnin = 100000,
                                                MCMC.samplesize = 50000,
                                                MCMLE.maxit = 20), 
                            verbose = TRUE)

# Model fit
model.fit <- model.fit %>%
  add_row(modelfit_fun(gwesprtp_attr_model))

# Goodness-of-fit (GOF)
gwesprtp_attr_model.gof <- gof(gwesprtp_attr_model)

# Simulate networks
gwesprtp_attr_model.sim <- simulate(gwesprtp_attr_model,
                                 nsim = 100,
                                 control = control.simulate.ergm(
                                   MCMC.burnin = 1000,
                                   MCMC.interval = 1000),
                                 seed = 2020)

# Get model statistics and compare to empirical network
# Reciprocity
gwesprtp_attr_model.sim %>%
  map_dbl(~asIgraph(.x) %>% 
            reciprocity()) %>% 
  unlist() %>% 
  mean()

# Triads
gwesprtp_attr_model.triads <- triad_fun(gwesprtp_attr_model.sim)

### ------------------------------- ###
# ergm controls
# Parallel computing
# ergm(control = control.ergm(seed = 2020, parallel = 3, parallel.type = "PSOCK"))

# Additional controls (with defaults, cf. Goodreau et al. 2008)
# MCMC.interval = 1024
# MCMC.burnin = MCMC.interval * 16
# MCMC.samplesize = 1024

# Plot
### ------------------------------------------------------------------------ ###
triad.df <- triad.count %>%
  pivot_longer(!triad) %>%
  mutate(empirical = if_else(name == "empirical", 1, 0),
         empirical = factor(empirical))

ggplot(triad.df, aes(x = name, y = value, fill = empirical)) +
  geom_bar(stat = "identity") +
  facet_wrap(~triad) +
  scale_fill_manual(values = c("grey50", "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
