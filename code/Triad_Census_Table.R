# Markdown table of triad census including ggplot

# Load/install packages
### ------------------------------------------------------------------------###
if (!require(xfun)) install.packages("xfun")
pkg_attach2("tidyverse", "tidygraph", "ggraph", "igraph", "gt", "kableExtra")

# Nodes
nodes.df <- tibble(
  name = c("A", "B", "C"),
  x = c(0, .5, 1),         # fix position
  y = c(0, 1, 0)
)

# Edges
# 120D
edges.df <- tibble(
  # 120U
  triads = 
    c("003", "012", "102", 
      "021D", "021U", "021C", 
      "111D", "111U", 
      "030T", "030C", 
      "201", 
      "120D", "120U", "120C",
      "210", "300"),
  edges = 
    list(
      # 003
      tibble(from = c(),
             to = c()),
      # 012
      tibble(from = c("A"),
             to = c("B")),
      # 102
      tibble(from = c("A", "B"),
             to = c("B", "A")),
      # 012D
      tibble(from = c("B", "B"),
             to = c("A", "C")),
      # 021U
      tibble(from = c("A", "C"),
             to = c("B", "B")),
      # 021C
      tibble(from = c("A", "B"),
             to = c("B", "C")),
      # 111D
      tibble(from = c("A", "B", "C"),
             to = c("B", "A", "B")),
      # 111U
      tibble(from = c("A", "B", "B"),
             to = c("B", "A", "C")),
      # 030T
      tibble(from = c("A", "C", "A"),
             to = c("B", "B", "C")),
      # 030C
      tibble(from = c("B", "C", "A"),
             to = c("A", "B", "C")),
      # 201
      tibble(from = c("A", "B", "B", "C"),
             to = c("B", "A", "C", "B")),
      # 120D
      tibble(from = c("B", "B", "A", "C"),
             to = c("A", "C", "C", "A")),
      # 120U
      tibble(from = c("A", "C", "A", "C"),
             to = c("B", "B", "C", "A")),
      # 120C
      tibble(from = c("A", "B", "A", "C"),
             to = c("B", "C", "C", "A")),
      # 210
      tibble(from = c("A", "B", "C", "A", "C"),
             to = c("B", "C", "B", "C", "A")),
      # 300
      tibble(from = c("A", "B", "B", "C", "A", "C"),
             to = c("B", "A", "C", "B", "C", "A"))
      ))

# Combine
### ------------------------------------------------------------------------###
triad.df <- tibble(
  nodes = list(rep(nodes.df, 16)),
  triad_id = edges.df$triads,
  edges = edges.df$edges
) %>%
  mutate(graphs = map2(.x = edges, .y = nodes, ~
                         graph_from_data_frame(d = .x,
                                               vertices = .y, 
                                               directed = TRUE)))

# Plot
triad.df <- triad.df %>%
  mutate(plots = map2(.x = graphs, .y = triad_id, ~
                        ggraph(graph = .x, layout = "manual", x = x, y = y) +
                        geom_node_point(size = 10) +
                        geom_edge_parallel(start_cap = circle(8, "mm"), 
                                       end_cap = circle(8, "mm"),
                                       arrow = arrow(type = "closed", 
                                                     length = unit(2, "mm"))) +
                        annotate("text", x = Inf, y = Inf, fontface = "bold", 
                                 vjust = "inward", hjust = "inward", 
                                 label = paste0(.y), size = 8) +
                        theme_graph()))

# Join actual data (created in Visa_Analysis.R)
### ------------------------------------------------------------------------###
# gt table
# Figures
table.fig <- triad.df %>%
  select(plots)

# Data
table_data.df <- triad.df %>%
  mutate(n_triad = visa_triad.df,
         triad_fig = NA) %>%
  select(triad_id, n_triad, triad_fig)
  
# Table
table.df <- table_data.df %>%
  gt::gt() %>%
  text_transform(
    locations = cells_body(vars(triad_fig)),
    fn = function(x) {
      map(table.fig$plots, ggplot_image, height = px(100))
    }
  )
