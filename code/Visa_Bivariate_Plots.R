# Descriptive statistics
### ------------------------------------------------------------------------ ###
# Run Visa_Analysis.R until ERGM (~line 148)

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("patchwork")

# Join network statistics to node attributes
visa_imp.df <- visa_imp.df %>%
  left_join(y = visa_degree.df, by = c("destination_iso3" = "country"))

# Create a dataframe containing plotting variables
plot.df <- expand_grid(iv = c("gdp_log","polity2", "nterror_log"), 
                       uv = c("outdegree","indegree"))

# Plotting function
scatter_fun = function(x, y) {
  ggplot(data = visa_imp.df, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point() +
    geom_smooth(method = "lm", color = "black") +
    theme_minimal() +
    labs(x = x,
         y = y,
         title = paste0("Scatterplot of ", y, " and ", x))
}

# Plots
plot.df <- plot.df %>%
  mutate(plots = map2(.x = iv, .y = uv, ~scatter_fun(.x, .y)))

# Use patchwork to combine plots
wrap_plots(plot.df$plots)
