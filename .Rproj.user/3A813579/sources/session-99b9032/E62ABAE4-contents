################################################################################
################################################################################
#########################     Hydrogel Study      ##############################
##################     Greenhouse - Lovegrass - Weight Data ######################
#########################   University of Florida   ##############################
#########################     Gage LaPierre      ##############################
#########################     2022 - 2025        ##############################
################################################################################
################################################################################

######################### Clears Environment & History   ########################
rm(list=ls(all=TRUE))
cat("\014")

#########################     Installs Packages    ##############################
list.of.packages <- c("tidyverse", "car", "gridExtra")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load required libraries
library(tidyverse)
library(car)
library(gridExtra) # Make sure gridExtra is loaded for grid.arrange

# Load the dataset
# Assuming 'Polymer Study - Lovegrass Weight.csv' is accessible
# If running in a local R environment, adjust path: data <- read.csv("Data/Polymer Study - Lovegrass Weight.csv")
data <- read.csv("Data/Polymer Study - Lovegrass Weight.csv")

# Convert categorical variables
data$Treatment <- as.factor(data$Treatment)

# Summary statistics
summary_stats <- data %>%
  group_by(Treatment) %>%
  summarise(across(c(Total.Weight, Root.Weight, Shoot.Weight, Lost.Weight,
                     Lost.Weight.., Root.., Shoot..),
                   list(mean = mean, sd = sd, median = median, min = min, max = max), na.rm = TRUE))
print(summary_stats)

# Check for normality
shapiro_results <- list(
  Total.Weight = shapiro.test(data$Total.Weight)$p.value,
  Root.Weight = shapiro.test(data$Root.Weight)$p.value,
  Shoot.Weight = shapiro.test(na.omit(data$Shoot.Weight))$p.value,
  Lost.Weight = shapiro.test(na.omit(data$Lost.Weight))$p.value,
  Lost.Weight.. = shapiro.test(data$Lost.Weight..)$p.value,
  Root.. = shapiro.test(data$Root..)$p.value,
  Shoot.. = shapiro.test(data$Shoot..)$p.value
)
print(shapiro_results)

# Determine test based on normality
anova_results <- list()
test_type <- list()
for (metric in names(shapiro_results)) {
  if (shapiro_results[[metric]] > 0.05) {
    model <- aov(as.formula(paste(metric, "~ Treatment")), data = data)
    anova_results[[metric]] <- summary(model)[[1]][["Pr(>F)"]][1]
    test_type[[metric]] <- "ANOVA"
  } else {
    model <- kruskal.test(as.formula(paste(metric, "~ Treatment")), data = data)
    anova_results[[metric]] <- model$p.value
    test_type[[metric]] <- "Kruskal-Wallis"
  }
}
print(anova_results)

# Define a mapping of metrics to desired y-axis labels.
y_labels <- c(
  "Total.Weight" = "Total biomass (g)",
  "Root.Weight" = "Root biomass (g)",
  "Shoot.Weight" = "Shoot biomass (g)",
  "Lost.Weight" = "Lost biomass (g)",
  "Lost.Weight.." = "Lost biomass (%)",
  "Root.." = "Root mass ratio (%)",
  "Shoot.." = "Shoot mass ratio (%)"
)

# Function to create boxplots and annotate significance and test type
create_boxplot <- function(metric, p_value, test_used, y_label) {
  plot <- ggplot(data, aes(x = Treatment, y = !!sym(metric), fill = Treatment)) +
    geom_boxplot() +
    scale_fill_manual(values = c("lightblue", "#FFA07A")) +
    scale_x_discrete(labels = c("NP" = "Control", "P" = "Hydrogel")) +
    theme_classic(base_size = 12) +
    labs(x = NULL, y = y_labels[[metric]]) + # x-axis title is NULL for all plots initially
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      axis.text.x = element_text(size = 12 * 2, color = "black"), # Default x-axis ticks are visible
      axis.text.y = element_text(size = 12 * 2, color = "black"),
      axis.title.x = element_text(size = 12 * 2, color = "black"),
      axis.title.y = element_text(size = 12 * 2, color = "black")
    ) +
    coord_cartesian(ylim = c(NA, max(data[[metric]], na.rm = TRUE) * 1.1)) +
    annotate(
      "text",
      x = 1.5,
      y = max(data[[metric]], na.rm = TRUE) * 1,
      label = paste0(test_used, "\np = ", round(p_value, 3)),
      size = 6
    )
  return(plot)
}

# Generate boxplots
# Create a named list of plots for easier reordering
all_boxplots_named <- lapply(names(anova_results), function(metric) {
  setNames(list(create_boxplot(metric, anova_results[[metric]], test_type[[metric]], metric)), metric)
}) %>%
  unlist(recursive = FALSE) # Unlist to get a flat named list of plots

# Define the desired order of metrics
desired_order_metrics <- c(
  "Shoot.Weight",
  "Shoot..",
  "Lost.Weight",
  "Lost.Weight..",
  "Root.Weight",
  "Root..",
  "Total.Weight"
)

# Reorder the boxplots list according to the desired order
boxplots_ordered <- all_boxplots_named[desired_order_metrics]

# --- MODIFICATION: Control x-axis labels for combined plot ---
n_plots <- length(boxplots_ordered) # Use the length of the ordered list
n_cols <- 2 # Desired number of columns
n_rows <- ceiling(n_plots / n_cols)

# Identify plots that should keep their x-axis labels (bottom row of each column)
plots_to_keep_labels <- c()
for (col_idx in 1:n_cols) {
  # Get all plot indices that fall into this column when arranged row-by-row
  potential_column_indices <- seq(col_idx, n_plots, by = n_cols)
  if (length(potential_column_indices) > 0) {
    plots_to_keep_labels <- c(plots_to_keep_labels, tail(potential_column_indices, 1))
  }
}

# Modify themes for plots not in the bottom row of their respective columns
for (i in seq_along(boxplots_ordered)) { # Iterate through the ordered list
  if (!(i %in% plots_to_keep_labels)) {
    boxplots_ordered[[i]] <- boxplots_ordered[[i]] +
      theme(axis.text.x = element_blank(), # Remove x-axis tick labels
            axis.title.x = element_blank()) # Ensure x-axis title is also blank
  }
}

# Print all plots (as before, for individual display if desired)
do.call(grid.arrange, c(boxplots_ordered))

# Save all boxplots into one high-quality figure
combined_plot <- do.call(grid.arrange, c(boxplots_ordered, ncol=n_cols)) # Use the ordered list

# Save the combined plot as a high-quality PNG
ggsave(
  filename = "Figures/Greenhouse/combined_greenhouse_biomass_boxplots.png",
  plot = combined_plot,
  width = 12, # Adjusted width for 2 columns
  height = 18, # Adjusted height for 4 rows
  dpi = 600,
  bg = "white" # Set background to white for the combined figure
)
print("\nAll greenhouse biomass boxplots combined and saved to 'Figures/Greenhouse/combined_greenhouse_biomass_boxplots.png'")
