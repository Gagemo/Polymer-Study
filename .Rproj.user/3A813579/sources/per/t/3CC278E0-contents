################################################################################
################################################################################
#########################     Hydrogel Study      ##############################
######### Greenhouse - Lovegrass - Moisture & Height Data ######################
#########################   University of Florida   ############################
#########################     Gage LaPierre      ###############################
#########################     2022 - 2025        ###############################
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
# Assuming 'Polymer Study - Lovegrass - Moisture & Height.csv' is accessible
# If running in a local R environment, adjust path: data <- read.csv("Data/Polymer Study - Lovegrass - Moisture & Height.csv")
data <- read.csv("Data/Polymer Study - Lovegrass - Moisture & Height.csv")

# Convert categorical variables
data$Treatment <- as.factor(data$Treatment)

# Summary statistics
summary_stats <- data %>%
  group_by(Treatment) %>%
  summarise(across(c(Plug.Moisture, Sand.Moisture, Height, Leaf.Height, Height.Difference),
                   list(mean = mean, sd = sd, median = median, min = min, max = max), na.rm = TRUE))
print(summary_stats)

# Check for normality
shapiro_results <- list(
  Plug.Moisture = shapiro.test(data$Plug.Moisture)$p.value,
  Sand.Moisture = shapiro.test(data$Sand.Moisture)$p.value,
  Height = shapiro.test(na.omit(data$Height))$p.value,
  Leaf.Height = shapiro.test(data$Leaf.Height)$p.value,
  Height.Difference = shapiro.test(data$Height.Difference)$p.value
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

# Define a Named Vector for Y-axis Labels
y_labels <- c(
  "Plug.Moisture" = "Plug moisture (%)",
  "Sand.Moisture" = "Sand moisture (%)",
  "Height" = "Plant height (cm)",
  "Leaf.Height" = "Leaf height (cm)",
  "Height.Difference" = "Height difference (cm)"
)

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
      axis.text.x = element_blank(), # Set x-axis tick labels to blank by default
      axis.text.y = element_text(size = 12 * 2, color = "black"), # Black y-axis ticks
      axis.title.x = element_blank(), # Set x-axis title to blank by default
      axis.title.y = element_text(size = 12 * 2, color = "black") # Black y-axis label
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
boxplots <- lapply(names(anova_results), function(metric) {
  create_boxplot(metric, anova_results[[metric]], test_type[[metric]], metric)
})

# --- MODIFICATION: Control x-axis labels for combined plot ---
n_plots <- length(boxplots)
n_cols <- 3 # Desired number of columns
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
for (i in seq_along(boxplots)) {
  if (i %in% plots_to_keep_labels) {
    boxplots[[i]] <- boxplots[[i]] +
      theme(axis.text.x = element_text(size = 12 * 2, color = "black"), # Add x-axis tick labels back
            axis.title.x = element_text(size = 12 * 2, color = "black")) # Add x-axis title back
  }
}


# Print all plots (as before)
do.call(grid.arrange, c(boxplots, ncol=n_cols))

# Save all boxplots into one high-quality figure
combined_plot <- do.call(grid.arrange, c(boxplots, ncol=n_cols))

ggsave(
  filename = "Figures/Greenhouse/combined_greenhouse_boxplots.png",
  plot = combined_plot,
  width = 15, # Adjust width as needed based on ncol and number of plots
  height = 11, # Adjust height as needed
  dpi = 600,
  bg = "white" # Set background to white for the combined figure
)
print("\nAll greenhouse boxplots combined and saved to 'Figures/Greenhouse/combined_greenhouse_boxplots.png'")
