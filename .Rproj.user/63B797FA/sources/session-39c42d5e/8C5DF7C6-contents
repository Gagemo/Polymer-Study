################################################################################
################################################################################
#########################      Polymer Study      ##############################
######### Greenhouse - Lovegrass - Moisture & Height Data ######################
#########################  University of Florida  ##############################
#########################     Gage LaPierre       ##############################
#########################      2022 - 2025        ##############################
################################################################################
################################################################################

######################### Clears Environment & History  ########################
rm(list=ls(all=TRUE))
cat("\014") 

#########################     Installs Packages   ##############################
list.of.packages <- c("tidyverse", "car")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load required libraries
library(tidyverse)
library(car)

# Load the dataset
data <- read.csv("Data/Polymer Study - Lovegrass - Moisture & Height.csv")

# Convert categorical variables
data$Treatment <- as.factor(data$Treatment)

# Summary statistics
summary(data)

# Check for normality
shapiro.test(data$Plug.Moisture)
shapiro.test(data$Sand.Moisture)
shapiro.test(na.omit(data$Height))
shapiro.test(na.omit(data$Leaf.Height))
shapiro.test(na.omit(data$Height.Difference))

# Kruskal-Wallis test to compare treatments (non-parametric alternative to ANOVA)
model.Plug.Moisture <- kruskal.test(Plug.Moisture ~ Treatment, data = data)
model.Plug.Moisture
model.Sand.Moisture <- kruskal.test(Sand.Moisture ~ Treatment, data = data)
model.Sand.Moisture
model.Height <- kruskal.test(Height ~ Treatment, data = data)
model.Height
model.Leaf.Height <- kruskal.test(Leaf.Height ~ Treatment, data = data)
model.Leaf.Height
model.Height.Difference <- kruskal.test(Height.Difference ~ Treatment, data = data)
model.Height.Difference

# ANOVA
model.Plug.Moisture <- aov(Plug.Moisture ~ Treatment, data = data)
summary(model.Plug.Moisture)
model.Sand.Moisture <- aov(Sand.Moisture ~ Treatment, data = data)
summary(model.Sand.Moisture)
model.Height <- aov(Height ~ Treatment, data = data)
summary(model.Height)
model.Leaf.Height <- aov(Leaf.Height ~ Treatment, data = data)
summary(model.Leaf.Height)
model.Height.Difference <- aov(Height.Difference ~ Treatment, data = data)
summary(model.Height.Difference)

# Function to create boxplots and annotate significance
create_boxplot <- function(metric, p_value, y_label) {
  plot <- ggplot(data, aes(x=Treatment, y=!!sym(metric), fill=Treatment)) +
    geom_boxplot() +
    theme_classic() +
    labs(y = y_label) +
    theme(legend.position = "none")
  
  # Annotate significance if p-value is below 0.05
  if (p_value < 0.05) {
    plot <- plot + annotate("text", x=1.5, y=max(data[[metric]], na.rm=TRUE), label="*", size=8)
  }
  
  return(plot)
}

# Generate boxplots
boxplot_Plug.Moisture <- create_boxplot("Plug.Moisture", summary(model.Plug.Moisture)[[1]][["Pr(>F)"]][1], "Plug Moisture (%)")
boxplot_Sand.Moisture <- create_boxplot("Sand.Moisture", summary(model.Sand.Moisture)[[1]][["Pr(>F)"]][1], "Sand Moisture (%)")
boxplot_Height <- create_boxplot("Height", summary(model.Height)[[1]][["Pr(>F)"]][1], "Height (cm)")
boxplot_Leaf.Height <- create_boxplot("Leaf.Height", summary(model.Leaf.Height)[[1]][["Pr(>F)"]][1], "Leaf Height (cm)")
boxplot_Height.Difference <- create_boxplot("Height.Difference", summary(model.Height.Difference)[[1]][["Pr(>F)"]][1], "Height Difference (cm)")

# Print all plots
gridExtra::grid.arrange(boxplot_Plug.Moisture, boxplot_Sand.Moisture, boxplot_Height, 
                        boxplot_Leaf.Height, boxplot_Height.Difference, 
                        ncol=2)

