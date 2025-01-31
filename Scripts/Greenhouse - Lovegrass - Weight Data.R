################################################################################
################################################################################
#########################      Polymer Study      ##############################
##################   Greenhouse - Lovegrass - Weight Data ######################
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
data <- read.csv("Data/Polymer Study - Lovegrass Weight.csv")

# Convert categorical variables
data$Species <- as.factor(data$Species)
data$Treatment <- as.factor(data$Treatment)

# Summary statistics
summary(data)

# Check for normality
shapiro.test(data$Total.Weight)
shapiro.test(data$Root.Weight)
shapiro.test(data$Shoot.Weight)
shapiro.test(data$Lost.Weight) # Data does not fit normalality
shapiro.test(data$Lost.Weight..) # Data does not fit normalality
shapiro.test(data$Root..) # Data does not fit normalality
shapiro.test(data$Shoot..) # Data does not fit normalality

# ANOVA
model.Total.Weight <- aov(Total.Weight ~ Treatment, data = data)
summary(model.Total.Weight)
model.Root.Weight <- aov(Root.Weight ~ Treatment, data = data)
summary(model.Root.Weight)
model.Shoot.Weight <- aov(Shoot.Weight ~ Treatment, data = data)
summary(model.Shoot.Weight)

# Kruskal-Wallis test to compare treatments (non-parametric alternative to ANOVA)
model.Lost.Weight <- kruskal.test(Lost.Weight ~ Treatment, data = data)
model.Lost.Weight
model.Lost.Weight.. <- kruskal.test(Lost.Weight.. ~ Treatment, data = data)
model.Lost.Weight..
model.Root.Weight.. <- kruskal.test(Root.. ~ Treatment, data = data)
model.Root.Weight..
model.Shoot.Weight.. <- kruskal.test(Shoot.. ~ Treatment, data = data)
model.Shoot.Weight..

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
boxplot_Total.Weight <- create_boxplot("Total.Weight", summary(model.Total.Weight)[[1]][["Pr(>F)"]][1], "Total Weight (g)")
boxplot_Root.Weight <- create_boxplot("Root.Weight", summary(model.Root.Weight)[[1]][["Pr(>F)"]][1], "Root Weight (g)")
boxplot_Shoot.Weight <- create_boxplot("Shoot.Weight", summary(model.Shoot.Weight)[[1]][["Pr(>F)"]][1], "Shoot Weight (g)")
boxplot_Lost.Weight <- create_boxplot("Lost.Weight", model.Lost.Weight$p.value, "Lost Weight (g)")
boxplot_Lost.Weight.. <- create_boxplot("Lost.Weight..", model.Lost.Weight..$p.value, "Lost Weight (%)")
boxplot_Root.. <- create_boxplot("Root..", model.Root.Weight..$p.value, "Root (%)")
boxplot_Shoot.. <- create_boxplot("Shoot..", model.Shoot.Weight..$p.value, "Shoot (%)")

# Print all plots
gridExtra::grid.arrange(boxplot_Total.Weight, boxplot_Root.Weight, boxplot_Shoot.Weight, 
                        boxplot_Lost.Weight, boxplot_Lost.Weight.., boxplot_Root.., boxplot_Shoot.., 
                        ncol=2)

