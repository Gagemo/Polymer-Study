################################################################################
################################################################################
#########################      Polymer Study      ##############################
#########################      Milkweed Data      ##############################
#########################  University of Florida  ##############################
#########################     Gage LaPierre       ##############################
#########################      2022 - 2023        ##############################
################################################################################
################################################################################

######################### Clears Environment & History  ########################
rm(list=ls(all=TRUE))
cat("\014") 

#########################     Installs Packages   ##############################
list.of.packages <- c("tidyverse", "vegan", "agricolae", "extrafont", "plotrix", 
                      "ggsignif", "multcompView", "ggpubr", "rstatix", "labdsv",
                      "tables")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

##########################     Loads Packages     ##############################
library(tidyverse)
library(lubridate)

##########################     Read in  Data       #############################
# Load the dataset
data <- read.csv("RScript/Polymer Study - Milkweed.csv")

# Convert date column to Date type
data$date <- as.Date(data$date, format = "%Y/%m/%d")

# Filter out rows with missing Height values
data_clean <- data %>% 
  filter(!is.na(Height))

# Create the plot
height_plot <- ggplot(data_clean, aes(x = date, y = Height, color = ID, group = ID)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Height Over Time",
    x = "Date",
    y = "Height (cm)",
    color = "Individual ID"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Display the plot
height_plot

# Save the plot as a file
ggsave("RScript/Figures/Height_Over_Time.png", height_plot, width = 10, height = 6)

# Calculate the change in height from start to finish by treatment
height_change <- data_clean %>% 
  group_by(Treatment, ID) %>% 
  summarize(
    Start_Height = Height[which.min(date)],
    End_Height = Height[which.max(date)],
    Change_in_Height = End_Height - Start_Height
  ) %>% 
  ungroup()

# Create the plot
change_plot <- ggplot(height_change, aes(x = Treatment, y = Change_in_Height, fill = Treatment)) +
  geom_boxplot() +
  geom_point() +
  labs(
    title = "Change in Height from Start to Finish by Treatment",
    x = "Treatment",
    y = "Change in Height (cm)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )
# Display the plot
print(change_plot)

# Save the plot as a file
ggsave("RScript/Figures/Height_Change_by_Treatment.png", change_plot, width = 10, height = 6)

aov(height_change, Change_in_Height ~ Treatment)












