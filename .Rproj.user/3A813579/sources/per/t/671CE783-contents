#########################     Installs Packages   ##############################
list.of.packages <- c("tidyverse", "car", "gridextra", "rstatix", "emmeans",
                      "afex", "lmerTest", "lme4")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(rstatix) 
library(lme4)    
library(lmerTest) 
library(afex)    
library(emmeans) 

# Load the dataset
data <- read.csv("Data/Polymer Study - Field Data.csv")

# Convert categorical variables
data$Species <- as.factor(data$Species)
data$Polymer <- as.factor(data$Polymer)
data$Year <- as.factor(data$Year)

# Replace 'Dead' with NA in 'Height' and create a 'Dead' indicator
data$Dead <- ifelse(data$Height == "Dead", 1, 0)
data$Height[data$Height == "Dead"] <- NA
data$Height <- as.numeric(data$Height)

# Calculate proportion of dead plants, excluding "0" Year
dead_proportion <- data %>%
  filter(Year != "0") %>%
  group_by(Species, Polymer, Year) %>%
  summarise(
    Total_Plants = n(),
    Dead_Plants = sum(Dead, na.rm = TRUE),
    Proportion_Dead = Dead_Plants / Total_Plants,
    .groups = "drop"
  )

print("Proportion of Dead Plants:")
print(dead_proportion)

# Filter out rows where all plants are dead or alive for logistic regression, exclude 0 month
data_filtered <- data %>%
  filter(Year != "0") %>%
  group_by(Species, Polymer, Year) %>%
  filter(n_distinct(Dead) > 1) %>%
  ungroup()

# Logistic Regression
logistic_results <- data_filtered %>%
  group_by(Species, Polymer) %>%
  do(model = glm(Dead ~ Year, family = binomial, data = .)) %>%
  summarise(
    Species = first(Species),
    Polymer = first(Polymer),
    tidy(model),
    .groups = "drop"
  ) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Sig = ifelse(p.value < 0.05, "*", ""),
    Odds_Ratio = exp(estimate)
  )

print("\nLogistic Regression Results:")
print(logistic_results)

# Combine data for plotting
plot_data <- dead_proportion %>%
  left_join(logistic_results, by = c("Species" = "Species", "Polymer" = "Polymer"))

# Create the plot
ggplot(plot_data, aes(x = interaction(Year, Species), y = Proportion_Dead, 
                      fill = Polymer)) + # changed x axis and fill
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("lightblue", "#FFA07A")) + # colors by polymer
  theme_classic() +
  labs(
    x = "Year - Species", # changed x axis label
    y = "Proportion Dead"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 1) +
  geom_text(
    aes(label = Sig, y = Proportion_Dead + 0.05),
    position = position_dodge(width = 0.9),
    vjust = 0
  )

## Growth Height ##
data_height_all_years <- data %>%
  filter(!is.na(Height)) # Exclude rows where Height is NA (dead plants)

print("\n--- Growth Height Analysis (Including Year 0) ---")

# 1. Descriptive Statistics for Height (Including Year 0)
height_summary_all_years <- data_height_all_years %>%
  group_by(Species, Polymer, Year, Site) %>%
  summarise(
    Mean_Height = mean(Height, na.rm = TRUE),
    SD_Height = sd(Height, na.rm = TRUE),
    N_Plants = n(),
    .groups = "drop"
  )

print("\nMean and Standard Deviation of Plant Heights (Including Year 0):")
print(height_summary_all_years)


# 2. Linear Mixed-Effects Model for Height (Now with Year 0 as a baseline in factor 'Year')
# Model: Height ~ Polymer * Species * Year + (1|Site) + (1|Plot_ID)
# 'Year' is treated as a factor. The intercept will represent the mean height at 'Year 0'
# for the reference levels of Polymer and Species. Coefficients for other Year levels
# will represent the change in height from 'Year 0'.

# Ensure that the combination of Site and Plot uniquely identifies a plot
data_height_all_years$Plot_ID <- interaction(data_height_all_years$Site, data_height_all_years$Plot, drop = TRUE)

# Set "0" as the reference level for Year, so comparisons are relative to baseline height.
data_height_all_years$Year <- relevel(data_height_all_years$Year, ref = "0")

tryCatch({
  model_height_lmer_all_years <- lmer(Height ~ Polymer * Species * Year + (1|Site) + (1|Plot_ID),
                                      data = data_height_all_years, REML = TRUE)
  print("\nLinear Mixed-Effects Model Summary for Height (Including Year 0):")
  print(summary(model_height_lmer_all_years))
  
  # Perform ANOVA-like tests on the mixed model to get p-values for fixed effects
  print("\nANOVA Table for Fixed Effects (Height Model, Including Year 0):")
  print(anova(model_height_lmer_all_years))
  
  # Post-hoc tests for significant interactions or main effects
  # The interpretations of emmeans will now be relative to the Year 0 baseline or differences from it.
  print("\nPost-hoc tests (example: Polymer effect within Species and Year, including Year 0):")
  emmeans_interaction_all_years <- emmeans(model_height_lmer_all_years, ~ Polymer | Species * Year)
  print(pairs(emmeans_interaction_all_years, adjust = "bonferroni"))
  
  print("\nPost-hoc tests (example: Year effect within Species and Polymer, showing changes from Year 0):")
  emmeans_year_all_years <- emmeans(model_height_lmer_all_years, ~ Year | Species * Polymer)
  # Contrast "pairwise" directly compares all levels. To see change from Year 0:
  print(pairs(emmeans_year_all_years, adjust = "bonferroni"))
  
}, error = function(e) {
  message("Could not fit mixed-effects model with all years. Error: ", e$message)
  message("This might happen if there's insufficient variance within random effects groups or if 'Plot_ID' is not granular enough relative to observations.")
})


# 3. Visualization of Growth Heights (Now includes Year 0)
# Plot mean height over years, faceted by site
ggplot(height_summary_all_years, aes(x = Year, y = Mean_Height, color = Polymer, linetype = Species, group = interaction(Polymer, Species))) +
  geom_line(linewidth = 1) +
  geom_point(aes(shape = Species), size = 3) +
  scale_color_manual(values = c("No" = "darkgreen", "Yes" = "darkorange")) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Mean Height (cm)",
    title = "Mean Plant Height Over Time by Polymer, Species, and Site (Including Year 0)",
    color = "Polymer Treatment",
    linetype = "Species",
    shape = "Species"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~ Site, scales = "free_y")

# Boxplot to visualize distribution of heights at each measurement point (Now includes Year 0)
ggplot(data_height_all_years, aes(x = Year, y = Height, fill = Polymer)) +
  geom_boxplot(position = position_dodge(0.8)) +
  scale_fill_manual(values = c("No" = "lightblue", "Yes" = "#FFA07A")) +
  facet_grid(Species ~ Site, scales = "free_y") +
  theme_classic() +
  labs(
    x = "Year",
    y = "Height (cm)",
    title = "Distribution of Plant Heights by Species, Polymer, Year, and Site (Including Year 0)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


print("\nAnalysis complete. Check the generated plots for visual insights.")
