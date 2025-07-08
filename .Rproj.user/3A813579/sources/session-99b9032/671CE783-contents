################################################################################
################################################################################
#########################      Polymer Study      ##############################
#################### Field - Mortality & Height Data ###########################
#########################  University of Florida  ##############################
#########################     Gage LaPierre       ##############################
#########################      2022 - 2025        ##############################
################################################################################
################################################################################

#########################     Installs Packages   ##############################
list.of.packages <- c("tidyverse", "car", "gridExtra", "rstatix", "emmeans",
                      "afex", "lmerTest", "lme4", "broom", "ggpattern")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(rstatix) 
library(gridExtra)
library(lme4)    
library(lmerTest) 
library(afex)    
library(emmeans) 
library(broom)
library(ggpattern)

# Load the dataset
data <- read.csv("Data/Polymer Study - Field Data.csv")

# Convert categorical variables
# Explicitly setting the order of Year factor levels
data$Species <- as.factor(data$Species)
data$Polymer <- as.factor(data$Polymer)
data$Year <- factor(data$Year, levels = c("0", "5 Months", "11 Months")) # Set desired order
data$Site <- as.factor(data$Site)
data$Plot <- as.factor(data$Plot)

# Replace 'Dead' with NA in 'Height' and create a 'Dead' indicator
data$Dead <- ifelse(data$Height == "Dead", 1, 0)
data$Height[data$Height == "Dead"] <- NA
data$Height <- as.numeric(data$Height)

# --- Data Consistency Checks ---

print("\n--- Performing Data Consistency Checks ---")

# 1. Check for entirely duplicate rows
num_duplicates <- sum(duplicated(data))
if (num_duplicates > 0) {
  print(paste("Found", num_duplicates, "duplicate rows."))
  print(data[duplicated(data), ]) # This will show the duplicate rows
} else {
  print("No duplicate rows found.")
}

# 2. Check sample counts per Site, Plot, Year, Species, Polymer
# Expected: 5 plants per Plot, Year, Species, Polymer combination
samples_per_group <- data %>%
  group_by(Site, Plot, Year, Species, Polymer) %>%
  summarise(Count = n(), .groups = "drop")
print("\nCounts per Site, Plot, Year, Species, Polymer:")
print(samples_per_group)

# Identify if any counts are not 5
deviating_species_polymer <- samples_per_group %>%
  filter(Count != 5)
if (nrow(deviating_species_polymer) > 0) {
  print("\nDeviations from expected 5 plants per Site, Plot, Year, Species, Polymer:")
  print(deviating_species_polymer)
} else {
  print("\nAll counts are 5 plants per Site, Plot, Year, Species, Polymer (assuming no plants were dead at Year 0).")
}

# 3. Check total counts per Site, Plot, Year (across both species)
# Expected: 10 plants per Plot, Year (5 Milkweed + 5 Lovegrass)
samples_per_plot_year <- data %>%
  group_by(Site, Plot, Year, Polymer) %>%
  summarise(Total_Plot_Count = n(), .groups = "drop")
print("\nTotal counts per Site, Plot, Year, Polymer (across both species):")
print(samples_per_plot_year)

# Identify if any total plot counts are not 10
deviating_plot_year <- samples_per_plot_year %>%
  filter(Total_Plot_Count != 10)
if (nrow(deviating_plot_year) > 0) {
  print("\nDeviations from expected 10 plants per Site, Plot, Year, Polymer:")
  print(deviating_plot_year)
} else {
  print("\nAll total counts are 10 plants per Site, Plot, Year, Polymer.")
}

# 4. Check total counts per Site, Year
# Expected: 60 plants per Site, Year (6 plots * 10 plants/plot)
samples_per_site_year <- data %>%
  group_by(Site, Year, Polymer) %>%
  summarise(Total_Site_Count = n(), .groups = "drop")
print("\nTotal counts per Site, Year, Polymer:")
print(samples_per_site_year)

# Identify if any total site counts are not 60
deviating_site_year <- samples_per_site_year %>%
  filter(Total_Site_Count != 60)
if (nrow(deviating_site_year) > 0) {
  print("\nDeviations from expected 60 plants per Site, Year, Polymer:")
  print(deviating_site_year)
} else {
  print("\nAll total counts are 60 plants per Site, Year, Polymer.")
}

print("\n--- Data Consistency Checks Complete ---")

# --- Mortality Analysis ---

# Calculate proportion of dead plants, excluding "0" Year
dead_proportion <- data %>%
  filter(Year != "0") %>%
  group_by(Species, Polymer, Year, Site) %>%
  summarise(
    Total_Plants = n(),
    Dead_Plants = sum(Dead, na.rm = TRUE),
    Proportion_Dead = Dead_Plants / Total_Plants,
    .groups = "drop"
  )

print("Proportion of Dead Plants:")
print(dead_proportion)
# Save mortality proportions table
write.csv(dead_proportion, "Tables/mortality_proportions.csv", row.names = FALSE)


# Filter out rows where all plants are dead or alive for logistic regression, exclude 0 month
data_filtered_mortality <- data %>%
  filter(Year != "0") %>%
  # Add this line to explicitly set "5 Months" as the reference level for 'Year'
  mutate(Year = relevel(Year, ref = "5 Months")) %>%
  group_by(Species, Polymer, Year, Site) %>%
  filter(n_distinct(Dead) > 1) %>%
  ungroup()

# Logistic Regression
logistic_results <- data_filtered_mortality %>%
  group_by(Species, Polymer) %>%
  do(model = glm(Dead ~ Year, family = binomial, data = .)) %>%
  summarise(
    Species = first(Species),
    Polymer = first(Polymer),
    broom::tidy(model),
    .groups = "drop"
  ) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Sig = ifelse(p.value < 0.05, "*", ""),
    Odds_Ratio = exp(estimate)
  )

print("\n--- Mortality Analysis: Logistic Regression Results (Effect of Year on Mortality) ---")
print(logistic_results)
# Save logistic regression results table
write.csv(logistic_results, "Tables/mortality_logistic_regression_results.csv", row.names = FALSE)

# Plotting Mortality
plot_data_mortality <- dead_proportion %>%
  left_join(logistic_results, by = c("Species" = "Species", "Polymer" = "Polymer"))

ggplot(plot_data_mortality, aes(x = Year, y = Proportion_Dead, fill = Polymer, group = interaction(Polymer, Species), pattern = Species)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(width = 1), linewidth = 0.5,
                   color = "black", # Bar outlines are black
                   pattern_fill = "white", # Pattern fill is transparent
                   pattern_color = "black", # Pattern lines are black
                   pattern_density = 0.4,
                   pattern_spacing = 0.05) + # Slightly increased spacing for visibility
  scale_fill_manual(values = c("lightblue", "#FFA07A"),
                    labels = c("No" = "Control", "Yes" = "Hydrogel")) + # Changed labels here
  scale_pattern_manual(values = c("G" = "none", "M" = "stripe"),
                       labels = c("G" = "Lovegrass", "M" = "Milkweed")) +
  theme_classic(base_size = 14) + # Increased base font size by 2 (default is often 12)
  labs(
    x = NULL, # Removed x-axis title
    y = "Proportion dead", # Changed y-axis label to lowercase 'dead'
    title = NULL, # Removed figure title
    fill = "Treatment"
    # Removed pattern = "Species" from labs to remove its legend title
  ) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + # Changed angle to 0 and hjust to 0.5 for horizontal
  ylim(0, 1) +
  geom_text(
    aes(label = Sig, y = Proportion_Dead + 0.05),
    position = position_dodge(width = 0.1),
    vjust = 0,
    size = 4 # Adjust text size for significance labels if needed, default is 3.88
  ) +
  facet_wrap(~ Site) +
  guides(fill = guide_legend(override.aes = list(pattern = "none")), # Removes pattern from the fill legend
         pattern = "none") # Completely removes the pattern legend

ggsave("Figures/Field/Mortality.jpg", dpi = 800)

# --- Growth Height Analysis (Including Year 0) ---
data_height_all_years <- data %>%
  filter(!is.na(Height))

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
# Save height summary table
write.csv(height_summary_all_years, "Tables/height_descriptive_statistics.csv", row.names = FALSE)


# 2. Linear Mixed-Effects Model for Height (Now with Year 0 as a baseline in factor 'Year')
data_height_all_years$Plot_ID <- interaction(data_height_all_years$Site, data_height_all_years$Plot, drop = TRUE)
# Removed relevel as factor levels are now set globally
# data_height_all_years$Year <- relevel(data_height_all_years$Year, ref = "0")

tryCatch({
  model_height_lmer_all_years <- lmer(Height ~ Polymer * Species * Year + (1|Site) + (1|Plot_ID),
                                      data = data_height_all_years, REML = TRUE)
  print("\n--- Growth Height Analysis: Linear Mixed-Effects Model Summary (Including Year 0) ---")
  model_summary <- summary(model_height_lmer_all_years)
  print(model_summary)
  # Save mixed-effects model summary (fixed effects part)
  write.csv(as.data.frame(model_summary$coefficients), "Tables/height_lmer_fixed_effects_summary.csv", row.names = TRUE)
  
  
  print("\n--- Growth Height Analysis: ANOVA Table for Fixed Effects (Including Year 0) ---")
  # This table directly shows the p-values for main effects and interactions.
  anova_table <- anova(model_height_lmer_all_years)
  print(anova_table)
  # Save ANOVA table
  write.csv(as.data.frame(anova_table), "Tables/height_lmer_anova_table.csv", row.names = TRUE)
  
  
  print("\n--- Growth Height Analysis: Post-hoc tests (Polymer effect within Species and Year) ---")
  emmeans_polymer_effect <- emmeans(model_height_lmer_all_years, ~ Polymer | Species * Year)
  polymer_contrasts <- pairs(emmeans_polymer_effect, adjust = "bonferroni")
  print(polymer_contrasts)
  # Save polymer post-hoc contrasts
  write.csv(as.data.frame(polymer_contrasts), "Tables/height_lmer_polymer_posthoc.csv", row.names = FALSE)
  
  
  print("\n--- Growth Height Analysis: Post-hoc tests (Year effect within Species and Polymer) ---")
  emmeans_year_effect <- emmeans(model_height_lmer_all_years, ~ Year | Species * Polymer)
  year_contrasts <- pairs(emmeans_year_effect, adjust = "bonferroni")
  print(year_contrasts)
  # Save year post-hoc contrasts
  write.csv(as.data.frame(year_contrasts), "Tables/height_lmer_year_posthoc.csv", row.names = FALSE)
  
}, error = function(e) {
  message("Could not fit mixed-effects model with all years. Error: ", e$message)
  message("This might happen if there's insufficient variance within random effects groups or if 'Plot_ID' is not granular enough relative to observations.")
})

# 3. Visualization of Growth Heights (Now includes Year 0)
ggplot(height_summary_all_years, aes(x = Year, y = Mean_Height, color = Polymer, 
                                     linetype = Species, 
                                     group = interaction(Polymer, Species))) +
  geom_line(linewidth = 1) +
  geom_point(aes(shape = Species), size = 3) +
  scale_color_manual(values = c("No" = "lightblue", "Yes" = "salmon")) +
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

ggsave("Figures/Field/FieldHeight.jpg")

ggplot(data_height_all_years, aes(x = Year, y = Height, fill = Polymer, 
                                  pattern = Species)) +
  geom_boxplot_pattern(linewidth = 0.5, color = "black", # Bar outlines are black
                   pattern_fill = "white", # Pattern fill is transparent
                   pattern_color = "black", # Pattern lines are black
                   pattern_density = 0.4,
                   pattern_spacing = 0.05) + # Slightly increased spacing for visibility
  scale_fill_manual(values = c("lightblue", "#FFA07A"),
                    labels = c("No" = "Control", "Yes" = "Hydrogel")) + # Changed labels here
  scale_pattern_manual(values = c("G" = "none", "M" = "stripe"),
                       labels = c("G" = "Lovegrass", "M" = "Milkweed")) +
  theme_classic(base_size = 14) + # Increased base font size by 2 (default is often 12)
  labs(
    x = NULL, # Removed x-axis title
    y = "Height (cm)", # Changed y-axis label to lowercase 'dead'
    title = NULL, # Removed figure title
    fill = "Treatment"
    # Removed pattern = "Species" from labs to remove its legend title
  ) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + # Changed angle to 0 and hjust to 0.5 for horizontal
  facet_wrap(~ Site) +
  guides(fill = guide_legend(override.aes = list(pattern = "none")), # Removes pattern from the fill legend
         pattern = "none") # Completely removes the pattern legend


ggsave("Figures/Field/HeightAllYears.jpg", dpi = 600)

print("\nAnalysis complete. Check the generated plots for visual insights.")

