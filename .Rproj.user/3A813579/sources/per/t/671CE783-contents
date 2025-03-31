library(tidyverse)
library(rstatix)

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
ggplot(plot_data, aes(x = interaction(Year, Species), y = Proportion_Dead, fill = Polymer)) + # changed x axis and fill
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
