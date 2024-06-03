# Biostatistics final project ----
# Hypothesis Testing based on Confidence Intervals

# load libraries
library(tidyverse)
library(dplyr)
library(knitr)
library(tidyr)

# load data
diversity <- read.csv("data/2010FieldEcolDiversity.csv")

# Hypothesis Testing ----
# Subset data for 10years and 20years
data_10years <- diversity %>%
  filter(ManagementStage == "10years")

data_20years <- diversity %>%
  filter(ManagementStage == "20years")

# Perform two-sample t-test
t_test <- t.test(data_10years$SpeciesRichness, data_20years$SpeciesRichness, var.equal = FALSE)

print(t_test)

# Perform Mann-Whitney U-test
wilcox_test <- wilcox.test(data_10years$SpeciesRichness, data_20years$SpeciesRichness)

print(wilcox_test)

# Perform ANOVA
anova_results <- aov(SpeciesRichness ~ as.factor(ManagementStage), data = diversity)
summary(anova_results)

# Perform Tukey HSD test
tukey_results <- TukeyHSD(anova_results)

print(tukey_results)

# Perform Kruskal-Wallis test
kruskal_test <- kruskal.test(SpeciesRichness ~ as.factor(ManagementStage), data = diversity)

print(kruskal_test)

# Plots ----

# Stacked histograms for AverageC
ggplot(diversity, aes(x = AverageC, fill = as.factor(ManagementStage))) +
  geom_histogram(position = "stack", binwidth = 0.5, color = "black") +
  labs(
    title = "Stacked Histograms of AverageC by Management Stage",
    x = "Average C",
    y = "Count",
    fill = "Management Stage"
  ) +
  theme_minimal()

# Table of means for SpeciesRichness and AverageC
means_table <- diversity %>%
  group_by(ManagementStage) %>%
  summarise(
    mean_SpeciesRichness = mean(SpeciesRichness),
    mean_AverageC = mean(AverageC)
  )

print(means_table)


# Perform one-sample t-test
one_sample_test <- t.test(data_20years$AvgOverSppC, mu = 1)

print(one_sample_test)

# Perform bootstrapping
set.seed(05292024)

boot_results <- replicate(10000, {
  sample_data <- sample(data_20years$AvgOverSppC, replace = TRUE)
  mean(sample_data)
})

# Calculate confidence intervals
boot_ci <- quantile(boot_results, c(0.025, 0.975))

# Plot histogram of bootstrapped means
hist(boot_results, main = "Bootstrapped Distribution of AvgOverSppC", xlab = "AvgOverSppC")

# Print bootstrapped mean and confidence intervals
boot_mean <- mean(boot_results)
print(c(boot_mean, boot_ci))

# Calculate correlation
cor_results <- cor.test(diversity$TotalCover, diversity$SpeciesRichness)

# Scatterplot
ggplot(diversity, aes(x = SpeciesRichness, y = TotalCover)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Scatterplot of TotalCover vs. SpeciesRichness",
    x = "Species Richness",
    y = "Total Cover"
  ) +
  theme_minimal()

print(cor_results)

# Linear regression
lm_model <- lm(TotalCover ~ SpeciesRichness, data = diversity)
summary(lm_model)

# Log-transformed regression
log_lm_model <- lm(log(TotalCover + 1) ~ log(SpeciesRichness + 1), data = diversity)
summary(log_lm_model)

# QQ-plot of residuals
qqnorm(resid(log_lm_model))
qqline(resid(log_lm_model))

# Scatterplot of log-transformed data
ggplot(diversity, aes(x = log(SpeciesRichness + 1), y = log(TotalCover + 1))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Scatterplot of Log-Transformed TotalCover vs. Log-Transformed SpeciesRichness",
    x = "Log(Species Richness + 1)",
    y = "Log(Total Cover + 1)"
  ) +
  theme_minimal()

