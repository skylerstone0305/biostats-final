# Biostatistics Final Project ----
# Exploratory data analysis and data preparation

# load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)

# Load the data
diversity <- read.csv("data/2010FieldEcolDiversity.csv")

# inspect the data ----

str(diversity)

summary(diversity)

# create stacked histograms ----
ggplot(diversity, aes(x = SpeciesRichness, fill = as.factor(ManagementStage))) +
  geom_histogram(position = "stack", binwidth = 1, color = 'black') +
  labs(
    title = "Stacked Histograms of SpeciesRichness by Management Stage",
    x = "Species Richness",
    y = "Count",
    fill = "Management Stage"
  ) +
  theme_minimal()

# Unmanaged Areas (0 years): Typically, lower species richness due to invasive species like buckthorn dominating.

# Early-Stage Management (10 years): An intermediate increase in species richness as invasive species are removed and native species are reintroduced.

# Late-Stage Management (20 years): Highest species richness, reflecting successful restoration with sustained management efforts.

# table of descriptive statistics ----

# Calculate descriptive statistics
descriptive_stats <- diversity %>%
  group_by(ManagementStage) %>%
  summarise(
    mean = mean(SpeciesRichness),
    sd = sd(SpeciesRichness),
    cv = sd / mean,
    se = sd / sqrt(n())
  )

print(descriptive_stats)

kable(descriptive_stats)

# Calculate 95% confidence intervals
ci <- diversity %>%
  group_by(ManagementStage) %>%
  summarise(
    mean = mean(SpeciesRichness),
    se = sd(SpeciesRichness) / sqrt(n()),
    lower_ci = mean - qt(0.975, df = n() - 1) * se,
    upper_ci = mean + qt(0.975, df = n() - 1) * se
  )

print(ci)

kable(ci)
