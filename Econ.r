# Load Required Libraries
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(WDI)

# Import Data
# Fetch GDP growth, Gini index (income inequality), and trade (% of GDP) from the World Bank
data <- WDI(
  country = "all",
  indicator = c("NY.GDP.MKTP.CD", "SI.POV.GINI", "NE.TRD.GNFS.ZS"),
  start = 2000,
  end = 2020
)

# Rename Columns for Clarity
colnames(data) <- c("Country", "Country_Code", "Year", "GDP", "Gini", "Trade")

# Filter Developing Economies (Using World Bank Income Classification)
# Fetch list of countries classified as "developing" for simplicity
developing_countries <- c("India", "Pakistan", "Nigeria", "Bangladesh", "Kenya", "Ethiopia")
data <- data %>% filter(Country %in% developing_countries)

# Data Cleaning
data_clean <- data %>%
  drop_na(Gini, Trade) %>% # Remove missing values
  filter(GDP > 0, Gini > 0, Trade > 0) # Filter out invalid data

# View Cleaned Data
head(data_clean)

# Visualization: Trade vs Gini Index
ggplot(data_clean, aes(x = Trade, y = Gini, color = Country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Trade (% of GDP) vs Gini Index in Developing Economies",
    x = "Trade (% of GDP)",
    y = "Gini Index (Income Inequality)"
  ) +
  theme_minimal()

# Statistical Analysis: Linear Regression
model <- lm(Gini ~ Trade, data = data_clean)
summary(model)

# Summary and Insights
cat("Statistical Summary:")
print(summary(model))

cat("\nKey Insights:")
if (summary(model)$coefficients[2, 4] < 0.05) {
  cat("\nThere is a statistically significant relationship between trade and income inequality.")
} else {
  cat("\nThere is no statistically significant relationship between trade and income inequality.")
}

# Export Cleaned Data
write_csv(data_clean, "Trade_IncomeInequality_DevelopingEconomies.csv")
