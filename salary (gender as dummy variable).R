setwd('C:/Users/73691/Desktop/study/Analytics/BC2406 Analytics I Course Materials/Graded Team Assignment - Gender Discrimination Lawsuit')
# Load necessary library
library(dplyr)
library(ggplot2)
library(car)

# Load the dataset
data <- read.csv("Lawsuit.csv")

# Factor the Gender variable
data$Gender <- factor(data$Gender)

# Separate the data into clinical and research groups
clinical_data <- filter(data, Clin == 1)
research_data <- filter(data, Clin == 0)

# Verify that the base line reference level is female ('0')
levels(clinical_data$Gender)
levels(research_data$Gender)

# Build linear regression models for the clinical and research groups
clinical_model <- lm((Sal94 + Sal95)/2 ~ Prate + Exper + Gender, data = clinical_data)
research_model <- lm((Sal94 + Sal95)/2 ~ Prate + Exper + Gender, data = research_data)

# Check for multicollinearity
vif(clinical_model)
vif(research_model)

# Summary of models
summary(clinical_model)
summary(research_model) # Both models' R is no more than 0.7, probably the result of biased Salary dirtibution
# Use female as base line, male earn significantly more in both groups

# Visualization
coef(clinical_model)[4]
coef(research_model)[4]
data <- data.frame(
  Category = c('Avg Male Excess Income in Clinical', 'Avg Male Excess Income in Research'),
  Value = c(coef(clinical_model)[4], coef(research_model)[4])
)

ggplot(data, aes(x = Category, y = Value)) +
  geom_bar(stat = 'identity', fill = 'blue3', width = 0.4) +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5) +
  labs(x = NULL, y = 'Excess Income ($)', title = 'Male Excess Salary') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

