# ========================================================================================================
# Purpose:      Gender Lawsuit Discrimination - Female Doctors Representative
# Author:       Sasha Annabel (Slides 6,7,8)
# Topics:       BC2406 Summer Course
# Data Source:  Lawsuit.csv
# Packages:     data.table  
#=========================================================================================================

# Loading any necessary libraries
install.packages("data.table")
library(data.table)
library(dplyr)
library(ggplot2)

# Loading the data
lawsuit_data <- fread("C:/Users/sasha/Documents/RDirectory/Lawsuit.csv") #please change this during testing


#===========================GENDER DISTRIBUTION OF THOSE BOARD-CERTIFIED WHO IS IN CLINICAL EMPHASIS (Slide 6)

# Load necessary libraries
library(data.table)
library(ggplot2)

# Load the data
lawsuit_data <- fread("C:/Users/sasha/Documents/RDirectory/Lawsuit.csv")

# Rename Clin values for clarity
lawsuit_data$Clin <- factor(lawsuit_data$Clin, levels = c(0, 1), labels = c("Research Emphasis", "Clinical Emphasis"))

# Filter the dataset for board-certified individuals
lawsuit_cert <- subset(lawsuit_data, Cert == 1)

# Group the data by gender and clinical emphasis and count the number of individuals
gender_clin_dist <- as.data.frame(table(lawsuit_cert$Gender, lawsuit_cert$Clin))
colnames(gender_clin_dist) <- c("Gender", "Clin", "Count")

# Calculate percentages
gender_clin_dist <- within(gender_clin_dist, {
  Total <- ave(Count, Gender, FUN = sum)
  Percentage <- Count / Total * 100
})

# Print the distribution
print(gender_clin_dist)

# Create pie charts for each gender
gender_clin_dist$Clin <- as.factor(gender_clin_dist$Clin)
gender_clin_dist$Gender <- as.factor(gender_clin_dist$Gender)

# Plot for females
female_dist <- subset(gender_clin_dist, Gender == 0)
female_pie <- ggplot(female_dist, aes(x = "", y = Count, fill = Clin)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Emphasis for Board-Certified Females", x = "", y = "") +
  scale_fill_manual(name = "Emphasis", values = c("Research Emphasis" = "lightblue", "Clinical Emphasis" = "coral")) +
  theme_void() +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 7)

# Plot for males
male_dist <- subset(gender_clin_dist, Gender == 1)
male_pie <- ggplot(male_dist, aes(x = "", y = Count, fill = Clin)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Emphasis for Board-Certified Males", x = "", y = "") +
  scale_fill_manual(name = "Emphasis", values = c("Research Emphasis" = "lightblue", "Clinical Emphasis" = "coral")) +
  theme_void() +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 7)

# Print the pie charts
print(female_pie)
print(male_pie)


#===========================MEDIAN SALARY COMPARISON BY CLINICAL/RESEARCH EMPHASIS (Slide 7)

lawsuit_data <- fread("C:/Users/sasha/Documents/RDirectory/Lawsuit.csv")
lawsuit_data

#summarizing data by Clinical/Research emphasis, and GET THE MEDIAN SALARY
median_salary_emphasis <- lawsuit_data %>%
  group_by(Clin) %>%
  summarise(Median_Sal94 = median(Sal94), Median_Sal95 = median(Sal95))

median_salary_emphasis

#renaming Clin values to give meaning
lawsuit_data$Clin <- factor(lawsuit_data$Clin, levels = c(0, 1), labels = c("Research Emphasis", "Clinical Emphasis"))

#summarize data by Clinical/Research emphasis to calculate the median salary
median_salary_emphasis <- lawsuit_data %>%
  group_by(Clin) %>%
  summarise(Median_Sal94 = median(Sal94), Median_Sal95 = median(Sal95))

#plotting the Median Salary for 1994
median_sal94_plot <- ggplot(median_salary_emphasis, aes(x = Clin, y = Median_Sal94, fill = Clin)) +
  geom_bar(stat = "identity") +
  labs(title = "Median Salary Comparison (1994)", x = "Emphasis", y = "Median Salary (1994)") +
  scale_fill_manual(name = "Emphasis", values = c("Research Emphasis" = "lightblue", "Clinical Emphasis" = "coral"))

median_sal94_plot

#plotting the Median Salary for 1995
median_sal95_plot <- ggplot(median_salary_emphasis, aes(x = Clin, y = Median_Sal95, fill = Clin)) +
  geom_bar(stat = "identity") +
  labs(title = "Median Salary Comparison (1995)", x = "Emphasis", y = "Median Salary (1995)") +
  scale_fill_manual(name = "Emphasis", values = c("Research Emphasis" = "lightblue", "Clinical Emphasis" = "coral"))

median_sal95_plot


#===========================MEDIAN SALARY COMPARISON BY EMPHASIS, THEN BY GENDER (Slide 8)

lawsuit_data <- fread("C:/Users/sasha/Documents/RDirectory/Lawsuit.csv")

# Rename Clin values for clarity
lawsuit_data$Clin <- factor(lawsuit_data$Clin, levels = c(0, 1), labels = c("Research Emphasis", "Clinical Emphasis"))

# Rename departments
dept_names <- c(
  "1" = "Biochem/Molc. Bio",
  "2" = "Physiology",
  "3" = "Genetics",
  "4" = "Pediatrics",
  "5" = "Medicine",
  "6" = "Surgery"
)
lawsuit_data$Dept <- factor(lawsuit_data$Dept, levels = names(dept_names), labels = dept_names)

# Calculate median salary for male AND female in research emphasis per department
median_salary_research <- aggregate(Sal95 ~ Dept + Gender, data = subset(lawsuit_data, Clin == "Research Emphasis"), median, na.rm = TRUE)
colnames(median_salary_research)[3] <- "Median_Salary_95"

# Calculate median salary for male AND female in clinical emphasis per department
median_salary_clinical <- aggregate(Sal95 ~ Dept + Gender, data = subset(lawsuit_data, Clin == "Clinical Emphasis"), median, na.rm = TRUE)
colnames(median_salary_clinical)[3] <- "Median_Salary_95"

# Create a bar plot for median salary comparison of females and males IN RESEARCH ONLY per department
median_salary_research_plot <- ggplot(median_salary_research, aes(x = factor(Dept), y = Median_Salary_95, fill = factor(Gender))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.5) +
  labs(title = "Median Salary Comparison (1995) for Research Emphasis", x = "Department", y = "Median Salary (1995)") +
  scale_fill_manual(name = "Gender", values = c("0" = "brown2", "1" = "blue3"), labels = c("Female", "Male")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 16))

# Print the plot
print(median_salary_research_plot)


#===========================MEDIAN AND MEAN PUBLICATION RATE COMPARISON GRAGPH (Slide 8)

lawsuit_data <- fread("C:/Users/sasha/Documents/RDirectory/Lawsuit.csv")

# Rename Clin values for clarity
lawsuit_data$Clin <- factor(lawsuit_data$Clin, levels = c(0, 1), labels = c("Research Emphasis", "Clinical Emphasis"))

# Filter the dataset for research emphasis
research_emphasis <- lawsuit_data[lawsuit_data$Clin == "Research Emphasis", ]

# Calculate mean and median publication rate for males and females in research emphasis
publication_rate <- aggregate(Prate ~ Gender, data = research_emphasis, 
                              FUN = function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE)))

# Split the combined mean and median values into separate columns
publication_rate <- data.frame(publication_rate[, 1], publication_rate[, 2])
colnames(publication_rate) <- c("Gender", "Mean_Prate", "Median_Prate")

# Print the publication rates
print(publication_rate)

# Create a bar plot to compare MEAN publication rates of those in RESEARCH EMPHASIS per gender
mean_prate_plot <- ggplot(publication_rate, aes(x = factor(Gender), y = Mean_Prate, fill = factor(Gender))) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Mean Publication Rate from Research Emphasis", x = "Gender", y = "Mean Publication Rate") +
  scale_fill_manual(name = "Gender", values = c("0" = "brown2", "1" = "blue3"), labels = c("Female", "Male")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 16))

# Create a bar plot to compare MEDIAN publication rates of those in RESEARCH EMPHASIS per gender
median_prate_plot <- ggplot(publication_rate, aes(x = factor(Gender), y = Median_Prate, fill = factor(Gender))) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Median Publication Rate from Research Emphasis", x = "Gender", y = "Median Publication Rate") +
  scale_fill_manual(name = "Gender", values = c("0" = "brown2", "1" = "blue3"), labels = c("Female", "Male")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 16))

# Print the plots
print(mean_prate_plot)
print(median_prate_plot)




