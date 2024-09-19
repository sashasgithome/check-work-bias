setwd('C:/Users/Administrator/Desktop/BC2406 Analytics I Course Materials/Graded Team Assignment - Gender Discrimination Lawsuit')
library(data.table)
library(ggplot2)

lawsuit_dt <- fread('Lawsuit.csv')
# Graph name: "Proportion of Each Rank within Each Gender" (On page 3 of the graph)
# Calculate the count of each rank within each gender
rank_counts1 <- lawsuit_dt[, .N, by = .(Gender, Rank)]
# Calculate the proportion of each rank within each gender
rank_counts1[, prop := N / sum(N), by = Gender]
# Plot the proportion of each rank within each gender
ggplot(data = rank_counts1, aes(x = factor(Gender), y = prop, fill = factor(Rank))) +
  geom_col() +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "steelblue", "3" = "navy"), 
                    labels = c("1" = "Assistant", "2" = "Associate", "3" = "Full Professor")) +
  labs(title = "Proportion of Each Rank within Each Gender", x = "Gender", y = "Proportion", fill = "Rank") +
  scale_x_discrete(labels = c("0" = "Female", "1" = "Male")) +
  theme_minimal()


# Graph of "Experience vs Rank by Gender" (on page 3)
# Calculate average experience by Rank and Gender
avg_experience <- aggregate(Exper ~ Rank + Gender, data = lawsuit_dt, FUN = mean)
ggplot(data = lawsuit_dt, aes(x = factor(Rank), y = Exper, fill = factor(Gender))) +
  geom_boxplot() +
  scale_fill_manual(values = c("1" = "skyblue", "0" = "lightcoral"), labels = c("1" = "Male", "0" = "Female")) +
  labs(title = "Experience vs Rank by Gender",
       x = "Rank",
       y = "Experience (years)",
       fill = "Gender") +
  theme_minimal()

# Graph of "Average Salary Increase by Rank and Gender" (On page 5)
lawsuit_dt[, Sal_incrs := (Sal95 - Sal94)]
ggplot(data = lawsuit_dt, aes(x = factor(Rank), y = Sal_incrs, fill = factor(Gender))) +
  stat_summary(fun = mean, geom = "col", position = "dodge") +
  labs(title = "Average Salary Increase by Rank and Gender", x = "Rank", y = "Average Salary Increase", fill = "Gender") +
  scale_fill_manual(values = c("1" = "skyblue", "0" = "lightcoral"), labels = c("1" = "Male", "0" = "Female")) +
  theme_minimal()
