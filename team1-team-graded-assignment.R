#GRADED TEAM ASSIGNMENT PART 1 
#Compare Total Personnel by Gender in Each Department

#install necessary libraries
install.packages("data.table")
library(data.table)
library(dplyr)
library(ggplot2)

#read the .csv data (change the path accordingly guys)
lawsuit_data <- fread("C:/Users/sasha/Documents/RDirectory/Lawsuit.csv")
lawsuit_data


#renaming the Rank from 1,2,3 to Assistant, Associate, and Full Professor
lawsuit_data$Rank <- factor(lawsuit_data$Rank, levels = c(1, 2, 3), labels = c("Assistant", "Associate", "Full Professor"))

#renaming the departments from 1,2,3,.. to their actual meaningful names 
dept_names <- c(
  "1" = "Biochem/Molc. Biology",
  "2" = "Physiology",
  "3" = "Genetics",
  "4" = "Pediatrics",
  "5" = "Medicine",
  "6" = "Surgery"
)
lawsuit_data$Dept <- factor(lawsuit_data$Dept, levels = names(dept_names), labels = dept_names)

#summarizing only the Department, Gender, Rank, and Total Personnel to show
lawsuit_data2 <- lawsuit_data %>%
  group_by(Dept, Gender, Rank) %>%
  summarise(Total = n())

lawsuit_data2

#plotting all those ranked ASSISTANT in each department
assistant_plot <- ggplot(subset(lawsuit_data2, Rank == "Assistant"), aes(x = factor(Dept), y = Total, fill = factor(Gender))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Male and Female per Department : Assistant Rank", x = "Department", y = "Total Persons") +
  scale_fill_manual(name = "Gender", values = c("0" = "red", "1" = "blue"), labels = c("Female", "Male"))

assistant_plot

#plotting all those ranked ASSOCIATE in each department
associate_plot <- ggplot(subset(lawsuit_data2, Rank == "Associate"), aes(x = factor(Dept), y = Total, fill = factor(Gender))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Male and Female per Department : Associate Rank", x = "Department", y = "Total Persons") +
  scale_fill_manual(name = "Gender", values = c("0" = "red", "1" = "blue"), labels = c("Female", "Male"))

associate_plot

#plotting all those ranked FULL PROFESSOR in each department
professor_plot <- ggplot(subset(lawsuit_data2, Rank == "Full Professor"), aes(x = factor(Dept), y = Total, fill = factor(Gender))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Male and Female per Department : Professor Rank", x = "Department", y = "Total Persons") +
  scale_fill_manual(name = "Gender", values = c("0" = "red", "1" = "blue"), labels = c("Female", "Male"))

professor_plot
