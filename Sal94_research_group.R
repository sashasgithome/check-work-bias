setwd('C:/Users/73691/Desktop/study/Analytics/BC2406 Analytics I Course Materials/Graded Team Assignment - Gender Discrimination Lawsuit')
library(data.table)
library(ggplot2)

# Create subset (female and male)
dt<-fread('Lawsuit.csv')
f.dt<-dt[Gender==0]
m.dt<-dt[Gender==1]

# Step 1: compare the salary of female and male with research emphasis
# Create subset (female and male with research emphasis)
res.group <- dt[Clin == 0]
f.res <-dt[Gender==0 & Clin==0]
m.res <-dt[Gender==1 & Clin==0]

# Construct linear regression model to predict salary based on publication rate and experience
model1 <- lm(Sal94 ~ Prate + Exper, data = res.group.dt)
summary(model1)

# Calculate total salary gap for female with research emphasis----- = 97303.66 (predicted salary > actual salary)
f.res$predicted.salary <- predict(model1, newdata = f.res)
f.res[, f.salary.gap := predicted.salary - Sal94]
total.female.salary.gap <- sum(f.res$f.salary.gap)
print(total.female.salary.gap)


# Calculate total salary gap for male with research emphasis----- = -97303.66 (predicted salary < actual salary)
m.res.dt$predicted.salary <- predict(model1, newdata = m.res.dt)
m.res.dt[, m.salary.gap := predicted.salary - Sal94]
total.male.salary.gap <- sum(m.res.dt$m.salary.gap)
print(total.male.salary.gap)
