#CODE BY MUHAMMAD FIRDAUS BIN MUNTOHA

library(dplyr)
library(ggplot2)
library(data.table)

# Load file into R
lawsuit.dt<-fread(“Lawsuit.csv”)

# Divide datasets into Male and Female
lawsuit.f<-filter(lawsuit.dt, Gender == 0)
lawsuit.m<-filter(lawsuit.dt, Gender == 1)

# Factor categorical values
lawsuit.f$Dept<-factor(lawsuit.dt$Dept, labels = dept_names)
lawsuit.m$Dept<-factor(lawsuit.m$Dept,labels = dept_names)

# Rename Dept names
dept_names<-c("1"="Biochem/Molc. Bio","2"="Physiology","3"="Genetics","4"="Pediatrics","5"="Medicine","6"="Surgery")

#Plots
female_rank_breakdown_dept<-ggplot(lawsuit.f,aes(Dept,fill=Rank))+geom_bar(position = "dodge") + labs(title="No. of Female Doctors in each Department segregated into respective Ranks", x="Department",y="No. of Doctors") + scale_fill_manual(name="Rank",values=c("lightblue","steelblue","navy"),labels=c("Assistant","Associate","Full Professor"))
print(female_rank_breakdown_dept)

male_rank_breakdown_dept<-ggplot(lawsuit.m,aes(Dept,fill=Rank))+geom_bar(position = "dodge") + labs(title="No. of Male Doctors in each Department segregated into respective Ranks", x="Department",y="No. of Doctors") + scale_fill_manual(name="Rank",values=c("lightblue","steelblue","navy"),labels=c("Assistant","Associate","Full Professor"))
print(male_rank_breakdown_dept)
