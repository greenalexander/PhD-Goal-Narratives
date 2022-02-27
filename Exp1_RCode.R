# Load packages #
library(base)
library(psych)
library(stats)
library(rcompanion)

# Only include columns necessary for data analysis #
Exp1_Data <- subset(Exp1_Data, select=c("Gender", "Age", "Condition", "Goal", "Dropout"))

# What number of participants fail due to each dropout criteria? #
# 1 = failure of at least one control question #
# 2 = participants write more than 30 words #
# 3 = participants write nothing #
sum(Exp1_Data$Dropout==1)
sum(Exp1_Data$Dropout==2)
sum(Exp1_Data$Dropout==3)

# Remove participants who fail due to any of these criteria #
Exp1_Data <- Exp1_Data[ which(Exp1_Data$Dropout=='na'), ]

# Demographic statistics #
Exp1_Data$Gender <-  as.factor(Exp1_Data$Gender)
summary(Exp1_Data$Gender)

summary(Exp1_Data$Age)
sd(Exp1_Data$Age)

# How many participants mentioned a stand-in for coffee? #
summary(Exp1_Data$Goal==2)

# Collapse participants who mentioned coffee stand-ins with those who explicitly mentioned coffee #
Exp1_Data$Goal <- as.character(Exp1_Data$Goal)
Exp1_Data$Goal[Exp1_Data$Goal == "2"] <- "1"
Exp1_Data$Goal <- as.factor(Exp1_Data$Goal)
nlevels(Exp1_Data$Goal)
summary(Exp1_Data$Goal)

# Descriptive statistics by condition #
Exp1_Data$Condition <- as.factor(Exp1_Data$Condition)
summary(Exp1_Data$Condition)
describeBy(Exp1_Data, Exp1_Data$Condition)

# Create a contingency table for chi-squared test of independence #
R1 = c(73, 51)
R2 = c(43, 49)
R3 = c(53, 59)
rows   = 3
Matriz = matrix(c(R1, R2, R3),
                nrow=rows,
                byrow=TRUE)
rownames(Matriz) = c("abandoned", "interrupted", "completed")         
colnames(Matriz) = c("goal", "nogoal")  
Matriz

# Chi-squared test to investigate whether goal status determines goal saliency #
chisq.test(Matriz, correct = FALSE)