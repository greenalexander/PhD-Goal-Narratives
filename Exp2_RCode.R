# Load packages #
library(base)
library(psych)
library(stats)
library(rcompanion)

# Only include columns necessary for analysis #
Exp2_Data <- subset(Exp2_Data, select=c("Gender", "Age", "Condition", "Coding", "Comment"))

# What number of participants fail due to each dropout criteria? #
# 1 = failure of at least one control question #
# 2 = participants write more than 30 words #
# 3 = participants write nothing #
sum(Exp2_Data$Comment==1)
sum(Exp2_Data$Comment==2)
sum(Exp2_Data$Comment==3)

# Remove participants who fail due to any of these criteria #
Exp2_Data <- Exp2_Data[ which(Exp2_Data$Comment=='NA'), ]

# Demographic statistics #
Exp2_Data$Gender <-  as.factor(Exp2_Data$Gender)
summary(Exp2_Data$Gender)

Exp2_Data$Age <- as.numeric(Exp2_Data$Age)
summary(Exp2_Data$Age)
sd(Exp2_Data$Age)

Exp2_Data$Condition <- as.factor(Exp2_Data$Condition)
summary(Exp2_Data$Condition)

# How many participants mentioned a stand-in for cake? #
Exp2_Data$Coding <- as.factor(Exp2_Data$Coding)
summary(Exp2_Data$Coding)
Standins <- Exp2_Data[ c(Exp2_Data$Coding==2), ]
describeBy(Standins, Standins$Condition)

# collapse participants who mentioned a stand-in for cake with participants who used the word 'cake' in their summary #
Exp2_Data$Coding <- as.character(Exp2_Data$Coding)
Exp2_Data$Coding[Exp2_Data$Coding == "2"] <- "1"
Exp2_Data$Coding <- as.factor(Exp2_Data$Coding)
nlevels(Exp2_Data$Coding)
summary(Exp2_Data$Coding)

# Descriptive statistics by condition #
nlevels(Exp2_Data$Condition)
describeBy(Exp2_Data, Exp2_Data$Condition)

# Create a contingency table for the chi-squared test of independence #
R12 = c(149, 35)
R22 = c(126, 31)
R32 = c(143, 7)
rows   = 3
ct2 = matrix(c(R12, R22, R32),
                nrow=rows,
                byrow=TRUE)
rownames(ct2) = c("abandoned", "interrupted", "completed")         
colnames(ct2) = c("goal", "nogoal")  
ct2

# Inferential statistics #
chisq.test(ct2, correct = FALSE)
cramerV(ct2)

# Posthoc comparisons #
pairwiseNominalIndependence(ct2, fisher = FALSE, gtest = FALSE, chisq = TRUE, method = 'fdr', cramer = TRUE)
