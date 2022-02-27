# Load packages #
library(base)
library(psych)
library(stats)
library(afex)
library(car)
library(WRS2)
library(RVAideMemoire)

# Only include columns that are relevant for further analysis #
LongClean_GN3 <- subset(LongClean_GN3, select=c("Gender", "Age", "Observations", "Id", "Condition", "Surprise", "Exclusion", "Goal", "Story"))

# What number of participants fail due to each dropout criteria? #
# 1 = failure of at least one control question #
# 2 = participants write more than 30 words #
# 3 = participants write nothing #
sum(LongClean_GN3$Exclusion==1, na.rm=TRUE)
sum(LongClean_GN3$Exclusion==2, na.rm=TRUE)
sum(LongClean_GN3$Exclusion==3, na.rm=TRUE)

# Remove participants who fail due to any of these criteria #
LongClean_GN3 <- LongClean_GN3[ -which(LongClean_GN3$Exclusion==1), ]
LongClean_GN3 <- LongClean_GN3[ -which(LongClean_GN3$Exclusion==3), ]

# Demographic information, how many participants? #
LongClean_GN3$Id <- as.factor(LongClean_GN3$Id)
nlevels(LongClean_GN3$Id)

# Demographic information, gender #
male <- subset(LongClean_GN3, Gender=='Male', select = c(Id, Gender, Observations))
countm = length(unique(male$Id))
countm

female <- subset(LongClean_GN3, Gender=='Female', select = c(Id, Gender, Observations))
countfem = length(unique(female$Id))
countfem

other <- subset(LongClean_GN3, Gender=='Other', select = c(Id, Gender, Observations))
counto = length(unique(other$Id))
counto

pns <- subset(LongClean_GN3, Gender=='Prefer not to say', select = c(Id, Gender, Observations))
countpns = length(unique(pns$Id))
countpns

# Demographic information, age #
describe(Participant_data$Age)

# Demographic info, how many participants in each condition? #
describe.by(Participant_data, Participant_data$Condition)

complete <- subset(Participant_data, Condition=='complete')
interrupted <- subset(Participant_data, Condition=='interrupted')
abandoned <- subset(Participant_data, Condition=='abandoned')
quantile(complete$Surprise)
quantile(interrupted$Surprise)
quantile(abandoned$Surprise)

# Descriptive statistics #
LongClean_GN3$Story <- as.factor(LongClean_GN3$Story)
LongClean_GN3$Goal <- as.factor(LongClean_GN3$Goal)
summary(LongClean_GN3$Goal)
describe.by(LongClean_GN3, LongClean_GN3$Condition)

# First we need to investigate if 'surprise' differs by condition #
# We pre-registered an ANOVA for this, but we first need to see if the data satisfies parametric assumptions #
Participant_data$Condition <- as.factor(Participant_data$Condition)
describeBy(Participant_data, Participant_data$Condition)
# check for homogeneity of variance and normality #
leveneTest(Participant_data$Surprise, Participant_data$Condition, centre = median)
byf.shapiro(Participant_data$Surprise~Participant_data$Condition)
# Surprise is not homogeneous and normally distributed #
# Instead we will use a non-parametric equivalent: Kruskal-Wallis #
kruskal.test(Participant_data$Surprise ~ Participant_data$Condition)
# Results show that surprise does not differ by condition, so we will not include it in further analysis#

# Inferential statistics #
fullcondition <- mixed(Goal ~ Condition + (1|Id) + (1+Condition|Story), data = LongClean_GN3, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), method = 'LRT')
fullcondition
# We face problems of singular fit, so we reduce the complexity of the model by reducing one item at a time #
conditionslope <- mixed(Goal ~ Condition + (1|Id) + (Condition|Story), data = LongClean_GN3, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), method = 'LRT')
# We still face problems of singularity, so we reduce complexity again #
conditionintercept <- mixed(Goal ~ Condition + (1|Id) + (1|Story), data = LongClean_GN3, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), method = 'LRT')
# This latest model faces no trouble with singularity #
conditionintercept
# However, there is no difference between conditions #
