library(dplyr)
library(lme4)
library(readxl)
library(multcomp)

#Experiment data
df <- read.csv(file = 'C:/Users/TShan/Desktop/dataframe_AllTrial.csv', header = TRUE)
df$SUBJECT <- as.factor(df$SUBJECT)
df$TALKER <- as.factor(df$TALKER)

m <- glmer(RESULT ~ SNR + CHOICE + SNR*CHOICE + (1 | SUBJECT), data = df, family = binomial(link = "logit"))

print(m)
summary(m)
summary(glht(m, mcp(CHOICE="Tukey")))

#Pilot data
pdf <- read.csv(file = 'C:/Users/TShan/Desktop/dataframe_AllTrial_pilot.csv', header = TRUE)
pdf$SUBJECT <- as.factor(pdf$SUBJECT)
pdf$TALKER <- as.factor(pdf$TALKER)

pm <- glmer(RESULT ~ SNR + CHOICE + SNR*CHOICE + (1 | SUBJECT), data = pdf, family = binomial(link = "logit"))

print(pm)
summary(pm)
summary(glht(pm, mcp(CHOICE="Tukey")))