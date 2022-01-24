#Author: Suryateja Chalapati

#Importing required libraries
rm(list=ls())
library(rio)
library(moments)
library(dplyr)
library(data.table)
library(car)
library(stringr)
#install.packages("stringr")
#install.packages("data.table")

#Setting the working directory and importing the dataset
setwd("C:/Users/surya/Downloads")

df = import("Automobile Data.xlsx", sheet = "Sheet1")
colnames(df)=tolower(make.names(colnames(df)))

#Filtering dataset based on multiple conditions
df$region[df$region == "dallas / fort worth"] <- "dallas / fort worth, TX"
df$region[df$region == "odessa / midland"] <- "odessa / midland, TX"
df$region[df$region == "champaign urbana"] <- "champaign urbana, IL"
df$region[df$region == "chicago"] <- "chicago, IL"
df$region[df$region == "danville"] <- "danville, IL"
df$region[df$region == "southern illinois"] <- "southern illinois, IL"
df$region[df$region == "quad cities, IA/IL"] <- "quad cities, IL"

df = dplyr::filter(df, grepl('TX|IL|NC', region))

df1 = df[(df$cylinders == 4) | (df$cylinders == 6) | (df$cylinders == 8) ,]

df1 = df[(df$fuel == "gas") | (df$fuel == "diesel") ,]

df1 = setDT(df1)[, paste0('region',1:2):= tstrsplit(region, ',')]

df1$states = str_sub(df1$region,-2,-1)

df1 <- subset(df1, select = -region2)

#df1 = df1 %>% rename("region.1" = "region1", "state1" = "region2")

set.seed(36991670)
df.sample = data.frame(df1[sample(1:nrow(df1), 450, replace = FALSE),])
df.sample = df1 %>% group_by(states) %>% sample_n(150)

table(df.sample$states)
attach(df.sample)

#Analysis_1
leveneTest(asking.price~states,data=df.sample)

#Analysis_2
anova.out = aov(asking.price~states,data=df.sample)
summary(anova.out)

tukey.out=TukeyHSD(anova.out)
tukey.out

par(mar=c(5.1,8,4.1,2.1))
plot(TukeyHSD(anova.out), las=2, cex.axis=.8, sub = "For all three regions")
par(mar=c(5.1,4.1,4.1,2.1)) 

#Analysis_3
leveneTest(odometer~states,data=df.sample)

anova.out1 = aov(odometer~states,data=df.sample)
summary(anova.out1)

tukey.out1=TukeyHSD(anova.out1)
tukey.out1

par(mar=c(5.1,8,4.1,2.1))
plot(TukeyHSD(anova.out1), las=2, cex.axis=.8, sub = "For all three regions")
par(mar=c(5.1,4.1,4.1,2.1)) 

#Analysis_4
dftx = df.sample[df.sample$states == "TX", ]

anova.tx = aov(asking.price~region, data=dftx)
summary(anova.tx)

tukey.tx=TukeyHSD(anova.tx)
tukey.tx

par(mar=c(5.1,14,4.1,2.1))
plot(TukeyHSD(anova.tx), las=2, cex.axis=.8, sub = "For Texas")
par(mar=c(5.1,4.1,4.1,2.1)) 

#Analysis_5
anova.two = aov(asking.price~fuel+condition, data=df.sample)
summary(anova.two)

tukey.two=TukeyHSD(anova.two)
tukey.two

par(mar=c(5.1,8,4.1,2.1))
plot(TukeyHSD(anova.two), las=2, cex.axis=.8, sub = "For all three regions")
par(mar=c(5.1,4.1,4.1,2.1)) 
