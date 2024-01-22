#!/usr/bin/env Rscript
## ANOVA for RCBD Single location
#Can run without any packages
data <- read.table("data/data.csv", header = TRUE, sep = ",")
model <-lm(data$yield~ data$Replication+data$Variety)
#Obtain ANOVA
anova <-anova(model)
anova

