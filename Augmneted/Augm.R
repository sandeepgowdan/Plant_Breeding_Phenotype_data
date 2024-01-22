#!/usr/bin/env Rscript

## ANOVA and Genetic parameters for Augmented design library(augmentedRCBD)
library(augmentedRCBD)
data1 <- read.table("data22.csv", header = TRUE, sep = ",")
trt<-as.factor(data$Genotypes)
blk<-as.factor(data$Block)
bout <- augmentedRCBD.bulk(data = data1, block = "Block",
                           treatment = "Genotypes", traits = c("PH", "FEW", 
                                                               "NOF", "FYPP"),
                           checks = NULL, alpha = 0.05, describe = TRUE,
                           freqdist = TRUE, gva = TRUE,
                           check.col = c("brown", "darkcyan",
                                         "forestgreen", "purple", "blue"),
                           console = TRUE)


zz
report.augmentedRCBD.bulk(bout, file.path("augmentedRCBDbulkoutput.docx"))

                          