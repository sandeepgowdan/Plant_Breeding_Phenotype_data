#!/usr/bin/env Rscript

## ANOVA and Genetic parameters for Augmented design library(augmentedRCBD)
library(augmentedRCBD)
data <- read.table("data/data22.csv", header = TRUE, sep = ",")
trt<-as.factor(data$Genotypes)
blk<-as.factor(data$Block)
bout <- augmentedRCBD.bulk(data = data22, block = "Block",
                           treatment = "Genotypes", traits = c("PH", "FEW", 
                                                               "NOF", "FYPP"),
                           checks = NULL, alpha = 0.05, describe = TRUE,
                           freqdist = TRUE, gva = TRUE,
                           check.col = c("brown", "darkcyan",
                                         "forestgreen", "purple", "blue"),
                           console = TRUE)
report.augmentedRCBD.bulk(bout, file.path(("output"), "augmentedRCBDbulkoutput.docx"))
