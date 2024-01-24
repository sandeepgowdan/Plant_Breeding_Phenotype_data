#!/usr/bin/env Rscript
##GGE Biplot Analysis
library(metan)
data <- read.table("data/data1.csv", header = TRUE, sep = ",")

#Biplot
gge_model <- gge(data, ENV, GEN, GY)
predict(gge_model)
plot(gge_model)

b <- plot(gge_model, col.gen = "red", size.text.env = 4)
plot(b)

# Type 2 Biplot analysis Mean Performance Vs Stability
gge_model <- gge(data, ENV, GEN, GY, svp = "genotype")
plot(gge_model, type = 2)

# Type 3 Which won where
gge_model <- gge(data, ENV, GEN, GY, svp = "symmetrical")
plot(gge_model, type = 3)

# Type 4 Discriminatieness vs representativeness
gge_model <- gge(data, ENV, GEN, GY)
plot(gge_model, type = 4)

# Type 5 Examine an Environment
gge_model <- gge(data, ENV, GEN, GY, svp = "symmetrical")
plot(gge_model, type = 5, sel_env = "E3")

# Type 6 Ranking Environments
gge_model <- gge(data, ENV, GEN, GY)
plot(gge_model, type = 6)

# Type 7 Examine a genotype
gge_model <- gge(data, ENV, GEN, GY, svp = "genotype")
plot(gge_model, type = 7, sel_gen = "G5")

# Type 8 Examine a genotype
gge_model <- gge(data, ENV, GEN, GY, svp = "genotype")
plot(gge_model, type = 8)

# Type 9 Compare two genotypes
gge_model <- gge(data, ENV, GEN, GY, svp = "symmetrical")
plot(gge_model, type = 9, sel_gen1 = "G9", sel_gen2 = "G4")

