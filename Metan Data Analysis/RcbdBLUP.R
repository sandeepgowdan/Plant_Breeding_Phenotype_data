#!/usr/bin/env Rscript
##BLUP analysis for Multiseason/MultiLocation RCBD data
library(metan)
data <- read.table("data/data111.csv", header = TRUE, sep = ",")
WAASB_model <- waasb(data, ENV, GEN, REP,
                     resp = c(FFN, DFF, PH, IL, NIN, FL,FD, TW, NOS, FYPP))

plot(WAASB_model, type = "re", ncol = 4)

#variance components

get_model_data(WAASB_model, what = "vcomp")

#genetic parameters(genetic variability parameters)

get_model_data(WAASB_model, what = "genpar")

#predicted means for the genotypes(BLUP)

get_model_data(WAASB_model, what = "blupg")

# predicted means of each environment genotypic combination

WAASB_model %>%
  get_model_data(what = "blupge")

