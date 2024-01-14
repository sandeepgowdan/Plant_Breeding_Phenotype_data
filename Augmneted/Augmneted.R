
library(augmentedRCBD)
setwd("C:/Users/windows/OneDrive/Desktop/results")
data <- read.csv(input_file, header = TRUE, sep = ",")
bout <- augmentedRCBD.bulk(data = data, block = "Block",
                           treatment = "Genotypes", traits = c("FYPP"),
                           checks = NULL, alpha = 0.05, describe = TRUE,
                           freqdist = TRUE, gva = TRUE,
                           console = TRUE)

Desc_Stat <- bout$`Descriptive statistics`
write.csv(Desc_Stat, "Descriptive_Statstics.csv")

ANOVA_Treatment_Adjusted <- bout$`ANOVA, Treatment Adjusted`
write.csv(ANOVA_Treatment_Adjusted, "ANOVA_Treatment_Adjusted.csv")

ANOVA_Block_Adjusted <- bout$`ANOVA, Block Adjusted`
write.csv(ANOVA_Block_Adjusted, "ANOVA_Block_Adjusted.csv")

Adjusted_Means <- bout$Means
write.csv(Means, "Adjusted_Means.csv")

Std_Errors <- bout$`Std. Errors`
write.csv(Std_Errors, "Std_Errors.csv")

critical_Difference <- bout$CD
write.csv(critical_Difference, "critical_Difference.csv")

Adjusted_mean <- bout$`Overall adjusted mean`
write.csv(Adjusted_mean, "Overall_Adjusted_mean.csv")

CV <- bout$CV
write.csv(CV, "Co-efficient_of_Variation.csv")

Genetic_Variablity_Analysis <- bout$`Genetic variability analysis`
write.csv(Genetic_Variablity_Analysis, "Genetic_Variablity_Analysis.csv")

Check_Statstics <- bout$`Check statistics`
write.csv(Check_Statstics, "Check_Statstics.csv")
