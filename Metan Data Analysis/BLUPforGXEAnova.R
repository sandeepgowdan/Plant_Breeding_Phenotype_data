#!/usr/bin/env Rscript

#Genotype-environment analysis by mixed-effect models- RCBD
library(metan)
#===============================================================#
# genotypes as random effects                                             #
#===============================================================#
data <- read.table("data/data1.csv", header = TRUE, sep = ",")
View(data)
a=gamem_met(data,
            env = ENV,
            gen = GEN,
            rep = REP,
            resp = everything())
# Distribution of random effects
plot(a, type = "re")
# Genetic parameters
get_model_data(a,"genpar")
# Genetic parameters
get_model_data(a,"vcomp")
###################################
#===============================================================#
#  assuming ENV as random effects 
b=gamem_met(data,
            env=ENV,
            gen=GEN,
            rep=REP,
            resp=GY,
            random = "env")
# Genetic parameters
get_model_data(b, "vcomp")
# Distribution of random effects (first variable)
plot(b, type = "re")
# Genetic parameters
get_model_data(b, "genpar")#only possible for models fitted with random = 'gen' or random = 'all'
#####################################################################
#genotypes, environments, genotype-vs-environment interaction and 
#blocks nested within environments) are assumed to be random factors
#####################################################################
c=gamem_met(data,
            env=ENV,
            gen=GEN,
            rep=REP,
            resp=GY,
            random = "all")
# Genetic parameters
get_model_data(c,"vcomp")
# Distribution of random effects 
plot(c, type = "re")
# Genetic parameters
get_model_data(c, "genpar")
#===============================================================#

