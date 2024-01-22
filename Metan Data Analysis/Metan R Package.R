library(metan)
BLUP <- waasb(jjj,
              resp = FYPP,
              gen = GEN,
              env = ENV,
              rep = REP)
plot_blup(BLUP)
plot_blup(BLUP, which = "ge")


library(metan)
vcov <- covcor_design(jjj, GEN, REP, everything())
means <- as.matrix(vcov$means)
pcov <- vcov$phen_cov
gcov <- vcov$geno_cov

index <- Smith_Hazel(means, pcov = pcov, gcov = gcov, weights = rep(1, 12))
plot(index)


library(metan)
BLUP <- waasb(jjj,
              resp = FYPP,
              gen = GEN,
              env = ENV,
              rep = REP)
plot_blup(BLUP)
plot_blup(BLUP)

plot_blup(BLUP, which = "ge")


View(jjj)

library(metan)
vcov <- covcor_design(jjj, GEN, REP, everything())
means <- as.matrix(vcov$means)
pcov <- vcov$phen_cov
gcov <- vcov$geno_cov

index <- Smith_Hazel(means, pcov = pcov, gcov = gcov, weights = rep(1, 12))
plot(index)

