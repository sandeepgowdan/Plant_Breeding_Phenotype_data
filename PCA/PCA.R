
setwd("C:/Users/windows/OneDrive/Desktop/example/output")
library(FactoMineR)
sample<- PCA(data.frame(princp[2:14], row.names = 1), graph = FALSE, ncp=10, scale.unit = TRUE)
library(ggplot2)
library("factoextra")
eig.val <- get_eigenvalue(sample)
scree.plot <- fviz_eig(sample, addlables = TRUE, ylim = c(0, 50))
var.plot <- fviz_pca_var(sample, col.var = "cos2",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE)
ind.plot <- fviz_pca_ind(sample, col.ind = "cos2",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE
)
bi.plot <- fviz_pca_biplot(sample, repel = TRUE,
                           col.var = "red",
                           col.ind = "black",
                           circle = TRUE)

library("magrittr")
library(ggpubr)
ggexport(plotlist = list(scree.plot,
                         ind.plot,
                         var.plot, bi.plot), filename = "PCA-new.pdf")

#retrive
sample$eig <- sample$eig
sample$var <- sample$var
sample$var$coord <- sample$var$coord
sample$var$cor <- sample$var$cor
sample$var$cos2 <- sample$var$cos2
sample$var$contrib <- sample$var$contrib
sample$ind <- sample$ind
sample$ind$coord <- sample$ind$coord
sample$ind$cos2 <- sample$ind$cos2
sample$ind$contrib <- sample$ind$contrib
sample$call <- sample$call
sample$call$centre <- sample$call$centre
sample$call$ecart.type <- sample$call$ecart.type
sample$call$row.w <- sample$call$row.w
sample$call$col.w <- sample$call$col.w


#sink
sink("PCA_sample.txt")
sample<- PCA(data.frame(princp[2:14], row.names = 1), graph = FALSE, ncp=10, scale.unit = TRUE)
sample$eig
sample$var
sample$var$coord
sample$var$cor
sample$var$cos2
sample$var$contrib
sample$ind
sample$ind$coord
sample$ind$cos2
sample$ind$contrib
sample$call
sample$call$centre
sample$call$ecart.type
sample$call$row.w
sample$call$col.w
sink()

attributes(sample)


