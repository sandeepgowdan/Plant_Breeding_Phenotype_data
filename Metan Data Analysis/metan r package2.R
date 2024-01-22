library(metan)
dataset <- jjj %>% select_cols(4:16)

# Default plot setting
corr_plot(dataset)

# Chosing variables to be correlated
corr_plot(dataset,FFN, DFF, PH, NPB, IL, NIN, FL, FD, AFW, NOF, TW, NOS, FYPP)


# Axis labels, similar to the function pairs()
# Gray scale
corr_plot(dataset, FFN, DFF, PH, NPB, IL, NIN, FL, FD, AFW, NOF, TW, NOS, FYPP,
          shape.point = 19,
          size.point = 2,
          alpha.point = 0.5,
          alpha.diag = 0,
          pan.spacing = 0,
          col.sign = 'gray',
          alpha.sign = 0.3,
          axis.labels = TRUE)

corr_plot(dataset, FFN, DFF, PH, NPB, IL, NIN, FL, FD, AFW, NOF, TW, NOS, FYPP,
          prob = 0.01,
          shape.point = 21,
          col.point = 'black',
          fill.point = 'orange',
          size.point = 2,
          alpha.point = 0.6,
          maxsize = 4,
          minsize = 2,
          smooth = TRUE,
          size.line = 1,
          col.smooth = 'black',
          col.sign = 'cyan',
          col.up.panel = 'black',
          col.lw.panel = 'black',
          col.dia.panel = 'black',
          pan.spacing = 0,
          lab.position = 'tl')


library(metan)

d1 <- ge_cluster(jjj, ENV, GEN, nclust = 3)
plot(d1, nclust = 3)

mean_gen <-
  jjj %>%
  means_by(GEN) %>%
  column_to_rownames("GEN")

d <- clustering(mean_gen)
plot(d, xlab = "Euclidean Distance")

