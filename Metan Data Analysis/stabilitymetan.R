###### Genotype by environment interaction and Stability analysis ############
#############################Stability analysis with metan ##########################################
install.packages("metan")
library(metan)
install.packages("ggplot2")
library(ggplot2)
install.packages("writexl")
library(writexl)
install.packages("GGEBiplotGUI")
library(GGEBiplotGUI)
options(max.print = 10000)
#################data import####################################
stabdata<-read.table("clipboard", h=T, stringsAsFactors = TRUE)
attach(stabdata)
str(stabdata)
options(max.print = 10000)

library(readxl)
stabdata <- read_excel("C:/Users/MURALIB/Desktop/R training programe/Stability analysis/stabdata.xlsx")
View(stabdata)
str(stabdata)

############################# factors with unique levels ####################
stabdata$ENV <- factor(stabdata$ENV , levels=unique(stabdata$ENV ))
stabdata$GEN <- factor(stabdata$GEN, levels=unique(stabdata$GEN))
stabdata$REP <- factor(stabdata$REP, levels=unique(stabdata$REP))
str(stabdata)
########################### Data inspection and cleaning functions###############
inspect(stabdata, plot=TRUE)
find_outliers(stabdata, var=GY, plots=TRUE)
find_outliers(stabdata, var=HM, plots=TRUE)
remove_rows_na(stabdata)
replace_zero(stabdata)
find_text_in_num(stabdata$PHT)
find_text_in_num(stabdata$YLD)

######################### data analysis ##################################
###################### descriptive stats ################################
desc_stat(stabdata)
desc_stat(stabdata, stats="all")
ds <- desc_stat(stabdata, stats="all") 
ds
write_xlsx(ds, "ds.xlsx")

histplot<-desc_stat(stabdata, hist = TRUE) 
######################## mean performances ##############################
####################### mean of genotypes #############################
mg <- means_by(stabdata, GEN)
mg
View(mg)
######################### mean of environmnets #######################
me <- means_by(stabdata, ENV)
me
View(me)
########### mean performance of genotypes across environments ###########
mge <- stabdata %>% 
  group_by(ENV, GEN) %>%
  desc_stat(GY, HM, stats="mean")
mge
View(mge)
#####################two-way table##########################
twgy<-make_mat(stabdata, GEN,ENV,GY)
twgy
twhm<-make_mat(stabdata, GEN,ENV,HM)
twhm
make_long(twgy)
make_long(twhm)
#########Exporting mean performances#################
sheets <- list("Genmean" = mg, "Envmean" = me, "Genmeaninenv"= mge, "twowaygy"=twgy,"twowayhm"=twhm)
write_xlsx(sheets,"Mean peformances.xlsx")
############### plotting performance across environments ################
## GY
GY1 <- ge_plot(stabdata, ENV, GEN, GY)
GY1
GY2 <- ge_plot(stabdata, ENV, GEN, GY, type=2)
GY2
arrange_ggplot(GY1, GY2)
## HM
HM1 <- ge_plot(stabdata, ENV, GEN, HM)
HM1
HM2 <- ge_plot(stabdata, ENV, GEN, HM, type=2)
HM2
arrange_ggplot(HM1, HM2)
######################Genotype-environment winners####################
win <- ge_winners(stabdata, ENV, GEN, resp = everything())
win
ranks <- ge_winners(stabdata, ENV, GEN, resp = everything(), type = "ranks")
ranks
#####################ge or gge effects##################
gegy <- ge_effects(stabdata, ENV, GEN, GY)
ggegy <- ge_effects(stabdata, ENV, GEN, GY, type = "gge")
plot(gegy)
bplot(ggegy)

gehm <- ge_effects(stabdata, ENV, GEN, HM)
ggehm <- ge_effects(stabdata, ENV, GEN, HM, type = "gge")
plot(gehm)
plot(ggehm)

sheets <- list("winner" = win, "winranks" = ranks)
write_xlsx(sheets,"GenEnvwinners.xlsx")
########################## fixed effect models #############################
########################### Individual  and Joint anova ####################################
indaov<-anova_ind(stabdata,ENV,GEN,REP,GY)
indaov
indaovout<-indaov$GY$individual
indaovout

jointaov<-anova_joint(stabdata,ENV,GEN,REP,GY)
jointaov
jointaovout<-jointaov$GY$anova
jointaovout
sheets <- list("indanova" = indaovout, "jointanova" = jointaovout)
write_xlsx(sheets,"indandjointanova.xlsx")
## Bartlett test 
bartlett.test(stabdata$GY~stabdata$ENV, data = stabdata)
######################## stability analysis:ANOVA Based Models ###########################
ann <- Annicchiarico(stabdata, ENV, GEN, REP, GY)
print(ann)
eco <- ecovalence(stabdata, ENV, GEN, REP, GY)
print(eco)
Shu <- Shukla(stabdata, ENV, GEN, REP, GY)
print(Shu)

sink("anovabasedstability")
Annicchiarico(stabdata, ENV, GEN, REP, GY)
print(ann)
ecovalence(stabdata, ENV, GEN, REP, GY)
print(eco)
Shukla(stabdata, ENV, GEN, REP, GY)
print(Shu)
sink()
######################## Regression based stability analysis: Eberhart and Russell's regression mode ###########################
reg <- ge_reg(stabdata,ENV,GEN,REP,GY)
reg
regaov<-reg$GY$anova
regaov
regpara<-reg$GY$regression
regpara
plot(reg)

sheets <- list("regressionanova"=regaov, "regressions"=regpara)
write_xlsx(sheets,"regressionStabilitytp.xlsx")

#########################non-parametric stability models###############
super <- superiority(stabdata, ENV,GEN, GY )
print(super)
fox <- Fox (stabdata, ENV,GEN, GY )
print(fox)
###################### AMMI based stability analysis #####################################
ammiout<-performs_ammi (stabdata,ENV,GEN,REP,GY)
ammiout
ammianova<-ammiout$GY$ANOVA
ammianova
ammipca<-ammiout$GY$PCA
ammipca
ammimeans<-ammiout$GY$MeansGxE
ammimeans
ammimodel<-ammiout$GY$model
ammimodel
##################AMMI indexes#########################
ammiindex<-ammi_indexes(ammiout)
ammiindex
ammiindout<-ammiindex$GY
ammiindout
sheets <- list("ammianova" = ammianova, "ammipca" = ammipca, "ammimodel"=ammimodel, "ammimeans"= ammimeans, "ammiindex"=ammiindout)
write_xlsx(sheets,"ammistabilitytp.xlsx")
#AMMI biplots#
ammiplot1<-plot_scores(ammiout, x.lab = " Grain Yield")
ammiplot1
ammiplot2<-plot_scores(ammiout, type = 2, polygon = TRUE)
ammiplot2
ammiplot2<-plot_scores(ammiout,  type = 2,col.env = "blue",
col.gen = transparent_color(),col.segm.env = "orange",
highlight = c("G1", "G2"),col.highlight = "darkcyan",axis.expand = 1.5)
ammiplot2
ammiplot3 <- plot_scores(ammiout, type = 4)
ammiplot3
arrange_ggplot(ammiplot1, ammiplot2, ammiplot3, tag_levels = "a", nrow = 1)

################ ammi based on weighted average of absolute scores #################################
waasammi <- waas(stabdata,ENV,GEN,REP,GY)
waasammi
waasanova<-waasammi$GY$ANOVA
waasanova
waasmodel<-waasammi$GY$model
waasmodel
wabs<- ammi_indexes(waasammi)
wabsout<-wabs$GY
wabsout
sheets <- list("waasanova" = waasanova, "wabsout"=wabsout)
write_xlsx(sheets,"waasammi.xlsx")
################weighted average of absolute scores plots #################################
wassplot1 <- plot_scores(waasammi, type = 3)
wassplot1

wassplot2 <- plot_scores(waasammi, type = 2, polygon = TRUE)
wassplot2

###################### GGE Model based stability analysis ###############################
ggemodel<-gge(stabdata, ENV, GEN, GY)
predict(ggemodel)
plot(ggemodel)  ####A basic biplot
##########Biplot2:Mean performance vs stability#####
ggemodel<-gge(stabdata, ENV, GEN, GY, svp = "genotype")
plot(ggemodel, type = 2)
##########Biplot3: Which won where#############
ggemodel<-gge(stabdata, ENV, GEN, GY, svp = "symmetrical")
plot(ggemodel, type = 3)
##########Biplot4:Descriminativeness and representativeness#############
ggemodel<-gge(stabdata, ENV, GEN, GY, svp = "3")
plot(ggemodel, type = 4)
##########Biplot5: Examine an environment################
ggemodel<-gge(stabdata, ENV, GEN, GY, svp = "3")
plot(ggemodel, type = 5, sel_env = "E10")
##########Biplot6: Ranking environments###################
ggemodel<-gge(stabdata, ENV, GEN, GY, svp = "2")
plot(ggemodel, type = 6)
##########Biplot7: Examine a genotype###################
ggemodel<-gge(stabdata, ENV, GEN, GY, svp = "1")
plot(ggemodel, type = 7, sel_gen = "G8")
##########Biplot8: Ranking  genotype###################
ggemodel<-gge(stabdata, ENV, GEN, GY, svp = "1")
plot(ggemodel, type = 8)
##########Biplot8: Ranking  genotype###################
ggemodel<-gge(stabdata, ENV, GEN, GY, svp = "1")
plot(ggemodel, type = 8)
##########Biplot9: compare two genotypes genotype###################
ggemodel<-gge(stabdata, ENV, GEN, GY, svp="3")
plot(ggemodel, type = 9, sel_gen1 = "G8",sel_gen2 = "G10")
##########Biplot10: Relationship among environment###################
ggemodel<-gge(stabdata, ENV, GEN, GY, svp="2")
plot(ggemodel, type = 10)

stabdatagge <- make_mat(data_ge, GEN, ENV, GY) %>% round(2)
stabdatagge
GGEBiplot(Data = stabdatagge)

##################Multi-trait stability index##################
model<-waasb(stabdata, ENV, GEN, REP,resp = c(GY, HM), random = "all",mresp = c("h, l"),wresp = c(60, 40))
get_model_data(model, what = "WAASBY")
index <- mtsi(model, index = "waasby", mineval = 0.7, verbose = FALSE)
print(index)
plot(index)
###########################Wrapper function for stability analysis#############
gestats<-ge_stats(stabdata,ENV,GEN,REP,GY)
gestats
gestatsout<-gestats$GY
gestatsout
write_xlsx(gestatsout,"gestats.xlsx")
######If ranks only to be extracted##############
ranksp <- get_model_data(gestats, "ranks") 
ranksp
##########Spearman's rank correlation matrix between the computed stability indexes##
corplot<-corr_stab_ind(gestats, plot = FALSE, stats = "all" )
corplot
corplot2<-corr_stab_ind(gestats, plot = FALSE, stats = "ammi" )
corplot2
corplot3<-corr_stab_ind(gestats, plot = FALSE, stats = "par" )
corplot3
corplot4<-corr_stab_ind(gestats, plot = FALSE, stats = "nonpar" )
corplot4
#####################Correlation coefficients with p values###############
coef_all <- corr_coef(data_ge2)
print(coef_all)
corplota<-plot(coef_all)
corplota
granum<-corr_plot(data_ge2)
granum

###################Estimation of path coefficients###################
pathcoef<-path_coeff(data_ge2, resp = KW)
pathcoef<-path_coeff(data_ge2, resp = KW, brutstep = TRUE)
path1<-path_coeff(data_ge2, resp = KW, pred = c(PERK, EP, NKR, PH, NR,TKW,EL,CD,ED))
path1
