library(FielDHub)
##randomization to eliminate error and enhance statstical validity and control for the extranious or unknoum factors
alpha<- alpha_lattice(
  t=55,
  r=3,
  k=5,
  l=2,
  plotNumber = 1,
  locationNames = c("bangalre", "hyderabad"),
  seed = 1235 
)
##field layout
plot(alpha)

##field book
options(max.print=999999)
alpha$fieldBook



#Using agricole
library(agricolae)
trt<- 1:55
t<-length(trt)
k<- 5
r<-3
outdesign<- design.alpha(trt,k,r,serie=1)
outdesign$sketch
outdesign$book

