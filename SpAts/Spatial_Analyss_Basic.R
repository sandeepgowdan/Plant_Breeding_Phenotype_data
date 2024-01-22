library(SpATS)
data(wheatdata)
wheatdata$R <- as.factor(wheatdata$row)
wheatdata$C <- as.factor(wheatdata$col)

m0 <- SpATS(response = "yield", spatial = ~ SAP(col, row, nseg = c(10,20)), 
            genotype = "geno", fixed = ~ colcode + rowcode, random = ~ R + C, 
            data = wheatdata, control =  list(tolerance = 1e-03))

# Fitted values: prediction on the dataset used for fitting the model
pred1.m0 <- predict(m0, newdata = wheatdata)
pred1.m0

# Genotype prediction
pred2.m0 <- predict(m0, which = "geno")
pred2.m0[1:5,]
pred2.m0



library(SpATS)
data(wheatdata)
wheatdata$R <- as.factor(wheatdata$row)
wheatdata$C <- as.factor(wheatdata$col)

m0 <- SpATS(response = "yield", spatial = ~ SAP(col, row, nseg = c(10,20), degree = 3, pord = 2), 
            genotype = "geno", fixed = ~ colcode + rowcode, random = ~ R + C, data = wheatdata, 
            control =  list(tolerance = 1e-03))

# Default plotting
plot(m0)
# Annotated
plot(m0, annotated = TRUE, main = "Wheat data (Gilmour et al., 1997)")

