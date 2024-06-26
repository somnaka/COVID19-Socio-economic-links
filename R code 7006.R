# set working directory
setwd(dirname(file.choose()))
getwd()

# read in data from csv file
mydata <- read.csv("u2409950_rev.csv", stringsAsFactors = FALSE)
head(mydata)    # Inspect top rows of the data
str(mydata)

# make $Area and $Areacode as factors
mydata$Area <- factor(mydata$Area)
mydata$Areacode <- factor(mydata$Areacode)

str(mydata)

#-----------------------DATA EXPLORATION------------------------------
# check for missing data

apply(mydata, MARGIN = 2, FUN = function(x) sum(is.na(x)))
library(Amelia)
missmap(mydata, col = c("black", "grey"), legend = FALSE)

# boxplot for dependent variable of "pdeath_number"
# boxplot(x, main,xlab, ylab)
boxplot(mydata$pdeath_number, xlab="Covid_deaths", ylab="Count")
# label outliers
boxdata <- boxplot(mydata$pdeath_number, xlab="Covid_deaths", ylab="Count")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(mydata$pdeath_number==boxdata$out[i]),pos=4, cex=1)}

# inspect outliers
mydata[259,]

# Mean, Std. deviation, Maximum and Minimum of "pdeath_number"
summary(mydata$pdeath_number)
summary(mydata)

# Summary statistics printed separately
mean(mydata$pdeath_number)
sd (mydata$pdeath_number)
max(mydata$pdeath_number)
min(mydata$pdeath_number)

# Create a boxplot of all independent variables
#boxplot( pMale, pFemale, pWhite, pBlack, pAsian, pH1_4, p5_abv, pWFH, pPublicT, pPrivT, pWorkage, pRetage,
        #names=c("pMale", "pFemale", "pWhite", "pBlack", "pAsian", "pH1.4", "p5_abv", "pWFH", "pPublicT", "pPrivT", "pWorkage", "pRetage"),
        #xlab="SocioFactors", ylab="Percentage", col = "Bisque")

attach (mydata)
# frequency histogram
hist(mydata$pdeath_number)
hist(mydata$pdeath_number, col = "light blue", border = "dark blue", freq = T, ylim = c(0,150),
     xlab = "percentage of Covid deaths", main = "Histogram")

# Add a rug plot
rug (pdeath_number)

# probability density histogram
hist(pdeath_number, col = "light blue", border = "dark blue", freq = F, ylim = c(0,5.50),
     xlab = "percentage of Covid deaths", main = "Histogram")

# Add a rug plot
rug (pdeath_number)

# Add a density curve
lines (density(sort(pdeath_number)))

# Add a Normal curve

xfit <- seq(from = min(pdeath_number), to = max(pdeath_number), by = 0.2)
yfit = dnorm(xfit, mean(pdeath_number), sd(pdeath_number))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

# Plot histogram and normal approximation
library(rcompanion)
plotNormalHistogram(pdeath_number, main = "Histogram", xlab = "pdeath_ number")

#KS test------------------

ks.test(mydata$pdeath_number, "pnorm", mean(mydata$pdeath_number), sd(mydata$pdeath_number))

#------------------------Correlation test techniques----------------------
# select subset of data
mydata2 <- data.frame(pdeath_number, pMale, pFemale, pWhite, pBlack, pAsian, pH1_4, p5_abv, pWFH, pPublicT, pPrivT, pWorkage, pRetage)
# add column names
colnames(mydata2) <- c("pdeath_number","pMale", "pFemale", "pWhite", "pBlack", "pAsian", "pH1.4", "p5_abv", "pWFH", "pPublicT", "pPrivT", "pWorkage", "pRetage")

# basic correlation matrix
cor(mydata2, method = "spearman")
mydata_cor <- cor(mydata2, method = "spearman")

round(mydata_cor, digits = 2)
#----------------Correlation plots-----------------------

#-----using corrplot--
library(corrplot)
mydata2 <- data.frame(pdeath_number, pMale, pFemale, pWhite, pBlack, pAsian, pH1_4, p5_abv, pWFH, pPublicT, pPrivT, pWorkage, pRetage)

cor_matrix <- cor(mydata2)

# Plot the correlation matrix using corrplot
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

#--------------------using corrgram--------------------

cor.matrix <- cor(mydata2, use = "pairwise.complete.obs", method = "spearman")
round(cor.matrix, digits = 2)
cor.df <- as.data.frame(cor.matrix)
View(cor.df)

library(psych)

pairs.panels(mydata2, method = "spearman", hist.col = "grey", col = "blue", main = "Spearman")

library(corrgram)
# corrgram works best with Pearson correlation
corrgram(mydata2, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="London variables")

# test correlation of dependent variable with all independent variables
cor.test(mydata$pdeath_number, mydata$pMale, method = "spearman")
cor.test(mydata$pdeath_number, mydata$pFemale, method = "spearman")
cor.test(mydata$pdeath_number, mydata$pWhite, method = "spearman")
cor.test(mydata$pdeath_number, mydata$pBlack, method = "spearman")
cor.test(mydata$pdeath_number, mydata$pAsian, method = "spearman")
cor.test(mydata$pdeath_number, mydata$pH1_4, method = "spearman")
cor.test(mydata$pdeath_number, mydata$p5_abv, method = "spearman")
cor.test(mydata$pdeath_number, mydata$pWFH, method = "spearman")
cor.test(mydata$pdeath_number, mydata$pPublicT, method = "spearman")
cor.test(mydata$pdeath_number, mydata$pPrivT, method = "spearman")
cor.test(mydata$pdeath_number, mydata$pWorkage, method = "spearman")
cor.test(mydata$pdeath_number, mydata$pHRetage, method = "spearman")

# basic multivariate scatterplot matrix of some variables
pairs(~ pdeath_number + pMale + pFemale,  data = mydata,
      main = "multivariate scatterplot matrix Gender")

pairs(~ pdeath_number + pWhite + pBlack + pAsian,  data = mydata,
      main = "multivariate scatterplot matrix Race")

#----------------------DATA ANALYSIS------------------------------------

# select variables by excluding those not required; the %in% operator means 'matching'
myvars <- names(mydata) %in% c("Area", "Areacode", "Population", "pFemale", "p5_abv", "pPublicT",
                               "pdeath_number")

# the ! operator means NOT
mydata3 <- mydata[!myvars]
str(mydata3)
rm(myvars)


# Kaiser-Meyer-Olkin statistics: if overall MSA > 0.6, proceed to factor analysis
library(psych)
KMO(cor(mydata3))


# Determine Number of Factors to Extract
library(nFactors)

# get eigenvalues: eigen() uses a correlation matrix
ev <- eigen(cor(mydata3))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col="blue", xlab="variables")

# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="red", xlab="number of components", ylab ="cumulative proportion")

#-----Section 08----

# Varimax Rotated Principal Components
# retaining 'nFactors' components
library(GPArotation)

# principal() uses a data frame or matrix of correlations
fit <- principal(mydata3, nfactors=5, rotate="varimax")
fit

# -----Multiple Linear Regression-----------

# model with all variables except the ones with correlation close to zero 
model1 <- lm(pdeath_number~pdeath_number + pMale + pWhite + pBlack + pAsian 
                              + pH1_4 + pWFH + pPrivT + pWorkage + pRetage)
summary(model1)
# calculate variance inflation factor
library(car)
vif(model1)
sqrt(vif(model1)) > 2  # if > 2 vif too high

#model with variables from factor analysis
model2 <- lm(pdeath_number ~ pMale + pPrivT + pH1_4 + pWorkage + pWFH)
summary(model2)
sqrt(vif(model2)) > 2
hist(model2$residuals)

# test whether model2 and model1 are significantly different using F test
anova(model2, model1, test = "F")

# use a stepwise approach to search for a best model
library(RcmdrMisc)
# use a stepwise approach to search for a best model
model3 <- stepwise(model2, direction = "forward")
summary (model3)
hist(model3$residuals)
rug(model3$residuals)
plot(model3$residuals ~ model3$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model3$residuals, "pnorm", mean(model3$residuals), sd(model3$residuals))
sqrt(vif(model3)) > 2
calc.relimp(model3, type = c("lmg"), rela = TRUE)

# test whether model3 and model2 are significantly different using F test
anova(model3, model2, test = "F")

detach (mydata)
