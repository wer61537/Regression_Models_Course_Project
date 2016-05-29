#Regression models project


#remove all objects just to be safe
rm(list = ls(all = TRUE))
#load libraries
library(MASS)
library(car)
library(ggplot2)
library(knitr)
library("xtable")
library(caret)
library(relaimpo)

#================================================
# Exploration of data
#================================================
#Loading data
data(mtcars)
#peak at the data
str(mtcars)
head(mtcars)

#recode transmission type
mtcars$am <- factor(mt2$am, labels = c('Automatic', 'Manual'))
sort(row.names(mtcars))
names(mtcars)
ggplot(mtcars, aes(x=hp,! y=mpg,! color=factor(am)))+! geom_point() 

p1<-qplot(y=mpg, x=disp, data = mtcars,color=factor(am)) +     geom_smooth() +
  labs(x="Displacement (cu.in.)", y="Miles per Gallon (MPG)")
p5<-qplot(y=mpg, x=hp, data = mtcars,color=factor(am)) +     geom_smooth() +
  labs(x="Gross Horsepower ", y="Miles per Gallon (MPG)")
p2<-qplot(y=mpg, x=drat, data = mtcars,color=factor(am)) +     geom_smooth() +
  labs(x="Rear Axle Ratio", y="Miles per Gallon (MPG)")
p3<-qplot(y=mpg, x=wt, data = mtcars,color=factor(am)) +     geom_smooth() + 
  labs(x="Weight (lb/1000)", y="Miles per Gallon (MPG)")
p4<-qplot(y=mpg, x=qsec, data = mtcars,color=factor(am)) +     geom_smooth() +
  labs(x="Time to cover 1/4 mile (sec)", y="Miles per Gallon (MPG)")

p1
p2
p3
p4
p5
#while not the best way, do for show
pcat1<-qplot(y=mpg, x=cyl, data = mtcars,color=factor(am)) +     geom_smooth() +
  labs(x="Number of Cylinders", y="Miles per Gallon (MPG)")
pcat2<-qplot(y=mpg, x=vs, data = mtcars,color=factor(am)) +     geom_smooth() +
  labs(x="Engine Type (0 = Vee, 1 = Straight)", y="Miles per Gallon (MPG)")
pcat3<-qplot(y=mpg, x=am, data = mtcars) +     geom_smooth() +
  labs(x="Transmission Type", y="Miles per Gallon (MPG)")
pcat4<-qplot(y=mpg, x=gear, data = mtcars,color=factor(am)) +  
  geom_smooth(method = "gam", se = TRUE)  +
  labs(x="Number of Forward Gears", y="Miles per Gallon (MPG)")
pcat5<-qplot(y=mpg, x=carb, data = mtcars,color=factor(am)) +     geom_smooth() + 
  labs(x="Number of Carburetors", y="Miles per Gallon (MPG)")
pcat1
pcat2

pcat3
pcat4
pcat5
#================================================
# Visual exploration of data
#================================================

#================================================
#Converting categorical variables to factor
#================================================
mtcars$am   = as.factor(mtcars$am)
mtcars$cyl  = as.factor(mtcars$cyl)
mtcars$vs   = as.factor(mtcars$vs)
mtcars$gear = as.factor(mtcars$gear)
#================================================
#Converting categorical variables to factor
#================================================


#================================================
#  Evaluate for high correlation between variables
#================================================
#Dropping dependent variable
mtcars.indep.vars= subset(mtcars, select = -c(mpg))

#Identifying numeric variables
mtcars.num.vars <- mtcars.indep.vars[sapply(mtcars.indep.vars, is.numeric)]

#Calculating Correlation
cor.mtrx <- cor(mtcars.num.vars)
cor.vars <- findCorrelation(cor.mtrx, cutoff=0.6)

#Identifying Variable Names of Highly Correlated Variables
cor.vars.names <- colnames(mtcars.num.vars)[cor.vars]

#Remove highly correlated variables and create a new dataset
mtcars2 <- mtcars[, -which(colnames(mtcars) %in% cor.vars.names)]
dim(mtcars2)

mtcars2
#================================================
#  Evaluate for high correlation between variables
#================================================

#================================
#  Initial model
#================================
#two initial models were created (only mpg ~ am and mpg ~.)
fit.am.r <- lm(mpg ~ am, data = mtcars2)
fit.all.r<- lm(mpg~.,data=mtcars2)

#detail about mpg~am
summary(fit.am.r)  #0.3385
print(xtable(summary(fit.am.r)  ), type = "html")
#detail about mpg~.
#verify no multicorrelatity
sqrt(vif(fit.all.r))>2  #all are false which is good

summary(fit.all.r)  #7026
print(xtable(summary(fit.all.r)  ), type = "html")
#================================
#  Initial model
#================================

#================================
#  Step wise evaluations of fit
#================================
#Stepwise Selection based on AIC
stepBoth <- stepAIC(fit.all.r, direction="both")
summary(stepBoth) #.7399


#Backward Selection based on AIC
stepBack <- stepAIC(fit.all.r, direction="backward")
summary(stepBack)  #.7399

#Forward Selection based on AIC
stepFor <- stepAIC(fit.all.r, direction="forward")
summary(stepFor) #.7026

#Stepwise Selection with BIC
n = dim(mtcars2)[1]
stepBIC = stepAIC(fit.all.r,k=log(n))
summary(stepBIC)  #0.7399
print(xtable(summary(stepBIC)  ), type = "html")
#anova(fit.all.r,fit.am.r,stepBoth, stepBIC,stepBack,stepFor)

model.int <- lm(mpg ~ cyl * am, data=mtcars2)
summary(model.int)
#no interaction term has a low p value. 

#there is no significant difference
anova(model.int ,stepBIC, stepBack)

#================================
#  Step wise evaluations of fit
#================================

#checks on the different models
#================================
#  Standardised coefficients of the diffferent models
#================================
lm.beta(fit.am.r)
lm.beta(fit.all.r)
lm.beta(stepBoth)
lm.beta(stepBack)
lm.beta(stepFor)
lm.beta(stepBIC)
#================================
#  Standardised coefficients
#================================


#================================
#  Testing for Multicollinearity
#  Check VIF of all the variables
# > 2 is an issue
#================================
sqrt(vif(fit.all.r))>2
sqrt(vif(stepBoth))>2
sqrt(vif(stepBack))>2
sqrt(vif(stepFor))>2
sqrt(vif(stepBIC))>2
#================================
#  Testing for Multicollinearity
#  Check VIF of all the variables
#================================


#================================
#  Autocorrelation Test
#================================
durbinWatsonTest(fit.all.r)  #p of 0.442
durbinWatsonTest(stepBoth)  #p of 0.144
durbinWatsonTest(stepBack) #p of 0.14
durbinWatsonTest(stepFor) #p of 0.414
durbinWatsonTest(stepBIC) #p of 0.174
#================================
#  Autocorrelation Test
#================================

#================================
#  Normality Of Residuals (Should be > 0.05)
#================================
shapiro.test(residuals(fit.all.r,type="pearson"))
shapiro.test(residuals(stepBoth,type="pearson"))
shapiro.test(residuals(stepBack,type="pearson"))
shapiro.test(residuals(stepFor,type="pearson"))
shapiro.test(residuals(stepBIC,type="pearson"))
#================================
#  Normality Of Residuals (Should be > 0.05)
#================================


#================================
#  Testing for heteroscedasticity (Should be > 0.05)
#================================
ncvTest(fit.all.r,)
ncvTest(stepBoth)
ncvTest(stepBack)
ncvTest(stepFor)
ncvTest(stepBIC)
#================================
#  Testing for heteroscedasticity (Should be > 0.05)
#================================

#================================
#  Outliers – Bonferonni test
#================================
outlierTest(fit.all.r)
outlierTest(stepBoth)
outlierTest(stepBack)
outlierTest(stepFor)
outlierTest(stepBIC)
#for all models, No Studentized residuals with Bonferonni p < 0.05
#Largest |rstudent|
#$fit.all.r Volvo 142E
#stepBoth Toyota Corolla 
#stepBack Toyota Corolla 
#stepFor Volvo 142E
#tepBIC Toyota Corolla 
#================================
#  Outliers – Bonferonni test
#================================


#================================
#  qq plots of studentized residuals
#================================
q0<-qqPlot(fit.am.r, main="Q-Q Plot for mpg ~ am") #qq plot for studentized resid 
q1<-qqPlot(fit.all.r, main="Q-Q Plot for mpg ~ cyl + drat + qsec + vs + am + gear") #qq plot for studentized resid 
q2<-qqPlot(stepBoth, main="Q-Q Plot for stepAic, direction ='Both'") #qq plot for studentized resid 
q3<-qqPlot(stepBack, main="Q-Q Plot for stepAic, direction ='Backward'") #qq plot for studentized resid 
q4<-qqPlot(stepFor, main="Q-Q Plot for stepAic, direction ='Forward'") #qq plot for studentized resid 
q5<-qqPlot(stepBIC, main="Q-Q Plot for stepAic, critera = BIC, direction ='Backward', mpg ~ cyl + am") #qq plot for studentized resid 
#================================
#  qq plots of studnetized residuals
#================================

#================================
#  leverage plots
#================================
leveragePlots(fit.am.r) # leverage plots
leveragePlots(fit.all.r) # leverage plots 
leveragePlots(stepBoth) # leverage plots 
leveragePlots(stepBack) # leverage plots 
leveragePlots(stepFor) # leverage plots 
leveragePlots(stepBIC) # leverage plots 
#================================
#  leverage plots
#================================


#================================
#  See Residuals
#================================
resid1 <- residuals(fit.all.r)
resid2 <- residuals(stepBoth)
resid3 <- residuals(stepBack)
resid4 <- residuals(stepFor)
resid5 <- residuals(stepBIC)
resid1
resid2
resid3
resid4
resid5
#================================
#  See Residuals
#================================

#================================
#  Relative Importance
#================================
calc.relimp(fit.all.r)
calc.relimp(stepBoth)
calc.relimp(stepBack)
calc.relimp(stepFor)
calc.relimp(stepBIC)
#================================
#  Relative Importance
#================================

# ======================================
# models without correlated variables
# ======================================
calc.relimp(fit.am.r)
calc.relimp(fit.all.r)
calc.relimp(stepBoth)
calc.relimp(stepBack)
calc.relimp(stepFor)
calc.relimp(stepBIC)
# ======================================
# models without correlated variables
# ======================================


# ======================================
# plot studentized residuals vs. fitted values 
# ======================================
spreadLevelPlot(fit.am.r)
spreadLevelPlot(fit.all.r)
spreadLevelPlot(stepBoth)
spreadLevelPlot(stepBack)
spreadLevelPlot(stepFor)
spreadLevelPlot(stepBIC)
# ======================================
# plot studentized residuals vs. fitted values 
# ======================================


# ======================================
# Influential Observations
# added variable plots 
# ======================================
avPlots(fit.am.r)
avPlots(fit.all.r)
avPlots(stepBoth)
avPlots(stepBack)
avPlots(stepFor)
avPlots(stepBIC)
# ======================================
# Influential Observations
# added variable plots 
# ======================================

# ======================================
# Cook's D plot
# identify D values > 4/(n-k-1) 
# ======================================
cutoff <- 4/((nrow(mtcars)-length(fit.am.r$coefficients)-2)) 
plot(fit.am.r, which=4, cook.levels=cutoff)

cutoff <- 4/((nrow(mtcars)-length(fit.all.r$coefficients)-2)) 
plot(fit.all.r, which=4, cook.levels=cutoff)

cutoff <- 4/((nrow(mtcars)-length(stepBIC$coefficients)-2)) 
plot(stepBIC, which=4, cook.levels=cutoff)
# ======================================
# Cook's D plot
# identify D values > 4/(n-k-1) 
# ======================================


# ======================================
# Distribution of studentized residuals
# ======================================
sresid <- studres(fit.am.r) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals for mpg ~ am")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit) 

sresid <- studres(fit.all.r) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals for mpg ~ cyl + drat + qsec + vs + am + gear")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit) 


sresid <- studres(stepBIC) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals for mpg ~ cyl + am")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit) 
# ======================================
# Distribution of studentized residuals
# ======================================



