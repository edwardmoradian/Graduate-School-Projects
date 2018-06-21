library(corrplot)
library(readr)
library(dplyr)
library(lmtest)
library(car)
library(MASS)

### Load the Data

mpg <- read_csv("C:/Users/Home/Desktop/Home/School/Graduate School/1 CSUEB/3 - Spring 2018/Stat 6509 Theory and Application and Regression/Project/Data.csv")
summary(mpg)

### Exploratory Analysis 

## Density plots of quantitative predictor variables, checking for normality

plot(density(mpg$EngDispl))
plot(density(mpg$NumCyl))
plot(density(mpg$NumGears))
plot(density(mpg$GHGRating))
plot(density(mpg$SmogRating))
plot(density(mpg$CO2Combined))

## Checking whether to drop any predictor variables becuase of being highly correlated with one another

# Correlating GHG, CO2, and Smog. GHG and CO2 Highly Correlated.
cor(mpg[,11:13])

# Correlating Engine Displacement, Number of Cylinders, Number of Gears
enginecorrelations <- data.frame(mpg$EngDispl,mpg$NumCyl,mpg$NumGears)
cor(enginecorrelations)

## Correlation between the response variable and the quantitative predictor variables
RespPredCorrelation <- data.frame(mpg$CombinedMPG,mpg$EngDispl,mpg$NumGears,mpg$SmogRating,mpg$CO2Combined)
colnames(RespPredCorrelation) <- c("Miles Per Gallon","Engine Displacement","Number of Gears","Smog Rating","CO2")
corrplot(cor(RespPredCorrelation), method ="number")

# Fit the data into a regression model

full = lm(CombinedMPG ~ factor(MfrCountry) + EngDispl + factor(Transmission) + NumGears	+ factor(DriveDesc) + factor(FuelUsageDesc) + factor(CarlineClassDesc) + factor(OilViscosity) + SmogRating + CO2Combined + SmogRating*CO2Combined, data=mpg)
null = lm(CombinedMPG ~ 1, data = mpg)
step(null,scope=list(lower=null,upper=full),direction="both")       
step(null,scope=list(lower=null,upper=full),direction="forward") 
step(null,scope=list(lower=null,upper=full),direction="backward") # Every direction yields same optimal model

BestFit = lm(formula = CombinedMPG ~ CO2Combined + NumGears + factor(FuelUsageDesc) + 
               EngDispl + factor(MfrCountry) + factor(OilViscosity) + factor(DriveDesc) + 
               factor(CarlineClassDesc) + SmogRating + CO2Combined:SmogRating, data = mpg)
summary(BestFit)

# Check for outliers and remove them

outlierTest(BestFit) # observation 330,392,416,535,536,601,602,1048 and 1056 are outliers according to Bonferroni p-values for outlier testing
influenceIndexPlot(BestFit) # influence plots indicate that obs 602 and 1056 are outliers with high influence
influencePlot(BestFit)
mpg2 <- mpg[-c(602,1056),]

BestFit2 = lm(formula = CombinedMPG ~ CO2Combined + NumGears + factor(FuelUsageDesc) + 
                EngDispl + factor(MfrCountry) + factor(OilViscosity) + factor(DriveDesc) + 
                factor(CarlineClassDesc) + SmogRating + CO2Combined:SmogRating, data = mpg2)
summary(BestFit2) # model has improved in R-squared

## Checking for valid constant variance and linearity assumptions

# Checking for constant variances using Bruesch-Pagan test

bptest(BestFit2) # constant variance assumption fails

# Checking for constant variances, linearity and normality using residual plots and QQ-plots.

BestFit2.res = resid(BestFit2)
par(mfrow=c(2,3))
plot(BestFit2,which=1:6)
residualPlots(BestFit2)
marginalModelPlots(BestFit2)

## Performing a lambda^-1 transformation by the boxcox

b = boxcox(BestFit2)
lambda=b$x
lik=b$y
bc=cbind(lambda,lik)
bc[order(-lik),]

BestFit3=lm(formula = CombinedMPG^-1 ~ CO2Combined + NumGears + factor(FuelUsageDesc) + 
              EngDispl + factor(MfrCountry) + factor(OilViscosity) + factor(DriveDesc) + 
              factor(CarlineClassDesc) + SmogRating + CO2Combined:SmogRating, data = mpg2)

## Rechecking constant variances, linearity and normality assumptions. Constant Variances does not hold.

BestFit3.res = resid(BestFit3)
par(mfrow=c(2,3))
plot(BestFit3,which=1:6)
residualPlots(BestFit3)
marginalModelPlots(BestFit3)

## Results from regression model

summary(BestFit3)
bptest(BestFit3)

# Polynomial second-order regression model for curvillinear nature

BestFit4=lm(formula = CombinedMPG ~ poly(CO2Combined,2) + NumGears + factor(FuelUsageDesc) + 
              EngDispl + factor(MfrCountry) + factor(OilViscosity) + factor(DriveDesc) + 
              factor(CarlineClassDesc) + SmogRating + CO2Combined:SmogRating,data = mpg2)

# Rerun Step Function with a polynomial for CO2Combined

full = lm(CombinedMPG ~ factor(MfrCountry) + EngDispl + factor(Transmission) + NumGears	+ factor(DriveDesc) + factor(FuelUsageDesc) + factor(CarlineClassDesc) + factor(OilViscosity) + SmogRating + poly(CO2Combined,2) + SmogRating*CO2Combined, data=mpg)
null = lm(CombinedMPG ~ 1, data = mpg)
step(null,scope=list(lower=null,upper=full),direction="both")       
step(null,scope=list(lower=null,upper=full),direction="forward") 
step(null,scope=list(lower=null,upper=full),direction="backward") # Every direction yields same optimal model

# New model with lesser predictor variables

BestFit5=lm(CombinedMPG ~ poly(CO2Combined,2) + factor(FuelUsageDesc) + 
              EngDispl + factor(CarlineClassDesc) + factor(MfrCountry) + 
              NumGears + factor(OilViscosity) + factor(DriveDesc),data=mpg2)

# Model checks

summary(BestFit5)
residualPlots(BestFit5)
hist(mpg2$CombinedMPG)
bptest(BestFit5)
marginalModelPlots(BestFit5)

# Using Test Data for testing.

mpg.test <- read_csv("C:/Users/Home/Desktop/Home/School/Graduate School/1 CSUEB/3 - Spring 2018/Stat 6509 Theory and Application and Regression/Project/Data2018.csv")
prediction <- predict(BestFit5, mpg.test)

summary(prediction)
summary(mpg.test$CombinedMPG)

cor(prediction, mpg.test$CombinedMPG)
plot(prediction, mpg.test$CombinedMPG, main="Predicted Values vs. Actual Values", xlab="Predicted Values", ylab="Actual Values",col="Blue")
abline(1,.9896986,col="Red")


