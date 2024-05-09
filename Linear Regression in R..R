#Assessing distributions and calculating the correlation coefficient
#Assess the association between the walking distance and 
#lung function, age, gender, packhistory, COPD severity and presence of comorbidity.
#Set the working directory
setwd("C:/Users/user/Documents/Statistical thinking for public health/")
#Loading data set into R Studio
COPD <- read.csv("COPD_student_dataset.csv")
head(COPD)
#Good practice exercises*
#Inspecting for missing and outlier values**
#Installing the Hmsic package( allows us to inspect distribution for all of these variables) 
install.packages("Hmisc", lib= "C:/Users/user/Documents/Rfordatascience")
library(Hmisc)
#Description of individual variables
describe(COPD)
#This shows that only the response variabe has missing data. Furthermore, only 1 entry is missing.
#R's default is to exclude the missing data. 


#Checking if any of the continuous predictor variables have narrow distributions
#Histogram to view the distribution of the variable, walking distance
hist(COPD$MWT1Best, main="Histogram of Distance walked in 6 minutes (MWT1Best)", xlab="MWT1Best", breaks=12) 
#Histogram to view distribution of variable, FEV1
hist(COPD$FEV1, main="Histogram of lung function (FEV1)", xlab="FEV1") 
#Histrogram to view the distribution of the variable FEV1PRED
hist(COPD$FEV1PRED, main="Histogram of lung function (FEV1PRED)", xlab="FEV1PRED") 
#Histrogram to view the distribution of the variable FVC
hist(COPD$FVC, main="Histogram of lung function (FVC)", xlab="FVC")
#Histrogram to view the distribution of the variable FVCPRED
hist(COPD$FVCPRED, main="Histogram of lung function (FVCPRED)", xlab="FVCPRED")
#Histrogram to view the distribution of the variable age
hist(COPD$AGE, main="Histogram of AGE", xlab="AGE")
#Histrogram to view the distribution of the variable packhistory
hist(COPD$PackHistory, main="Histogram of PackHistory", xlab="PackHistory")
#Examining the categorical variable to completeness
#Creating a new variable indicating the presence of at least one comorbidity or complete absence
#First, all variables must be saved as factors
COPD$Diabetes <- as.factor(COPD$Diabetes)
class(COPD$Diabetes)
COPD$muscular <- as.factor(COPD$muscular)
class(COPD$muscular)
COPD$hypertension <- as.factor(COPD$hypertension)
class(COPD$hypertension)
COPD$AtrialFib <- as.factor(COPD$AtrialFib)
class(COPD$AtrialFib)
COPD$IHD <- as.factor(COPD$IHD)
class(COPD$IHD)
#Creating a new variable, comorbids
#First make sure is same length as all other variables
comorbids <- length(COPD$X)
#Making it so that if with any comorbids, 1, if without, 0. 
comorbids [COPD$Diabetes==1| COPD$muscular==1| COPD$hypertension==1| COPD$AtrialFib==1| COPD$IHD== 1] <- 1
comorbids [is.na(comorbids)] <-0
comorbids <- factor(comorbids)
#Display Comorbids
print(comorbids)
#Summary of comorbids
summary(comorbids)
#Adding new comorbids variable into the data sets
COPD$comorbids <- comorbids
table(COPD$comorbids)








#Examine the relationship between your candidate continuous predictor variables**
#Creating a correlation matrix for continuous variables. 
mylrmatrix<- COPD[, c("MWT1Best", "AGE" , "PackHistory", "FEV1", "FEV1PRED", "FVC", "FVCPRED")]
#Use pairwise complete observations, ignoring rows with missing values for the calculation
cor_matrix <- cor(mylrmatrix, use = "pairwise.complete.obs")
cor_matrix
#Correlation matrix to 2 decimal places
round(cor_matrix, 2)
#Correlation matrix plots
pairs(~AGE+PackHistory+ FEV1+ FEV1PRED+ FVC+ FVCPRED, data= COPD )
#Above shows that the lung functions are highly correlated with one another. 
#Therefore, FVCPRED is the best one to use given that the results of the pairwise corelation matrix are >-0.7 but < 0.7 

#Install the gmodels package (if not installed)
install.packages("gmodels")
# Load the gmodels package
library(gmodels)
#Crosstable for cartegorical variable
CrossTable (COPD$comorbids)


#Running a linear regression model for age and distance walked. 
MWT1Best_AGE <- lm(MWT1Best~AGE, data = COPD)
#Printing the model objects
lm(formula = MWT1Best~AGE, data = COPD )
#Summary of model
summary(MWT1Best_AGE)
#To view 95% confidence interval
confint(MWT1Best_AGE)
#Checking model assumptions
predictedvals <- predict(MWT1Best_AGE)
residualvals <- residuals(MWT1Best_AGE)
par(mfrow=c(2,2))
hist(residualvals, main = "Histogram of residuals", xlab = "Residuals")

#Running a linear regression model for pack history and distance walked. 
MWT1Best_PackHistory <- lm(MWT1Best~PackHistory, data = COPD)
#Printing the model objects
lm(formula = MWT1Best~PackHistory, data = COPD )
#Summary of model
summary(MWT1Best_PackHistory)
#To view 95% confidence interval
confint(MWT1Best_PackHistory)
#Checking model assumptions
predictedvals <- predict(MWT1Best_PackHistory)
residualvals <- residuals(MWT1Best_PackHistory)
par(mfrow=c(2,2))
hist(residualvals, main = "Histogram of residuals", xlab = "Residuals")


#Running a linear regression model for FVCPRED and distance walked. 
MWT1Best_FVCPRED <- lm(MWT1Best~FVCPRED, data = COPD)
#Printing the model objects
lm(formula = MWT1Best~FVCPRED, data = COPD )
#Summary of model
summary(MWT1Best_FVCPRED)
#To view 95% confidence interval
confint(MWT1Best_FVCPRED)
#Checking model assumptions
predictedvals <- predict(MWT1Best_FVCPRED)
residualvals <- residuals(MWT1Best_FVCPRED)
par(mfrow=c(2,2))
hist(residualvals, main = "Histogram of residuals", xlab = "Residuals")


#Running a linear regression model for comorbids and distance walked. 
MWT1Best_comorbids <- lm(MWT1Best~comorbids, data = COPD)
#Printing the model objects
lm(formula = MWT1Best~comorbids, data = COPD )
#Summary of model
summary(MWT1Best_comorbids)
#To view 95% confidence interval
confint(MWT1Best_comorbids)
#Checking model assumptions
predictedvals <- predict(MWT1Best_comorbids)
residualvals <- residuals(MWT1Best_comorbids)
par(mfrow=c(2,2))
hist(residualvals, main = "Histogram of residuals", xlab = "Residuals")


** 3. Fit a simple linear regression model between outcome and each predictor **
#LM between walking distance and gender
lr1AGE <- lm(MWT1Best~AGE, data= COPD)
summary(lr1AGE)
confint(lr1AGE)
#LM between walking distance and FEV1
lr2FEV1 <- lm(MWT1Best~FEV1, data= COPD)
summary(lr2FEV1)
confint(lr2FEV1)  
#LM between walking distance and FVC
lr3FVC <- lm(MWT1Best~FVC, data= COPD)
summary(lr3FVC)
confint(lr3FVC)    
#LM between walking distance and FEV1PRED
lr4FEV1PRED <- lm(MWT1Best~FEV1PRED, data= COPD)
summary(lr4FEV1PRED)
confint(lr4FEV1PRED)    
#LM between walking distance and FVCPRED
lr5FVCPRED <- lm(MWT1Best~FVCPRED, data= COPD)
summary(lr5FVCPRED)
confint(lr5FVCPRED) 
#LM between walking distance and presence of comorbidities
lr6comorbids <- lm(MWT1Best~comorbids, data = COPD)
summary(lr6comorbids)
confint(lr6comorbids) 
#LM between walking distance and packhistory
lr7PackHistory <- lm(MWT1Best~PackHistory, data = COPD)
summary(lr7PackHistory)
confint(lr7PackHistory) 
#Therefore pick FEV1 in the model over other lung function scored because the largest adjusted r in fev1 models explains fev to be the cause of most of the variance
#Fitting the linear regression model for walking distance
multiplelmMWT1Best <- lm(MWT1Best~FEV1+ AGE + PackHistory+ factor(comorbids), data= COPD)
summary(multiplelmMWT1Best)
confint(multiplelmMWT1Best)
#Lack of change in the coefficients between the simpla model and the multiple model should reassure you that collinearity is unlikely to be a problem in the model. 
#To examine the Variance inflation factors. 
#Install car package for vif calculation
install.packages("car")
library(car)
#Calculatimg the VIFs for the model
vif_values <- vif(multiplelmMWT1Best)
vif_values
#None of the VIFs is greater than 5 and therefore collinearity is not of concern.


********************************************************************************
#Summary of the descriptive statistics of Distance walked in 6 minuted(MWT1Best)
list("Summary" = summary(COPD$MWT1Best), "Mean" = mean(COPD$MWT1Best, na.rm=TRUE), "Standard Deviation" = sd(COPD$MWT1Best, na.rm=TRUE), "Range" = range(COPD$MWT1Best, na.rm=TRUE), "Inter-Quartile Range" = IQR(COPD$MWT1Best, na.rm=TRUE))
#Summary of descriptive statistics Of lung function (FEV1)
list("Summary" = summary(COPD$FEV1), "Mean" = mean(COPD$FEV1, na.rm=TRUE), "Standard Deviation" = sd(COPD$FEV1, na.rm=TRUE), "Range" = range(COPD$FEV1, na.rm=TRUE), "Inter-Quartile Range" = IQR(COPD$FEV1, na.rm=TRUE))
#Scatter plot of Distance waked in 6 minutes against lung function
plot(COPD$FEV1, COPD$MWT1Best, xlab = "FEV1", ylab = "MWT1Best")
#Running a pearson correlation tests for the distance walked in six minutes and lung function
cor.test(COPD$FEV1, COPD$MWT1Best, use= "complete. obs", method= "pearson")
#Running a spearman correlation tests for the distance walked in six minutes and lung function
cor.test(COPD$FEV1, COPD$MWT1Best, use= "complete. obs", method= "spearman", exact= FALSE)
#Running a linear reression model for lung function and distance walked. 
MWT1Best_FEV1 <- lm(MWT1Best~FEV1, data = COPD)
summary(MWT1Best_FEV1)
#To view 95% confidence interval
confint(MWT1Best_FEV1)
#Checking model assumptions
par(mfrow=c(2,2))
plot(MWT1Best_FEV1)
 


#Running a multiple linear regression to confirm whether both age and lung function are predictors of walking distance
MWT1Best_FEV1_AGE <- lm(MWT1Best~FEV1+AGE, data = COPD)
MWT1Best_FEV1_AGE
#To view summary of the model
summary(MWT1Best_FEV1_AGE)
#To view confidence interval
confint(MWT1Best_FEV1_AGE)








#Histogram to view the distribution of the variable, walking distance
hist(COPD$MWT1Best, main="Histogram of Distance walked in 6 minutes (MWT1Best)", xlab="MWT1Best", breaks=12) 
#Histogram to view distribution of variable, age
hist(COPD$AGE, main="Histogram of Age", xlab="AGE") 
#Summary of the descriptive statistics of Distance walked in 6 minuted(MWT1Best)
list("Summary" = summary(COPD$MWT1Best), "Mean" = mean(COPD$MWT1Best, na.rm=TRUE), "Standard Deviation" = sd(COPD$MWT1Best, na.rm=TRUE), "Range" = range(COPD$MWT1Best, na.rm=TRUE), "Inter-Quartile Range" = IQR(COPD$MWT1Best, na.rm=TRUE))
#Summary of descriptive statistics Of Age
list("Summary" = summary(COPD$AGE), "Mean" = mean(COPD$AGE, na.rm=TRUE), "Standard Deviation" = sd(COPD$AGE, na.rm=TRUE), "Range" = range(COPD$AGE, na.rm=TRUE), "Inter-Quartile Range" = IQR(COPD$AGE, na.rm=TRUE))
#Scatter plot of Distance waked in 6 minutes against Age
plot(COPD$AGE, COPD$MWT1Best, xlab = "AGE", ylab = "MWT1Best")
#Running a pearson correlation tests for the distance walked in six minutes and Age
cor.test(COPD$AGE, COPD$MWT1Best, use= "complete. obs", method= "pearson")
#Running a spearman correlation tests for the distance walked in six minutes and lung function
cor.test(COPD$AGE, COPD$MWT1Best, use= "complete. obs", method= "spearman", exact= FALSE)
#Running a linear regression model for age and distance walked. 
MWT1Best_AGE <- lm(MWT1Best~AGE, data = COPD)
#Printing the model objects
lm(formula = MWT1Best~AGE, data = COPD )
#Summary of model
summary(MWT1Best_AGE)
#To view 95% confidence interval
confint(MWT1Best_AGE)
#Checking model assumptions
predictedvals <- predict(MWT1Best_AGE)
residualvals <- residuals(MWT1Best_AGE)
par(mfrow=c(2,2))
hist(residualvals, main = "Histogram of residuals", xlab = "Residuals") 


#What is the relationship between walking distance and FVC?
#How many rows and columns are in the data set?
dim(COPD)
#What variables are in the data set?
head(COPD)
#Inspecting walking distance variable
class(COPD$MWT1Best)
#Inspecting FVC variable
class(COPD$FVC)
#Histogram to view the distribution of the variable, walking distance
hist(COPD$MWT1Best, main="Histogram of Distance walked in 6 minutes (MWT1Best)", xlab="MWT1Best", breaks=12) 
#Histogram to view distribution of variable, FVC
hist(COPD$FVC, main="Histogram of FVC", xlab="FVC") 
#Summary of the descriptive statistics of Distance walked in 6 minuted(MWT1Best)
list("Summary" = summary(COPD$MWT1Best), "Mean" = mean(COPD$MWT1Best, na.rm=TRUE), "Standard Deviation" = sd(COPD$MWT1Best, na.rm=TRUE), "Range" = range(COPD$MWT1Best, na.rm=TRUE), "Inter-Quartile Range" = IQR(COPD$MWT1Best, na.rm=TRUE))
#Summary of descriptive statistics Of FVC
list("Summary" = summary(COPD$FVC), "Mean" = mean(COPD$FVC, na.rm=TRUE), "Standard Deviation" = sd(COPD$FVC, na.rm=TRUE), "Range" = range(COPD$FVC, na.rm=TRUE), "Inter-Quartile Range" = IQR(COPD$FVC, na.rm=TRUE))
#Scatter plot of Distance waked in 6 minutes against FVC
plot(COPD$FVC, COPD$MWT1Best, xlab = "FVC", ylab = "MWT1Best")
#Running a pearson correlation tests for the distance walked in six minutes and FVC
cor.test(COPD$FVC, COPD$MWT1Best, use= "complete. obs", method= "pearson")
#Running a spearman correlation tests for the distance walked in six minutes and FVC
cor.test(COPD$FVC, COPD$MWT1Best, use= "complete. obs", method= "spearman", exact= FALSE)
#Running a linear regression model for FVC and distance walked. 
MWT1Best_FVC <- lm(MWT1Best~FVC, data = COPD)
#Summary of model
summary(MWT1Best_FVC)
#To view 95% confidence interval
confint(MWT1Best_FVC)
#Checking model assumptions
predictedvals <- predict(MWT1Best_FVC)
residualvals <- residuals(MWT1Best_FVC)
par(mfrow=c(2,2))
plot(MWT1Best_FVC)
hist(residualvals, main = "Histogram of residuals", xlab = "Residuals") 


#Need to examine if there is collinearity between age and FVC
#What is the relationship between Age and FVC?
#Scatter plot of Age against FVC
plot(COPD$FVC, COPD$AGE, xlab = "FVC", ylab = "AGE")
#Running a pearson correlation tests for AGE and FVC
cor.test(COPD$FVC, COPD$AGE, use= "complete. obs", method= "pearson")
#Running a spearman correlation tests for Age and FVC
cor.test(COPD$FVC, COPD$AGE, use= "complete. obs", method= "spearman", exact= FALSE)


#Multiple linear egression model to examine the relationship between walking distance, FVC and age.
MWT1Best_FVC_AGE <- lm(MWT1Best~FVC+AGE, data = COPD)
#To view summary of the model
summary(MWT1Best_FVC_AGE)
#To view confidence interval
confint(MWT1Best_FVC_AGE)



#Good practice  
#Viewing the data set
dim(COPD)
#Viewing just the headers of the data set
head(COPD)
#Viewing the kind of variable in age
class(COPD$Age)
#Summary of age variable
summary(COPD$AGE) 
#Histogram of age
hist(COPD$AGE)
#What kind of variable is in COPD sEVERITY
class(COPD$COPDSEVERITY) 
#Covert the above named character variable (text) to factor variable (categorical). 
COPD$COPDSEVERITY <- as.factor(COPD$COPDSEVERITY)
class(COPD$COPDSEVERITY)
#Table of COPD severity including missing values
table(COPD$COPDSEVERITY, exclude = NULL)
#What kind of variable is gender?
class(COPD$gender) 
#Change integer(numerical with no decimal) variable to factor (categorical) variable
COPD$gender <- as.factor( COPD$gender)
class(COPD$gender)
#Table of gender, inluding missing values
table(COPD$gender, exclude = NULL)
#What type of variable is walking distance
class(COPD$MWT1Best)
#What type of variable is copd
class(COPD$copd)
#Changing integer(numerical without decimal) variable to factor(categorical) variable
COPD$copd <- as.factor(COPD$copd)
class(COPD$copd)


#Creating a new variable indicating the presence of at least one comorbidity or complete absence
#First, all variables must be saved as factors
COPD$Diabetes <- as.factor(COPD$Diabetes)
class(COPD$Diabetes)
COPD$muscular <- as.factor(COPD$muscular)
class(COPD$muscular)
COPD$hypertension <- as.factor(COPD$hypertension)
class(COPD$hypertension)
COPD$AtrialFib <- as.factor(COPD$AtrialFib)
class(COPD$AtrialFib)
COPD$IHD <- as.factor(COPD$IHD)
class(COPD$IHD)
#Creating a new variable, cormobids
#First make sure is same length as all other variables
comorbids <- length(COPD$X)
#Making it so that if with any comorbids, 1, if without, 0. 
comorbids [COPD$Diabetes==1| COPD$muscular==1| COPD$hypertension==1| COPD$AtrialFib==1| COPD$IHD== 1] <- 1
comorbids [is.na(comorbids)] <-0
comorbids <- factor(comorbids)
#Display Comorbids
print(comorbids)
#Summary of comorbids
summary(comorbids)
#Adding new comorbids variable into the data sets
COPD$comorbids <- comorbids



*Good practice exercises*
**1.Inspecting for missing and outlier values**
#Installing the Hmsic package( allows us to inspect distribution for all of these variables) 
install.packages("Hmisc", lib= "C:/Users/user/Documents/Rfordatascience")
library(Hmisc)
#Description of individual variables
describe(COPD)
#Histogram of CAT
hist(COPD$CAT)
#Removing outlier value 188
COPD$CAT [COPD$CAT >40] <- NA
#Examining the categorical variable to completeness
#Installing gmodels packages
install.packages ("gmodels")
library(gmodels)
#Crosstable for copd
CrossTable(COPD$copd)


**2.Examine the relationship between your candidate predictor variables**
#Creating a correlation matrix
mydata<- COPD[, c( "AGE" , "PackHistory", "FEV1", "FEV1PRED", "FVC", "CAT", "HAD", "SGRQ")]
cor_matrix <- cor(mydata)
cor_matrix
#Correlation matrix to 2 decimal places
round(cor_matrix, 2)
#Correlation matrix plots
pairs(~AGE+PackHistory+ FEV1+ FEV1PRED+ FVC+ CAT+ HAD+ SGRQ, data= COPD )
#Above shows that the lung functions are highly correlated iwth one another. 

#Crosstable for cartegorical variable
CrossTable (COPD$hypertension, COPD$IHD)

** 3. Fit a simple linear regression model between outcome and each predictor **
#LM between walking distance and gender
lr1 <- lm(MWT1Best~gender, data= COPD)
summary(lr1)
confint(lr1)
#LM between walking distance and FEV1
lr2 <- lm(MWT1Best~FEV1, data= COPD)
summary(lr2)
confint(lr2)  
#LM between walking distance and FVC
lr3 <- lm(MWT1Best~FVC, data= COPD)
summary(lr3)
confint(lr3)    
#LM between walking distance and FEV1PRED
lr4 <- lm(MWT1Best~FEV1PRED, data= COPD)
summary(lr4)
confint(lr4)    
#LM between walking distance and FVCPRED
lr5 <- lm(MWT1Best~FVCPRED, data= COPD)
summary(lr5)
confint(lr5)    
#Therefore pick FEV1 in the model over other lung function scored because the largest adjusted r in fev1 models explains fev to be the cause of most of the variance
#Fitting the linear regression model for walking distance
multiplelmMWT1Best <- lm(MWT1Best~FEV1+ AGE + factor(gender)+ factor(COPDSEVERITY)+ factor(comorbids), data= COPD)
summary(multiplelmMWT1Best)
confint(multiplelmMWT1Best)
#Lack of change in the coefficients between the simpla model and the multiple model should reassure you that collinearity is unlikely to be a problem in the model. 
#To examine the Variance inflation factors. 
#Install car package for vif calculation
install.packages("car")
library(car)
#Calculatimg the VIFs for the model
vif_values <- vif(multiplelmMWT1Best)
vif_values
#None of the VIFs is greater than 5 and therefore collinearity is not of concern. 






#Running a multiple regression model
#MWT1best= α+ β1∗Diabetic+ β2∗AtrialFib+ β3∗Diabetic∗AtrialFib
COPD$Diabetes <- as.integer(COPD$Diabetes)
COPD$AtrialFib <- as.integer(COPD$AtrialFib)
#Creating new variable- having both afib and diabetes
DAF <- COPD$Diabetes * COPD$AtrialFib
#Creating the linear regression model
r1 <- lm(MWT1Best~factor(Diabetes)+factor(AtrialFib)+factor(DAF), data=COPD) 
summary(r1)
confint(r1)
#Therefore, 
#MWT1best= 428.14+ -7.69∗Diabetic+ -72.05∗AtrialFib+ -130.11∗Diabetic∗AtrialFib


#MWT1best= α+ β1∗Diabetic+ β2∗IHD+ β3∗Diabetes*IHD
COPD$Diabetes <- as.integer(COPD$Diabetes)
COPD$IHD <- ifelse(as.integer(COPD$IHD) == 1, 1, 0)
#Creating new variable- having both IHD and diabetes
IHDD <- COPD$IHD * COPD$AtrialFib
#Creating the linear regression model
r3 <- lm(MWT1Best~factor(Diabetes)+factor(IHD), data= COPD)
summary(r3)
confint(r3)
r2 <- lm(MWT1Best~factor(Diabetes)+factor(IHD)+factor(IHD*Diabetes), data=COPD) 
summary(r2)
confint(r2)
#Therefore, 
#MWT1best= 499.60 + -140.85 ∗Diabetes+ -86.02 5∗IHDb+  43.33 1∗DiabeticIHD
#Confidence intervals for coefficients of IHD and Diabetes and IHD both contain zero and are therefore not statistically significant
#Adjusted r2 for Afib, Diabetes is higher than that of IHD diabetes, making a better model to use
#Adding the interaction between IHD and Diabetes  