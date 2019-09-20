
#________________________________________________________________________________________________#
#************************************************************************************************#
# --> The following setwd() command should be commented out until your code is ready to be 
#     submitted. At that time, comment out the setwd() command in the 'Load Data' section.

# Set working directory to run code on Prof. Hamilton's  
#setwd(paste0(loc.Teaching,'ECON270_2019Fall/Data'))

# List of sub-directories
#   /Data                 - folder with datasets for analysis (read-only)
#   /Submissions          - folder for submitted .R files (write-only)
#________________________________________________________________________________________________#
#************************************************************************************************#

#------------------------------------------------------------------------------------------------#
# Load Packages ---------------------------------------------------------------------------------#

#install.packages("tidyverse")
library(tidyverse)

#------------2222222------------------------------------------------------------------------------------#
# Load Data -------------------------------------------------------------------------------------#
setwd('C:/Users/akkiiru/Desktop/myRdirectory')
getwd()
regdata<-readRDS('Wooldridge_Wages.rds')


#examine data ------------------------------------------------------------------------------------------------#
names(regdata)
head(regdata)
tail(regdata)
summary(regdata)
View(regdata)
# Begin analysis --------------------------------------------------------------------------------#
#fit regression 
fit<- lm(wage~IQ, data= regdata)
summary(fit)

# plot
plot(regdata$IQ, regdata$wage)

#summary of the regressions
sum.fit<-summary(fit)
names(sum.fit)

#finding the coefficients
sum.fit$coefficients
coefficients(sum.fit)
a1<-coefficients(fit)
#intercept and slope
coefficients(fit)
coeff.data <- data.frame(sum.fit$coefficients)
str(coeff.data)
a1<-coefficients(fit)
#finding f sstatistic
sum.fit$fstatistic
coefficients(fit)

#finding r squared
a5<-sum.fit$r.squared

#finding adjusted r squared
sum.fit$adj.r.squared

#finding the standard error
a6<-sum.fit$standarderror

#finding residual and fitted values
fitted.residuals <- fit$residuals
yhat <- fit$fitted.values

#residual plot
plot(fitted.residuals, xlab='Observation', ylab='e', main='Residual Plot', col='red')
abline(0,0, col='blue')

#Residual plot and IQ
plot(regdata$wage,fitted.residuals)
abline(0,0, col='blue')

#predicting wages using IQ
a2 <- predict(fit, data.frame(IQ = 120))
answerA<-predict(fit, data.frame(IQ = 108))
answerB<-predict(fit, data.frame(IQ = 115))
AnswerC=answerB-answerA
#finding the corelation coefficient
a4<-cor( regdata$wage,regdata$IQ)

#Finding residual and IQ regression
newfit<- lm(fitted.residuals~IQ, data= regdata)
summary(newfit)

#summary of the regressions
sum.newfit<-summary(newfit)
names(sum.newfit)

#finding the coefficients
sum.newfit$coefficients
coefficients(sum.newfit)
a7<-coefficients(newfit)
#------------------------------------------------------------------------------------------------#
# Print Results ---------------------------------------------------------------------------------#

  # For quantitative responses (myvar) use the following command
  #  cat(paste0('a: ', '\n\n')); print(myvar); cat(paste0('', '\n\n'))
  #  
  # For qualitative responses use the following command 
  #  cat(paste0('a: answer to part a', '\n\n'))
  
#Report the intercept and slope coefficient as two variables in a single dataframe.
cat(paste0('a:  The intercept and slope coefficient', '\n\n')); print(a1); cat(paste0('', '\n\n'))

#Find the predicted weekly wages for someone with an IQ of 120.
cat(paste0('b:  The predicted weekly wages for someone with an IQ of 120.', '\n\n')); print(a2); cat(paste0('', '\n\n'))
#Find the expected difference in wages between two individuals who have IQs of 108 and 115, respectively
cat(paste0('c: The expected difference in wages between two individuals who have IQs of 108 and 115 ', '\n\n')); print(AnswerC); cat(paste0('', '\n\n'))
# Find the correlation coefficient between weekly wages and IQ.
cat(paste0('d:  The correlation coefficient between weekly wages and IQ', '\n\n')); print(a4); cat(paste0('', '\n\n'))
#Find the R2 for this regression model.
cat(paste0('e:  The R2 for this regression model.', '\n\n')); print(a5); cat(paste0('', '\n\n'))
#Find the standard error of the slope coefficient.
cat(paste0('f: The standard error of the slope coefficient. ', '\n\n')); print(a6); cat(paste0('', '\n\n'))
#g. Consider a new regression in which the residuals from above are the dependent variable and IQ is the
#independent variable. Report the intercept and slope coefficient as two variables in a single dataframe
cat(paste0('g: The intercept and slope coefficient as two variables in a single dataframe. ', '\n\n')); print(a7); cat(paste0('', '\n\n'))
# For qualitative responses use the following command 
cat(paste0('a: I love Econometrics', '\n\n'))  

 