####################################
##Install Packages
####################################

install.packages("hydroGOF")


####################################
##Call Libraries
####################################

library(readxl)
library(MASS)
library(caret)
library(hydroGOF)


####################################
##Import Train Set
####################################

hos <- read_excel("E:/STAT 628/Final Exam and Project/TrainData.xlsx") ### original dataset
names(hos) <- c('name','x1','x2','x3','x4','x5','x6','x7','x8','x9','x10','x11','x12','x13','x14','x15','x16','x17','x18','x19','x20','y')
hos <- hos[,-1]
yt <- log(hos$y)
hos <- cbind(hos,yt) #adding log(y) to data


####################################
##Import Test Set
####################################

hos12 <- read_excel("E:/STAT 628/Final Exam and Project/TestData.xlsx")
names(hos12) <- c('name','x1','x2','x3','x4','x5','x6','x7','x8','x9','x10','x11','x12','x13','x14','x15','x16','x17','x18','x19', 'x20' , 'y')
hos12 <- hos12[,-1]
yt2 <- log(hos12$y)
#hos12 <- hos12[ c(1:20)] ##drop Loss Reserves and the non-log column of y
hos12 <- cbind(hos12,yt2) #adding log(y) to data


####################################
##histogram of depdendant var
####################################

as.matrix(summary(hos))


####################################
##histogram of depdendant var
####################################

hist(hos$y, xlab="Accounts Receivable",main="Histogram of Y")
hist(hos$yt, xlab="log of Accounts Receivable",main="Histogram of Transformed Y")
hist(hos$x15, xlab="Gross Patient Revenue",main="Histogram of X15")
hist(log(hos$x15), xlab="log of Gross Patient Revenue",main="Histogram of Transformed X15")
hist(hos$x17)


###############################################
##correlation matrix and scatter matrix
###############################################

#correlation matrix
cor(hos$yt,log(hos$x15))
plot(hos$yt, log(hos$x15))
#scatter matrix
pairs(~x12+x13+x15+x16+x17+x18+x19+x20+y+yt, data=hos) #leaving out binary variables


###############################################
##Regression Model no amendments
###############################################

model <- lm(yt~.-y, data=hos) ## taking y out because yt is the log transformation of y, so it is redundent to keep
summary(model) #high r^2 because of variable 20


###############################################
##Regression Model - taking out x20
###############################################

model1 <- lm(yt~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19, data=hos)
summary(model1) #lower r^2 but gets rid of multicollinearity


###############################################
##Regression Model - taking log of revenue
###############################################

model2 <- lm(yt~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+log(x15)+x16+x17+x18+x19, data=hos)
summary(model2)


###############################################
##QQ Plot and Residual Plots
###############################################

plot(model2)


####################################
##Stepwise Regression
####################################

fit <- lm(yt~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+log(x15)+x16+x17+x18+x19, data=hos)
step <- stepAIC(fit, direction="both")
step$anova 
summary(step)

plot(step)


###############################################
##Outlier detection tests
###############################################

#DFFITS
dffits(model2, infl=lm.influence(model,),)

#DFBETAS
dfbetas(model2)

#Cooks Distance
cooks.distance(model2, infl=lm.influence(model,),)


####################################
##Cross-validation
####################################

#match the datasets used in the models above

hos13 <- hos12[-20]
hos13 <- hos13[-20]
hos13$x15 <- log(hos13$x15)

## classify the predictor model and calculate RMSE for the train model and the cross-corr

pred <- predict(step, hos13)
pred <- as.vector(pred)
rmse(yt2, pred) ## 13.71048
rmse(yt, model2$residuals) ## 17.60591
