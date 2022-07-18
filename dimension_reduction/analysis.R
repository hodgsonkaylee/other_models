###################################################
####### Case Study for Gene Expression Data #######
###################################################

# load packages

library(knitr)
library(dplyr)
library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(lme4)
library(car)
library(MASS)
library(leaps)

# Read in the data and fit the model

Gene <- read.table(file="GeneExpression.txt",header=T)

#########################################
####### Exploratory Data analysis #######
#########################################

## Linear Model Tests ##

# Randomly draw gene variables and fit models with only 50 variables selected
var.draws <- list()
fit.draws <- list()
df.draws <- list()
var.num <- seq(from=2, to=5150,by=1)
for(i in 1:10) {
  var.draws[[i]] <- as.vector(sample(var.num,size=50))
  df.draws[[i]] <- data.frame(cbind(Gene[,1],Gene[,var.draws[[i]][1:50]]))
  fit.draws[[i]] <- lm(df.draws[[i]][,1] ~ ., data=df.draws[[i]])
}

## Linearity ##

# Create added variable plots to evaluate the linearity of the data
pdf("avplots.pdf")
avPlots(fit.draws[[1]],main="",ylab="Malignant")[1:10]
dev.off()

avPlots(fit.draws[[2]],main="")
avPlots(fit.draws[[3]],main="")
avPlots(fit.draws[[4]],main="")
avPlots(fit.draws[[5]],main="")
avPlots(fit.draws[[6]],main="")
avPlots(fit.draws[[7]],main="")
avPlots(fit.draws[[8]],main="")
avPlots(fit.draws[[9]],main="")
avPlots(fit.draws[[10]],main="")

## Normality ##

fit.stres1 <- stdres(fit.draws[[1]])
fit.stres2 <- stdres(fit.draws[[2]])
fit.stres3 <- stdres(fit.draws[[3]])
fit.stres4 <- stdres(fit.draws[[4]])
fit.stres5 <- stdres(fit.draws[[5]])
fit.stres6 <- stdres(fit.draws[[6]])
fit.stres7 <- stdres(fit.draws[[7]])
fit.stres8 <- stdres(fit.draws[[8]])
fit.stres9 <- stdres(fit.draws[[9]])
fit.stres10 <- stdres(fit.draws[[10]])

hist(fit.stres1,xlab="Standardized Residuals",main="",freq=F) + curve(dnorm(x, 0, 1), add=TRUE, col="darkblue", lwd=2)
hist(fit.stres2,xlab="Standardized Residuals",main="",freq=F) + curve(dnorm(x, 0, 1), add=TRUE, col="darkblue", lwd=2)
hist(fit.stres3,xlab="Standardized Residuals",main="",freq=F) + curve(dnorm(x, 0, 1), add=TRUE, col="darkblue", lwd=2)
hist(fit.stres4,xlab="Standardized Residuals",main="",freq=F) + curve(dnorm(x, 0, 1), add=TRUE, col="darkblue", lwd=2)
hist(fit.stres5,xlab="Standardized Residuals",main="",freq=F) + curve(dnorm(x, 0, 1), add=TRUE, col="darkblue", lwd=2)
hist(fit.stres6,xlab="Standardized Residuals",main="",freq=F) + curve(dnorm(x, 0, 1), add=TRUE, col="darkblue", lwd=2)
hist(fit.stres7,xlab="Standardized Residuals",main="",freq=F) + curve(dnorm(x, 0, 1), add=TRUE, col="darkblue", lwd=2)
hist(fit.stres8,xlab="Standardized Residuals",main="",freq=F) + curve(dnorm(x, 0, 1), add=TRUE, col="darkblue", lwd=2)
hist(fit.stres9,xlab="Standardized Residuals",main="",freq=F) + curve(dnorm(x, 0, 1), add=TRUE, col="darkblue", lwd=2)
hist(fit.stres10,xlab="Standardized Residuals",main="",freq=F) + curve(dnorm(x, 0, 1), add=TRUE, col="darkblue", lwd=2)

## Equal Variance and Independence ##

fit.res1 <- resid(fit.draws[[1]])
fit.vals1 <- fitted.values(fit.draws[[1]])
plot(fit.vals1, fit.res1, ylab = "Residuals", xlab="Fitted Values", main="")
abline(0,0)

pdf("assump.pdf")
par(mfrow=c(1,2))
hist(fit.stres1,xlab="Standardized Residuals",main="",freq=F) + curve(dnorm(x, 0, 1), add=TRUE, col="darkblue", lwd=2)
plot(fit.vals1, fit.res1, ylab = "Residuals", xlab="Fitted Values", main="")
abline(0,0)
dev.off()

### Collinearity ###
vif(fit.draws[[1]])
vif(fit.draws[[2]])
vif(fit.draws[[3]])
vif(fit.draws[[4]])
vif(fit.draws[[5]])
vif(fit.draws[[6]])
vif(fit.draws[[7]])
vif(fit.draws[[8]])
vif(fit.draws[[9]])
vif(fit.draws[[10]])

### Outliers ###
outlierTest(fit.draws[[1]])
outlierTest(fit.draws[[2]])
outlierTest(fit.draws[[3]])
outlierTest(fit.draws[[4]])
outlierTest(fit.draws[[5]])
outlierTest(fit.draws[[6]])
outlierTest(fit.draws[[7]])
outlierTest(fit.draws[[8]])
outlierTest(fit.draws[[9]])
outlierTest(fit.draws[[10]])
# observation 2 is an outlier in all of these measures


#################################
####### Shrinkage - LASSO #######
#################################

library(glmnet)

# split the observations into a training set and a test set 
# using a random vector (train) of elemnts equal to TRUE if the observation is in the training set and FALSE otherwise
# the test vector has a TRUE if the observation is in the test set and FALSE otherwise
x <- model.matrix(Malignant~., data=Gene)[,-1]
y <- Gene$Malignant
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]
grid <- 10^seq(10,-2,length=100)

# cross-validation to find the best value of lambda (shrinkage parameter) - one with the lowest MSE
lasso.mod <- glmnet(x[train,],y[train],alpha=1,lambda=grid)
set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train],alpha=1)

# plot cross-validation measures to find the best lambda
pdf("cverror.pdf")
par(mfrow=c(1,2))
plot(lasso.mod)
plot(cv.out)
dev.off()

# output of best lambda and RMSE
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod,s=bestlam,newx=x[test,])
sqrt(mean((lasso.pred-y.test)^2))
# RMSE = 0.2099957

# Use the lasso method with the best value for lambda to get the coefficients
out <- glmnet(x,y,alpha=1,lambda=grid)
lasso.coefa <- predict(out,type="coefficients",s=bestlam)
matrix(cbind(lasso.coefa[lasso.coefa!=0]))

fit$out$dev.ratio[which(cv.out$out$lambda == bestlam)]

#############################################
####### Bootstrap Confidence Interval #######
#############################################

B <- 10000
vals <- seq(from=1,to=102)
lasso.coef <- list()

for(i in 1:B){
  dat.vals <- sample(vals,replace=TRUE)
  new.dat <- Gene[dat.vals,]
  x <- as.matrix(new.dat[,-1])
  y <- new.dat$Malignant
  lasso.val <- glmnet(x,y,alpha=1,lambda=bestlam)
  lasso.coef[[i]] <- predict(lasso.val,type="coefficients",s=bestlam)
}

lassocoef <- matrix(NA,nrow=29,ncol=B)
for(i in 1:B) {
  lassocoef[,i] <- cbind(lasso.coef[[i]][lasso.coefa!=0])
}

lasscoef <- matrix(NA,nrow=29,ncol=B)
for(j in 1:290000) {
  ifelse(lassocoef[j]==0, lasscoef[j] <- NA, lasscoef[j] <- lassocoef[j])
}

# Calculate the bootstrap mean and quantiles for the coefficients
coef.mean <- apply(lassocoef,1,mean)
coef.quant <- apply(lassocoef,1,function(x) quantile(x,probs=c(.025,.975),na.rm=TRUE)) 

# Show only coefficients that are included in the model
matrix(cbind(coef.mean[lasso.coef[[i]]!=0]))
matrix(cbind(coef.quant[lasso.coef!=0,]))

# deviange explained #
pdf("devex.pdf")
plot(out,xvar="dev",label=TRUE)
dev.off()




# Decided not to use this method #
##########################################################
####### Dimension Reduction - Principle Components #######
##########################################################

x <- model.matrix(Malignant~., data=Gene)[,-1]
y <- Gene$Malignant

library(pls)

set.seed(2)
pcr.fit <- pcr(Malignant ~., data=Gene, scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP") # plot the cross-validation scores

ncomp.onesigma <- selectNcomp(pcr.fit, method = "onesigma", plot = TRUE,ylim = c(.18, .6))
ncomp.permut <- selectNcomp(pcr.fit, method = "randomization", plot = TRUE,ylim = c(.18, .6))



pcr.cv <- crossval(pcr.fit, segments = 102)
plot(MSEP(pcr.cv), legendpos="topright")
summary(pcr.cv, what = "validation")

pcr.fit <- pcr(y~x,scale=TRUE,ncomp=90)
summary(pcr.fit)



