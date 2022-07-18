############################################
####### Case Study for Employee Data #######
############################################

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

Employee <- read.table(file="employee_data.txt",header=T)
fit <- lm(JobPerf~Age+Tenure+WellBeing+JobSat+IQ, data=Employee)
summary(fit)

#########################################
####### Exploratory Data analysis #######
#########################################

## Linearity Tests ##

# Create added variable plots to evaluate the linearity of the data
pdf("avplots.pdf")
avPlots(fit,main="",layout=c(2,3))
dev.off()

## Normality ##

fit.stres <- stdres(fit)
hist(fit.stres,xlab="Standardized Residuals",main="",freq=F) + curve(dnorm(x, 0, 1), add=TRUE, col="darkblue", lwd=2)

## Equal Variance and Independence ##
fit.res <- resid(fit)
fit.vals <- fitted.values(fit)
plot(fit.vals, fit.res, ylab = "Residuals", xlab="Fitted Values", main="")
abline(0,0)

# Plot Normality, Equal Variance, and Independence assumptions

pdf("assump.pdf")
par(mfrow=c(1,2))
normality <- hist(fit.stres,xlab="Standardized Residuals",main="Histogram of Standardized Residuals",freq = F) + curve(dnorm(x, 0, 1), add=TRUE, col="darkblue", lwd=2)
indev <- plot(fit.vals, fit.res, ylab = "Residuals", xlab="Fitted Values", main="Fitted vs. Residuals Plot") + abline(0,0)
dev.off()

### Collinearity ###
plot(Employee)
vif(fit)

### Outliers ###
outlierTest(fit)

#######################################
####### Impute the Missing Data #######
#######################################

# verify multivariate normal assumptions
pdf("normassump.pdf")
par(mfrow=c(2,3))
plot(density(Employee$WellBeing,na.rm=T),main="Wellbeing",xlab="",col="blue") + hist(Employee$WellBeing,freq=F,add=T) 
plot(density(Employee$JobSat,na.rm=T),main="Job Satisfaction",xlab="",col="blue") + hist(Employee$JobSat,freq=F,add=T) 
plot(density(Employee$JobPerf,na.rm=T),main="Job Performance",xlab="",col="blue") + hist(Employee$JobPerf,freq=F,add=T) 
plot(density(Employee$Age,na.rm=T),main="Age",xlab="",col="blue") + hist(Employee$Age,freq=F,add=T) 
plot(density(Employee$Tenure,na.rm=T),main="Tenure",xlab="",col="blue") + hist(Employee$Tenure,freq=F,add=T) 
plot(density(Employee$IQ,na.rm=T),main="IQ",xlab="",col="blue") + hist(Employee$IQ,freq=F,add=T) 
dev.off()

# Impute the data:
Employ <- Employee[,-1]
N <- dim(Employ)[1]
P <- dim(Employ)[2]
Nrep <- 1000

Mu <- list()
Sigma <- list()
Beta <- list()
rsquared <- list()

# Impute the data using multivariate normal. Repeat Nrep times.
for(i in 1:Nrep){
  Mu[[i]] <- apply(Employ, 2, mean,na.rm=T)
  Sigma[[i]] <- cov(Employ,use="pairwise.complete.obs")
  for(n in 1:N){
    for(p in 1:P){
      if(is.na(Employee[n,p+1])==TRUE){
        Employ[n,p] <- mvrnorm(1,mu=Mu[[i]],Sigma=Sigma[[i]])[p]
      }
    }
  }
  # Reestimate Parameters
  Beta[[i]] <- summary(lm(JobPerf~Age+Tenure+WellBeing+JobSat+IQ, data=Employ))$coefficients[,1:2]
  rsquared[[i]] <- summary(lm(JobPerf~Age+Tenure+WellBeing+JobSat+IQ, data=Employ))$r.squared
}

###################################################
####### Calculate Regression Summary Values #######
###################################################

# Move data into matrices
Beta.pooled <- matrix(NA,nrow=Nrep,ncol=P)
StdEr.pooled <- matrix(NA,nrow=Nrep,ncol=P)
for(j in 1:Nrep) {
    Beta.pooled[j,] <- Beta[[j]][,1]
    StdEr.pooled[j,] <- Beta[[j]][,2]
}

# Estimate the pooled coefficients and standard errors for imputed data
Beta.final <- apply(Beta.pooled,2,mean)

Var.m <- StdEr.pooled^2  
Var.w <- apply(Var.m,2,mean)
Varb <- (Beta.pooled-matrix(rep(Beta.final,Nrep),byrow=T,nrow=Nrep))^2
Var.b <- apply(Varb,2,sum)/(Nrep-1)

Var.t <- Var.w + Var.b + Var.b/Nrep
StdEr.final <- sqrt(Var.t)   

# Trace plots to check convergence
pdf("traceplots.pdf")
par(mfrow=c(2,3)) 
plot(Beta.pooled[,1],type="line",ylab="Intercept",xlab="")
plot(Beta.pooled[,4],type="line",ylab="Wellbeing",xlab="")
plot(Beta.pooled[,5],type="line",ylab="JobSat",xlab="")
plot(Beta.pooled[,2],type="line",ylab="Age",xlab="")  
plot(Beta.pooled[,3],type="line",ylab="Tenure",xlab="")
plot(Beta.pooled[,6],type="line",ylab="IQ",xlab="")
dev.off()

# Calculate the t statistic, p-value, and CI for the coefficients
alpha.star <- 0.05/P
FMI <- rep(NA,P)
df <- rep(NA,P)
t.val <- rep(NA,P)
p.val <- rep(NA,P)
CI <- matrix(NA,nrow=P,ncol=2)
for(i in 1:P){ 
  FMI[i] <- (Var.b[i] + Var.b[i]/Nrep)/Var.t[i]
  df[i] <- (Nrep-1)/(FMI[i]^2)
  t.val[i] <- Beta.final[i]/StdEr.final[i]
  p.val[i] <- 1-pt(t.val[i],df[i])
  CI[i,] <- Beta.final[i] + c(-1,1)*qt(1-alpha.star,df[i])*sqrt(Var.t[i])
}

reg.summary <- data.frame(matrix(c(Beta.final,CI[,1],CI[,2],StdEr.final,t.val,p.val),nrow=P,ncol=6))
colnames(reg.summary) <- c("coefficient","Lower Bound", "Upper bound","std error", "t stat", "p value")
rownames(reg.summary) <- c("Intercept","Age","Tenure","Wellbeing","JobSat","IQ")

# Calculate the R-squared value:
Rsquared <- rep(NA,1000)
for(i in 1:1000) Rsquared[i] <- rsquared[[i]]
Rsquared.mean <- sum(Rsquared)/1000
Rsquared.ci <- quantile(Rsquared,c(0.025,0.975))


