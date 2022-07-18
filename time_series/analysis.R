##################################
####### Clean Data and EDA #######
##################################

solar = read.csv('SolarSavings.csv',header=T)

fossil = 1:29
green = 30:51

png('eda1.png',width = 1000,height = 600)
plot(fossil,solar$PowerBill[fossil],xlab = 'Date',ylab= 'Bill',type = 'l', col = 'red',xlim = c(0,51),ylim = c(0,max(solar$PowerBill)),xaxt='n')
lines(c(29,green),solar$PowerBill[c(29,green)],col = 'green3')
axis(1,at=1:51,labels=solar$Date)
legend('topright',c('No Solar Panels','Solar Panels'),lty=c(1,1),col=c('red','green3'),cex = 1.25) 
dev.off()

solar$Winter = c(0,rep(c(rep(1,3),rep(0,9)),4),rep(1,2)) #Jan-March
solar$Summer = c(0,rep(c(rep(0,6),rep(1,3),rep(0,3)),4),rep(0,2)) #July-Sept
solar$Time = 1:51

#################################
####### Build AR(1) Model #######
#################################

library(nlme)
mod = gls(PowerBill~Solar+Solar:Winter+Solar:Summer,correlation = corAR1(form=~Time),method="ML",data=solar)
summary(mod)
intervals(mod) # Confidence intervals for fixed effects and variance components

plot(1:51,mod$fitted,type='l')

####################################################
####### Verify Assumptions of Gaussian Model #######
####################################################

# Decorrelate Regression Model #
R <- diag(N)
R <- phi^(abs(row(R)-col(R)))
L <- t(chol(R))
Linv <- solve(L)
Ydec <- Linv%*%c(mod$fitted,pred.mn)
bhat <- t(t(coef(mod)))
X <- model.matrix(~Solar+Solar:Winter+Solar:Summer,data=solar)
y <- solar$PowerBill
epsilonhat <- Linv%*%(y-X%*%bhat)
stres <- rep(NA,length(epsilonhat))
for(i in 1:length(epsilonhat)) stres[i] <- (epsilonhat[i]-mean(epsilonhat))/sqrt(var(epsilonhat))

# Check the assumptions of the model with the cholesky residuals:
hist(stres) # normal distribution
yhat <- X%*%bhat
plot(yhat,epsilonhat)
abline(h=0) # constant variance centered around 0

pdf("modas.pdf")
par(mfrow=c(1,2))
hist(stres,freq=FALSE,xlab="Decorrelated Standardized Residuals",main="Histogram of Standardized Residuals")
plot(yhat,epsilonhat,xlab="Predicted Values",ylab="Decorrelated Residuals",main="Fitted vs. Residuals Plot")
abline(h=0)
dev.off()

#####################################################
####### Inference - Average Savings for Solar #######
#####################################################

Xstar <- rbind(c(1,1,0,1,0,0),c(1,1,0,0,0,0),c(1,1,0,0,0,0),c(1,1,0,0,0,0),c(1,1,0,0,0,1),c(1,1,0,0,0,1),c(1,1,0,0,0,1),
               c(1,1,0,0,0,0),c(1,1,0,0,0,0),c(1,1,0,0,0,0),c(1,1,0,1,0,0),c(1,1,0,1,0,0))
Xstar.wo <- rbind(c(1,0,1,0,0,0),c(1,0,0,0,0,0),c(1,0,0,0,0,0),c(1,0,0,0,0,0),c(1,0,0,0,1,0),c(1,0,0,0,1,0),c(1,0,0,0,1,0),
                  c(1,0,0,0,0,0),c(1,0,0,0,0,0),c(1,0,0,0,0,0),c(1,0,1,0,0,0),c(1,0,1,0,0,0))

Xstar.diff <- Xstar.wo-Xstar
bhat <- t(t(coef(mod)))

savings <- (Xstar.diff%*%bhat)[c(11:12,1:10),]
savings.pi <- rbind(pred.int.diff[11:12,],pred.int.diff[1:10,])

pdf("averagesavings.pdf")
plot(1:12,savings,ylab="Average Savings Each Month",xlab="Months")
dev.off()

##########################################################################
####### Predictions Using Gaussian Process Regressions - My Method #######
##########################################################################

# Predictions #

# Set up values for Predictions and Intervals
mod = gls(PowerBill~Solar+Solar:Winter+Solar:Summer,correlation = corAR1(form=~Time),method="ML",data=solar)
phi <- .1006577
sig2 <- sigma(mod)^2
bhat <- cbind(coef(mod))
X <- model.matrix(~Solar+Solar:Winter+Solar:Summer,data=solar)
Y <- solar$PowerBill

# WITH SOLAR

## Set up the R matrix for observations AND predictions
# March, April, May, June, July, August, September, October, November, December, January, February
Xstar <- rbind(c(1,1,0,1,0,0),c(1,1,0,0,0,0),c(1,1,0,0,0,0),c(1,1,0,0,0,0),c(1,1,0,0,0,1),c(1,1,0,0,0,1),c(1,1,0,0,0,1),
               c(1,1,0,0,0,0),c(1,1,0,0,0,0),c(1,1,0,0,0,0),c(1,1,0,1,0,0),c(1,1,0,1,0,0))

N <- nrow(solar) #Number of observed time periods
K <- 12 #number of time periods forward
R <- diag(K+N)
R <- phi^(abs(row(R)-col(R))) ## AR(1) correlation matrix

# Calculate the predictions, prediction variance, and the prediction intervals
pred.mn <- Xstar%*%bhat + R[N+(1:K), (1:N)]%*%solve(R[(1:N),(1:N)])%*%(Y-X%*%bhat) #conditional mean of MVN
pred.var <- sig2*(R[N+(1:K),N+(1:K)]-(R[N+(1:K),(1:N)]%*%solve(R[(1:N),(1:N)])%*%R[(1:N),N+(1:K)])) # conditional variance of MVN
pred.int <- matrix(NA,nrow=K,ncol=2)
for(i in 1:K) pred.int[i,] <- pred.mn[i] + c(-1,1)*qnorm(0.975)*sqrt(diag(pred.var)[i])

# WITHOUT SOLAR

Xstar.wo <- rbind(c(1,0,1,0,0,0),c(1,0,0,0,0,0),c(1,0,0,0,0,0),c(1,0,0,0,0,0),c(1,0,0,0,1,0),c(1,0,0,0,1,0),c(1,0,0,0,1,0),
                  c(1,0,0,0,0,0),c(1,0,0,0,0,0),c(1,0,0,0,0,0),c(1,0,1,0,0,0),c(1,0,1,0,0,0))

N <- nrow(solar) #Number of observed time periods
K <- 12 #number of time periods forward
R <- diag(K+N)
R <- phi^(abs(row(R)-col(R))) ## AR(1) correlation matrix

# Calculate the predictions, prediction variance, and the prediction intervals
pred.mn.wo <- Xstar.wo%*%bhat + R[N+(1:K), (1:N)]%*%solve(R[(1:N),(1:N)])%*%(Y-X%*%bhat) #conditional mean of MVN
pred.var.wo <- sig2*(R[N+(1:K),N+(1:K)]-(R[N+(1:K),(1:N)]%*%solve(R[(1:N),(1:N)])%*%R[(1:N),N+(1:K)])) # conditional variance of MVN
pred.int.wo <- matrix(NA,nrow=K,ncol=2)
for(i in 1:K) pred.int.wo[i,] <- pred.mn.wo[i] + c(-1,1)*qnorm(0.975)*sqrt(diag(pred.var.wo)[i])

# How much savings on average for a customer switching to solar:
Xstar.diff <- Xstar.wo-Xstar
N <- nrow(solar) #Number of observed time periods
K <- 12 #number of time periods forward
R <- diag(K+N)
R <- phi^(abs(row(R)-col(R))) ## AR(1) correlation matrix

# Calculate the predictions, prediction variance, and the prediction intervals
pred.mn.diff <- Xstar.diff%*%bhat + R[N+(1:K), (1:N)]%*%solve(R[(1:N),(1:N)])%*%(Y-X%*%bhat) #conditional mean of MVN
pred.var.diff <- sig2*(R[N+(1:K),N+(1:K)]-(R[N+(1:K),(1:N)]%*%solve(R[(1:N),(1:N)])%*%R[(1:N),N+(1:K)])) # conditional variance of MVN
pred.int.diff <- matrix(NA,nrow=K,ncol=2)
for(i in 1:K) pred.int.diff[i,] <- pred.mn.diff[i] + c(-1,1)*qnorm(0.975)*sqrt(diag(pred.var.diff)[i])

savings <- c(pred.mn.diff[11:12],pred.mn.diff[1:10])
savings.pi <- rbind(pred.int.diff[11:12,],pred.int.diff[1:10,])

pdf("averagesavings.pdf")
plot(1:12,savings,ylab="Average Savings Each Month",xlab="Months")
dev.off()


###########################################################################
####### Predictions Using Gaussian Process Regressions - Simulation #######
###########################################################################

#how long to save 8000?
library(MASS)
simR = diag(120)
simR <- sig2*phi^(abs(row(simR)-col(simR))) ## AR(1) correlation matrix

simX = cbind(0,-1,rep(c(rep(1,3),rep(0,9)),10),rep(c(rep(-1,3),rep(0,9)),10),
             rep(c(rep(0,6),rep(1,3),rep(0,3)),10),rep(c(rep(0,6),rep(-1,3),rep(0,3)),10))

sim = mvrnorm(10000,simX%*%bhat,simR)

howlong = function(x){
  total = 0
  i = 1
  while(total < 8000){
    total = total + x[i]
    i = i + 1
  }
  return(i)
}

running = function(x){
  y = vector()
  y[1] = x[1]
  for(i in 2:length(x)){
    y[i] = y[i-1] + x[i]
  }
  return(y)
}

simrun = apply(sim,1,running)

png('money_saved.png')
plot(simrun[,1],type = 'l',xlab = 'Months after Switching',ylab = 'Total Money Saved')
for(i in 2:100){
  lines(simrun[,i],)
}
abline(h=8000,col='red')
dev.off()


simtime = apply(sim,1,howlong)
quantile(simtime,c(.025,.975))
summary(simtime)

plot(sim[1,],type = 'l')
for(i in 2:20){
  lines(sim[i,],)
}

################################
####### Prediction Power #######
################################

# R-squared using decorrelated values

rsquared <- 1-(sum(epsilonhat^2)/sum((y-mean(y))^2))

# CROSS VALIDATION to find mean bias, RPMSE, coverage, interval width

bias = vector()
rpmse = vector()
coverage = vector()
width = vector()
for(i in 1:1000){
  x = sample(dim(solar)[1],8)
  test = solar[x,]
  train = solar[-x,]
  testgls  = mod = gls(PowerBill~Solar+Solar:Winter+Solar:Summer,correlation = corAR1(form=~Time),method="ML",data=train)
  phi = coef(testgls$modelStruct$corStruct,unconstrained=FALSE)
  sig2 = testgls$sigma^2
  bhat = cbind(coef(testgls))
  X <- model.matrix(~Solar+Solar:Winter+Solar:Summer,data=train)
  Xstar <- model.matrix(~Solar+Solar:Winter+Solar:Summer,data=test)
  Y = train$PowerBill
  N <- nrow(solar) #Number of observed time periods
  R = diag(N)
  R <- phi^(abs(row(R)-col(R))) ## AR(1) correlation matrix
  
  pred.mn <- Xstar%*%bhat + R[x,(1:N)[-x]]%*%solve(R[(1:N)[-x],(1:N)[-x]])%*%(Y-X%*%bhat) #conditional mean of MVN
  pred.var <- sig2*(R[x,x]-(R[x,(1:N)[-x]]%*%solve(R[(1:N)[-x],(1:N)[-x]])%*%R[(1:N)[-x],x])) # conditional variance of MVN
  pred.int <- matrix(NA,nrow=length(x),ncol=2)
  for(j in 1:length(x)) pred.int[j,] <- pred.mn[j] + c(-1,1)*qnorm(0.975)*sqrt(diag(pred.var)[j])
  bias[i] = mean(pred.mn - test$PowerBill)
  rpmse[i] = sqrt(mean((pred.mn-test$PowerBill)^2))
  coverage[i] = mean(pred.int[,1] < test$PowerBill & 
                       pred.int[,2] > test$PowerBill)
  width[i] = mean(pred.int[,2] - pred.int[,1])
}
mean(bias)
mean(rpmse)
mean(coverage)
mean(width)

