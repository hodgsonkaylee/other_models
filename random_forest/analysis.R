##################################################
####### Project for Letter Recognition Data ######
##################################################

# Read in the data

letrec <- read.table(file="letter-recognition_data.txt",header=T,sep=",")
View(letrec)

# Explore Data
pdf("datexplorefin.pdf")
par(mfrow=c(2,1))
boxplot(letrec$width~letrec$letter,xlab="Letter",ylab="Width of Box",cex.axis=.6)
boxplot(letrec$high~letrec$letter,xlab="Letter",ylab="Height of Box",cex.axis=.6)
dev.off()

library(randomForest)

# Find the ideal number of variables to choose at each step of the Random Forest
Nrep <- 250
tuning <- matrix(NA,nrow=Nrep,ncol=15)
for(j in 1:Nrep){
  for(i in 1:15) {
    tuning[j,i] <- tuneRF(letrec[,2:17],letrec[,1],plot=F,trace=F,mtryStart = i,doBest=FALSE,stepFactor = 1)[2]
  }
}

write.table(tuning,file="tuning1.txt")
tuning1 <- read.table(file="data536.txt",header=T,sep="")
View(tuning1)
tuning1 <- as.matrix(tuning1)
tuningfinal <- rbind(tuning, tuning1)
write.table(tuningfinal,file="tuningfinal.txt")
View(tuningfinal)

Nrep <- 500
minval <- rep(NA,Nrep)
for(j in 1:Nrep) minval[j] <- which.min(tuningfinal[j,])
summary(as.factor(minval))
apply(tuningfinal,2,mean)

pdf("tuning.pdf")
par(mfrow=c(1,2))
plot(x=1:15,y=apply(tuningfinal,2,mean),main="Average OOB Error Estimates for each m",xlab="m",ylab="Average OOB Error")
abline(v=5,col='blue')
hist(minval,breaks=2:8,main="Frequency of Optimal Values",xlab="m")
dev.off()

train <- sample(1:nrow(letrec),.9*nrow(letrec),replace=F)

fit <- randomForest(letter~., data=letrec, subset=train, mtry=7, doBest=F, importance=T)
mean(fit$predicted==fit$y) # 0.963998, OOB error=4.29% 

tuneRF(letrec[,2:17],letrec[,1],trace=T, mtryStart = 2,doBest=FALSE,stepFactor = 1)[1:2]

