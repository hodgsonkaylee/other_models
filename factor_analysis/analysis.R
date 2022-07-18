syndat <- read.csv("factor_data.csv")
head(syndat)

###################################################################
### Path Analysis

library(lavaan)
library(semPlot)
library(GGally)

model <-'
Patri2016 ~ PRCom16
IFL2016 ~ AOM2015
AOM2015 ~ SonPref2015
BPD2017C ~ IFL2016 + Polygy2016
Polygy2016 ~ IFL2016
PSOW2014 ~ RapeXmp2016
HnrKlg2016 ~ BPD2017C
PRCom16 ~ PSOW2014
CsnMrg2016 ~ PRCom16
SonPref2015 ~ CsnMrg2016 + Patri2016
RapeXmp2016 ~ HnrKlg2016
'
fit <- cfa(model, data=syndat)
summary(fit,fit.measures = TRUE, standardized=T,rsquare=T)

semPaths(fit, 'std', layout = 'circle')

ggcorr(syndat[,2:12], nbreaks = 10, label = T, low = "red3", high = "green3", 
       label_round = 2, name = "Correlation Scale", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlation Plot") +
  theme(plot.title = element_text(hjust = 0.6))

###################################################################
### Confirmatory Factor Analysis
cfamodel <- '
syndrome =~ Patri2016 + IFL2016 + AOM2015 + BPD2017C + Polygy2016 + 
PSOW2014 + HnrKlg2016 + PRCom16 + CsnMrg2016 + SonPref2015 + RapeXmp2016'
fitcfa <- cfa(cfamodel,data=syndat,estimator="ULS")
summary(fitcfa,fit.measures=TRUE, standardized=T,rsquare=T)
semPaths(fitcfa, 'std', layout = 'circle')

###################################################################
### Simulation Study

# y1: 3 levels (0-2)
# y2: 5 levels (0-4)
# y3: 5 levels (0-4)
# y4: 4 levels (0-3)
# y5: 5 levels (0-4)
# y6: 4 levels (1-4)
# y7: 3 levels (0-2)
# y8: 5 levels (0-4)
# y9: 4 levels (0-3)
# y10: 5 levels (0-4)
# y11: 2 levels (0-1)

# different n sizes for binomial draws
k <- c(3,5,5,4,5,4,3,5,4,5,2)
syndat <- syndat[,2:12]

# observations that are missing or have messy data
set.seed(543675)
ranvals01 <- rbinom(176,1,0.01)
ranvals025 <- rbinom(176,1,0.025)
ranvals05 <- rbinom(176,1,0.05)
ranvals10 <- rbinom(176,1,0.1)
ranvals25 <- rbinom(176,1,0.25)
ranvals50 <- rbinom(176,1,0.5)
ranvals90 <- rbinom(176,1,0.9)

# assign new dataframes
SynVar.M01 <- SynVar.V01 <- syndat
SynVar.M025 <- SynVar.V025 <- syndat
SynVar.M05 <- SynVar.V05 <- syndat
SynVar.M10 <- SynVar.V10 <- syndat
SynVar.M25 <- SynVar.V25 <- syndat
SynVar.M50 <- SynVar.V50 <- syndat
SynVar.M90 <- SynVar.V90 <- syndat

# randomly choose observations that will be missing/random
for(i in 1:176){
  # 1% missing/messy
  if(ranvals01[i]==1){ 
    SynVar.M01[i,] <- rep(NA,11)
    for(j in 1:11) SynVar.V01[i,j] <- rbinom(1,k[j],0.5)
  }
  # 2.5% missing/messy
  if(ranvals025[i]==1){ 
    SynVar.M025[i,] <- rep(NA,11)
    for(j in 1:11) SynVar.V025[i,j] <- rbinom(1,k[j],0.5)
  }
  # 5% missing/messy
  if(ranvals05[i]==1){ 
    SynVar.M05[i,] <- rep(NA,11)
    for(j in 1:11) SynVar.V05[i,j] <- rbinom(1,k[j],0.5)
  }
  # 10% missing/messy
  if(ranvals10[i]==1){ 
    SynVar.M10[i,] <- rep(NA,11)
    for(j in 1:11) SynVar.V10[i,j] <- rbinom(1,k[j],0.5)
  }
  # 25% missing/messy
  if(ranvals25[i]==1){ 
    SynVar.M25[i,] <- rep(NA,11)
    for(j in 1:11) SynVar.V25[i,j] <- rbinom(1,k[j],0.5)
  }
  # 50% missing/messy
  if(ranvals50[i]==1){ 
    SynVar.M50[i,] <- rep(NA,11)
    for(j in 1:11) SynVar.V50[i,j] <- rbinom(1,k[j],0.5)
  }
  if(ranvals90[i]==1){
    SynVar.M90[i,] <- rep(NA,11)
    for(j in 1:11) SynVar.V90[i,j] <- rbinom(1,k[j],0.5)
}
}

# write to files for SAS analysis
write.csv(SynVar.M01,"synvarm01.csv")
write.csv(SynVar.V01,"synvarv01.csv")
write.csv(SynVar.M025,"synvarm025.csv")
write.csv(SynVar.V025,"synvarv025.csv")
write.csv(SynVar.M05,"synvarm05.csv")
write.csv(SynVar.V05,"synvarv05.csv")
write.csv(SynVar.M10,"synvarm10.csv")
write.csv(SynVar.V10,"synvarv10.csv")
write.csv(SynVar.M25,"synvarm25.csv")
write.csv(SynVar.V25,"synvarv25.csv")
write.csv(SynVar.M50,"synvarm50.csv")
write.csv(SynVar.V50,"synvarv50.csv")



### Confirmatory Factor Analysis: 1% Random
fitcfa01 <- cfa(cfamodel,data=SynVar.V01,estimator="ULS")
summary(fitcfa01,fit.measures=TRUE, standardized=T,rsquare=T)
# RMSE=0.000,CFI=1.000,SRMR=.061

### Confirmatory Factor Analysis: 2.5% Random
fitcfa025 <- cfa(cfamodel,data=SynVar.V025,estimator="ULS")
summary(fitcfa025,fit.measures=TRUE, standardized=T,rsquare=T)
# RMSE=0.000,CFI=1.000,SRMR=.066

### Confirmatory Factor Analysis: 5% Random
fitcfa05 <- cfa(cfamodel,data=SynVar.V05,estimator="ULS")
summary(fitcfa05,fit.measures=TRUE, standardized=T,rsquare=T)
# RMSE=0.000,CFI=1.000,SRMR=.069

### Confirmatory Factor Analysis: 10% Random
fitcfa10 <- cfa(cfamodel,data=SynVar.V10,estimator="ULS")
summary(fitcfa10,fit.measures=TRUE, standardized=T,rsquare=T)
# RMSE=0.000,CFI=1.000,SRMR=.057

### Confirmatory Factor Analysis: 25% Random 
fitcfa25 <- cfa(cfamodel,data=SynVar.V25,estimator="ULS")
summary(fitcfa25,fit.measures=TRUE, standardized=T,rsquare=T)
# RMSE=0.000,CFI=1.000,SRMR=.066

### Confirmatory Factor Analysis: 50% Random
fitcfa50 <- cfa(cfamodel,data=SynVar.V50,estimator="ULS")
summary(fitcfa50,fit.measures=TRUE, standardized=T,rsquare=T)
# RMSE=0.000,CFI=1.000,SRMR=.057

### Confirmatory Factor Analysis: 90% Random
fitcfa90 <- cfa(cfamodel,data=SynVar.V90,estimator="ULS")
summary(fitcfa90,fit.measures=TRUE, standardized=T,rsquare=T)
# RMSE=0.000,CFI=1.000,SRMR=.066

######################

### Confirmatory Factor Analysis: 1% Missing
fitcfa01m <- cfa(cfamodel,data=SynVar.M01,estimator="ULS")
summary(fitcfa01m,fit.measures=TRUE, standardized=T,rsquare=T)
# RMSE=0.000,CFI=1.000,SRMR=.061

### Confirmatory Factor Analysis: 2.5% Missing
fitcfa025m <- cfa(cfamodel,data=SynVar.M025,estimator="ULS")
summary(fitcfa025m,fit.measures=TRUE, standardized=T,rsquare=T)
# RMSE=0.000,CFI=1.000,SRMR=.063

### Confirmatory Factor Analysis: 5% Missing
fitcfa05m <- cfa(cfamodel,data=SynVar.M05,estimator="ULS")
summary(fitcfa05m,fit.measures=TRUE, standardized=T,rsquare=T)
# RMSE=0.000,CFI=1.000,SRMR=.063

### Confirmatory Factor Analysis: 10% Missing
fitcfa10m <- cfa(cfamodel,data=SynVar.M10,estimator="ULS")
summary(fitcfa10m,fit.measures=TRUE, standardized=T,rsquare=T)
# RMSE=0.000,CFI=1.000,SRMR=.057

### Confirmatory Factor Analysis: 25% Missing 
fitcfa25m <- cfa(cfamodel,data=SynVar.V25,estimator="ULS")
summary(fitcfa25m,fit.measures=TRUE, standardized=T,rsquare=T)
# RMSE=0.000,CFI=1.000,SRMR=.060

### Confirmatory Factor Analysis: 50% Missing
fitcfa50m <- cfa(cfamodel,data=SynVar.M50,estimator="ULS")
summary(fitcfa50m,fit.measures=TRUE, standardized=T,rsquare=T)
# RMSE=0.000,CFI=1.000,SRMR=.057

### Confirmatory Factor Analysis: 90% Missing
fitcfa90m <- cfa(cfamodel,data=SynVar.V90,estimator="ULS")
summary(fitcfa90m,fit.measures=TRUE, standardized=T,rsquare=T)
# RMSE=0.000,CFI=1.000,SRMR=.066



