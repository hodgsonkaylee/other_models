library(knitr)
library(dplyr)
library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(lme4)
library(XML)
library(ggfortify)
library(Rmisc)

#########################################################################################################
#########################################################################################################

#########################################
######### Scrape and Clean Data #########
#########################################

#STUDIO#
# from SuperheroStudioCode.R run for Fall 2017 Project

studio <- read.csv(header=TRUE,stringsAsFactors=FALSE,text='
                   "","Title","Studio","Comic"
                   "34","Ant-Man","BV","Marvel"
                   "1","Avengers","BV","Marvel"
                   "3","Avengers: Age of Ultron","BV","Marvel"
                   "20","Batman","WB","DC"
                   "54","Batman and Robin","WB","DC"
                   "27","Batman Begins","WB","DC"
                   "32","Batman Forever","WB","DC"
                   "37","Batman Returns","WB","DC"
                   "12","Batman v Superman: Dawn of Justice","WB","DC"
                   "24","Big Hero 6","BV","Marvel"
                   "63","Blade","NL","Marvel"
                   "59","Blade II","NL","Marvel"
                   "70","Blade: Trinity","NL","Marvel"
                   "6","Captain America: Civil War","BV","Marvel"
                   "36","Captain America: The First Avenger","Par.","Marvel"
                   "19","Captain America: The Winter Soldier","BV","Marvel"
                   "77","Catwoman","WB","DC"
                   "55","Daredevil","Fox","Marvel"
                   "9","Deadpool","Fox","Marvel"
                   "25","Doctor Strange","BV","Marvel"
                   "87","Elektra","Fox","Marvel"
                   "40","Fantastic Four (2005)","Fox","Marvel"
                   "68","Fantastic Four (2015)","Fox","Marvel"
                   "49","Fantastic Four: Rise of the Silver Surfer","Fox","Marvel"
                   "51","Ghost Rider","Sony","Marvel"
                   "71","Ghost Rider: Spirit of Vengeance","Sony","Marvel"
                   "50","Green Lantern","WB","DC"
                   "11","Guardians of the Galaxy","BV","Marvel"
                   "48","Hulk","Uni.","Marvel"
                   "14","Iron Man","Par.","Marvel"
                   "15","Iron Man 2","Par.","Marvel"
                   "5","Iron Man 3","BV","Marvel"
                   "73","Kick-Ass","Lions","Marvel"
                   "84","Kick-Ass 2","Uni.","Marvel"
                   "16","Man of Steel","WB","DC"
                   "7","Spider-Man","Sony","Marvel"
                   "8","Spider-Man 2","Sony","Marvel"
                   "10","Spider-Man 3","Sony","Marvel"
                   "13","Suicide Squad","WB","DC"
                   "96","Supergirl","Sony","DC"
                   "46","Superman","WB","DC"
                   "52","Superman II","WB","DC"
                   "66","Superman III","WB","DC"
                   "94","Superman IV: The Quest for Peace","WB","DC"
                   "30","Superman Returns","WB","DC"
                   "17","The Amazing Spider-Man","Sony","Marvel"
                   "29","The Amazing Spider-Man 2","Sony","Marvel"
                   "2","The Dark Knight","WB","DC"
                   "4","The Dark Knight Rises","WB","DC"
                   "56","The Green Hornet","Sony","DC"
                   "44","The Incredible Hulk","Uni.","Marvel"
                   "81","The Punisher","Lions","Marvel"
                   "89","The Spirit","Lions","DC"
                   "47","The Wolverine","Fox","Marvel"
                   "33","Thor","Par.","Marvel"
                   "28","Thor: The Dark World","BV","Marvel"
                   "53","Watchmen","WB","DC"
                   "38","X-Men","Fox","Marvel"
                   "35","X-Men Origins: Wolverine","Fox","Marvel"
                   "39","X-Men: Apocalypse","Fox","Marvel"
                   "22","X-Men: Days of Future Past","Fox","Marvel"
                   "42","X-Men: First Class","Fox","Marvel"
                   "21","X-Men: The Last Stand","Fox","Marvel"
                   "26","X2: X-Men United","Fox","Marvel"
                   ')
studio<-studio[,-1]

#TOMATOMETER#

url2 <- "http://www.superheronation.com/2011/08/22/rotten-tomatoes-ratings-for-superhero-movies/"
# (current as of April 2016)
webpage2 <- htmlParse(url2)
tomato1a <- readHTMLTable(webpage2,which=1,header=TRUE,colClasses=c("character","numeric"),stringsAsFactors=FALSE)
tomato2a <- readHTMLTable(webpage2,which=2,header=TRUE,colClasses=c("character","numeric"),stringsAsFactors=FALSE)
tomato3a <- readHTMLTable(webpage2,which=3,header=TRUE,colClasses=c("character","numeric"),stringsAsFactors=FALSE)

# throw out average row
tomato1b <- tomato1a[-63,]
tomato2b <- tomato2a[-14,]
tomato3b <- tomato3a[-7,]

# merge for total tomatometer
tomato4 <- rbind(tomato1b,tomato2b,tomato3b)
names(tomato4)[2] <- "Tomato"

# rename movies for better merging
tomato4$Title[4] <- "Iron Man"
tomato4$Title[13] <- "The Dark Knight Rises"
tomato4$Title[28] <- "Iron Man 2"
tomato4$Title[27] <- "The Amazing Spider-Man"
tomato4$Title[36] <- "Hulk"
tomato4$Title[38] <- "X-Men: The Last Stand"
tomato4$Title[11] <- "X2: X-Men United"
tomato4$Title[32] <- "The Incredible Hulk"
tomato4$Title[58] <- "Ghost Rider: Spirit of Vengeance"
tomato4$Title[53] <- "Blade: Trinity"
tomato4$Title[43] <- "The Green Hornet"
tomato4$Title[51] <- "Punisher: War Zone"
tomato4$Title[26] <- "Avengers: Age of Ultron"
tomato4$Title[10] <- "Captain America: Civil War"
tomato4$Title[33] <- "Thor: The Dark World"

# add in newer movies and their ratings
addin<-read.table(header=TRUE,text='
                  Title Tomato
                  "Big Hero 6" 89
                  "Captain America: The Winter Soldier" 89
                  "Doctor Strange" 91
                  "Suicide Squad" 26
                  "The Amazing Spider-Man 2" 53
                  "X-Men: Apocalypse" 48
                  ')
tomato5 <- rbind(tomato4,addin)

# alphabatize titles
tomato <- tomato5[order(tomato5$Title),]


#PRODUCTION BUDGET#
setClass("AccountingNumber")
setAs("character", "AccountingNumber",function(from) as.numeric(gsub(",","",gsub("[:$:]","",from))))

# website separated into 100 subsets ...
url3 <- "http://www.the-numbers.com/movie/budgets/all"
all.pages<-seq(from=1,to=5401,by=100)
production1<-NULL
for(page in all.pages){
  webpage3 <- htmlParse(paste(url3,page,sep="/"))
  
  production1a <- readHTMLTable(webpage3,which=1,header=TRUE,colClasses=c("numeric","character","character",
                                                                          "AccountingNumber","AccountingNumber","AccountingNumber"),stringsAsFactors=FALSE)
  production1b <- production1a[-seq(2,nrow(production1a),2),]
  
  production1<-rbind(production1,production1b)
}

# keep only columns we need, rename movie to "Title"
production2a <- production1[,c(3,4)]
names(production2a) <- c("Title","Budget")

# handle some individual movies with different names across the data sets for easier merging
addin<-read.table(header=TRUE,text='
                  Title Budget
                  "Supergirl"              35000000
                  "Avengers"              225000000
                  "Batman and Robin"      125000000
                  "Fantastic Four (2005)"  87500000
                  "Fantastic Four (2015)" 120000000
                  "X2: X-Men United"      125000000
                  "Blade II"               54000000
                  ')

production3 <- rbind(production2a,addin)

# alphabatize titles
production <- production3[order(production3$Title),]


#IMDb USER RATING#

url4 <- "http://www.imdb.com/list/ls051507615/?start=1&view=compact&sort=listorian:asc&defaults=1&scb=0.7495568785816431"
webpage4 <- htmlParse(url4)

user1a <- readHTMLTable(webpage4,which=1,header=TRUE,colClasses=c("numeric","character","character","character",
                                                                  "character","numeric","numeric","character"),stringsAsFactors=FALSE)

# keep only Title and IMDb score
user2a <- user1a[,c(2,7)]
names(user2a) <- c("Title","IMDb")

# rename movies for better merging and add the amazing spider-man 2 and others that aren't on the webpage
# note: the row counter on the webpage skips row 7 !
user2a$Title[61] <- "Avengers"
user2a$Title[38] <- "Batman and Robin"
user2a$Title[25] <- "Fantastic Four (2015)"
user2a$Title[68] <- "Fantastic Four (2005)"
user2a$Title[69] <- "Fantastic Four: Rise of the Silver Surfer"
user2a$Title[44] <- "X2: X-Men United"
user2a$Title[78] <- "The Punisher (1989)"

addin2<-read.table(header=TRUE,text='
                   Title IMDb
                   "The Amazing Spider-Man 2"            6.8
                   "Big Hero 6"                          7.9
                   "Batman v Superman: Dawn of Justice"  6.8
                   "Captain America: Civil War"          8.0
                   "Suicide Squad"                       6.5
                   "X-Men: Apocalypse"                   7.2
                   "X-Men: Days of Future Past"          8.0
                   ')
user2a <- rbind(user2a,addin2)

# movies with score 0 haven't happened yet
user2b <- subset(user2a,IMDb != 0)

# alphabatize titles
user <- user2b[order(user2b$Title),]


# final merge in dataset
superhero <- merge(studio,tomato)
superhero <- merge(superhero,production)
superhero <- merge(superhero,user)

# clean up
superhero$Studio <- factor(superhero$Studio)
superhero$Studio <- relevel(superhero$Studio,"WB")
superhero$Comic <- factor(superhero$Comic)
superhero$Budget <- superhero$Budget * 10^-6
superhero$Title[33]<-"Kick ***"
superhero$Title[34]<-"Kick *** 2"

# table of summary statistics
by(superhero$IMDb,superhero$Comic,mean)
by(superhero$IMDb,superhero$Comic,sd)

by(superhero$Budget,superhero$Comic,mean)
by(superhero$Budget,superhero$Comic,sd)

by(superhero$Tomato,superhero$Comic,mean)
by(superhero$Tomato,superhero$Comic,sd)

table(superhero$Comic)

#########################################################################################################
#########################################################################################################

######################################
######### Summary Statistics #########
######################################

# Summary Tables

# Function to find the summary statistics for numerical variables
my.summary <- function(x, na.rm=TRUE){c( Mean=mean(x, na.rm=TRUE), Min=min(x), q1=quantile(x,probs=0.025) , 
                                                  Median=median(x), q3=quantile(x,probs=0.975)  , Max=max(x))}

# Subset variables by type of Comic for comparison in summary statistics tables
budget.m <- subset(superhero,Comic=="Marvel")$Budget
budget.dc <- subset(superhero,Comic=="DC")$Budget
tomato.m <- subset(superhero,Comic=="Marvel")$Tomato
tomato.dc <- subset(superhero,Comic=="DC")$Tomato
imdb.m <- subset(superhero,Comic=="Marvel")$IMDb
imdb.dc <- subset(superhero, Comic=="DC")$IMDb
studio.m <- subset(superhero, Comic=="Marvel")$Studio
studio.dc <- subset(superhero, Comic=="DC")$Studio

### Create summary Statistics Tables ###

# Summary Statistics for Movie Budget
dataframe.b <- round(data.frame(matrix(cbind(my.summary(superhero$Budget),my.summary(budget.m),my.summary(budget.dc)),nrow=3,byrow=T)),2)
colnames(dataframe.b) <- c("Mean","Min", "Q1","Median","Q3","Max")
rownames(dataframe.b) <- c("Overall","Marvel","DC")
kable(dataframe.b, caption="Summary Statistics for Movie Budget")

# Summary Statistics for Tomato Ratings
dataframe.t <- round(data.frame(matrix(cbind(my.summary(superhero$Tomato),my.summary(tomato.m),my.summary(tomato.dc)),nrow=3,byrow=T)),2)
colnames(dataframe.t) <- c("Mean","Min", "Q1","Median","Q3","Max")
rownames(dataframe.t) <- c("Overall","Marvel","DC")
kable(dataframe.t, caption="Summary Statistics for Tomato ratings")

# Summary Statistics for IMDb Ratings
dataframe.i <- round(data.frame(matrix(cbind(my.summary(superhero$IMDb),my.summary(imdb.m),my.summary(imdb.dc)),nrow=3,byrow=T)),2)
colnames(dataframe.i) <- c("Mean","Min", "Q1","Median","Q3","Max")
rownames(dataframe.i) <- c("Overall","Marvel","DC")
kable(dataframe.i,caption="Summary Statistics for IMDb ratings")

# Summary Statistics for Studio type
desc.tot <- count(superhero, "Studio")
desc.m <- count(studio.m)
desc.dc <- count(studio.dc)
dataframe.s <- data.frame(matrix(c(desc.tot[1:8,2], 0, desc.m[1:7,2], desc.dc[1,2], 0,0,desc.dc[2,2],0,0,desc.dc[3,2],0),nrow=3,byrow=T))
colnames(dataframe.s) <- c("WB","BV","Fox","Lions","NL","Par.","Sony","Uni.")
rownames(dataframe.s) <- c("Overall","Marvel","DC")
kable(dataframe.s,caption="Summary Statistics for Studio Type")

#########################################################################################################
#########################################################################################################

#####################################
######### Statistical Model #########
#####################################

fit <- lm(IMDb~Studio+Budget+Tomato+Comic+Comic:Budget, data=superhero)
X <- model.matrix(IMDb~Studio+Budget+Tomato+Comic+Comic:Budget, data=superhero)
y <- superhero$IMDb
beta <- coef(fit)
coeff <- summary(fit)

# Reparameterized model used with an interaction term for Comic and Budget. 
# Coefficients in terms of the reparameterized model

#########################################################################################################
#########################################################################################################

####################################
######### Data Diagnostics #########
####################################

# Model Assumptions

# Collinearity: VIF
vif <- diag(solve(cor(X[-1,-1])))

# Constant variance and Normality: Residuals vs. Fitted plot and QQ plot
superhero$predicted <- predict(fit)
superhero$residuals <- residuals(fit)
pdf("diag.pdf")
autoplot(fit, which=c(1,2), label.size = 3)
dev.off()

# Influential Points: Leverage 
# Leverage Rule of Thumb: leverage > 2*(k+1)/n
k <- ncol(fit.matrix)-1
hat.diag <- lm.influence(fit)$hat
lev <- superhero[hat.diag > 2*(k+1)/length(y),]
leverage <- data.frame(lev[,2])

# Influential Points: Cook's Distance
# Cook's Distance Rule of Thumb: leverage > 4/(n-k-1)
cook.diag <- cooks.distance(fit)
cooks <- superhero[cook.diag > 4/(length(y)-k-1),]
Cooks <- cooks[,1]
# Observation #34 has high cook's distance

# Good vs. Bad Influence
# Good if the outlier provides information regarding a point on the x-axis that we otherwise wouldn't have had information on. The expands the inference potential of our model
# Bad if the outlier is at a point where we have other observations. This increases the variance and decreases the probability of finding significant coefficients in our model

# Outliers: R-Studentized Residuals
# high R-studentized Residuals > 2
residual <- rstudent(fit)
res <- superhero[abs(residual) > 2,]
# Observations #4,17,29,34,45 all have high r-studentized residuals

# Plot influential and outlier points
# Assign colors for influential points using for and if statements
film <- superhero$Title
col <- rep(NA,length(film))
for(i in 1:length(film)){
  if (film[i] =="Batman" || film[i] == "Catwoman" || film[i] == "Hulk" || film[i] == "Superman Returns") {
    col[i] <- "blue"
  } else if (film[i] == "Kick *** 2") {
    col[i] <- "purple"
  } else {col[i] <- "black"}
}
color <- col

# plot the scatterplot with the different colors for influential points
pdf("outlierinf.pdf")
budg.pl <- ggplot(data=superhero, aes(x=Budget, y=IMDb,colour=color)) + geom_point(aes(colour=color)) + labs(x="Budget",y="IMDb") + theme(legend.position="none") + scale_colour_manual(labels = c("Neither", "Outlier", "Influential Point and Outlier"), values=c("black","red","purple"))
tom.pl <- ggplot(data=superhero, aes(x=Tomato, y=IMDb,colour=color)) + geom_point(aes(colour=color)) + labs(x="Tomato",y="IMDb") + theme(legend.position = c(0.8, 0.2)) + scale_size(range=c(4,5)) + scale_colour_manual(labels = c("Neither", "Outlier", "Both"), values=c("black","red","purple"))
comic.pl <- ggplot(data=superhero, aes(x=Comic, y=IMDb,colour=color)) + geom_point(aes(colour=color)) + labs(x="Comic",y="IMDb") + theme(legend.position="none") + scale_colour_manual(labels = c("Neither", "Outlier", "Influential Point and Outlier"), values=c("black","red","purple"))
stud.pl <- ggplot(data=superhero, aes(x=Studio, y=IMDb,colour=color)) + geom_point(aes(colour=color)) + labs(x="Studio",y="IMDb") + theme(legend.position="none") + scale_colour_manual(labels = c("Neither", "Outlier", "Influential Point and Outlier"), values=c("black","red","purple"))
grid.arrange(budg.pl,tom.pl,comic.pl,stud.pl,ncol=2)
dev.off()

#########################################################################################################
#########################################################################################################

#####################################
########## Set Up Analysis ##########
#####################################

IMDb <- as.vector(superhero.update$IMDb)
Budget <- as.vector(superhero.update$Budget)
Tomato <- as.vector(superhero.update$Tomato)
Comic <- as.vector(superhero.update$Comic)
Studio <- as.vector(superhero.update$Studio)
Comicfac <- as.factor(superhero.update$Comic)
Studiofac <- as.factor(superhero.update$Studio)

M.matrix <- matrix(c(1,0,0,0,0,0,0,0,0,0,0,0,
                     1,1,0,0,0,0,0,0,0,0,0,0,
                     1,0,1,0,0,0,0,0,0,0,0,0,
                     1,0,0,1,0,0,0,0,0,0,0,0,
                     1,0,0,0,1,0,0,0,0,0,0,0,
                     1,0,0,0,0,1,0,0,0,0,0,0,
                     1,0,0,0,0,0,1,0,0,0,0,0,
                     1,0,0,0,0,0,0,1,0,0,0,0,
                     0,0,0,0,0,0,0,0,1,0,0,0,
                     0,0,0,0,0,0,0,0,0,1,0,0,
                     1,0,0,0,0,0,0,0,0,0,1,0,
                     0,0,0,0,0,0,0,0,1,0,0,1), byrow=TRUE, ncol=12)

# coefficient vector for cell means model
theta.cm <- M.matrix%*%coef(fit)

#########################################
########### Hypothesis Tests ############
#########################################

##### Test for Main Effect of Comic on IMDb #####

C <- matrix(c(1,0,0,0,0,0,0,0,0,0,-1,0,
              0,0,0,0,0,0,0,0,1,0,0,-1), byrow=TRUE, ncol=12)
C.star <- C%*%M.matrix
theta <- coef(fit)
df <- nrow(C)
n <- nrow(fit.matrix)
p <- ncol(fit.matrix)

F.num <- t(C.star %*% theta) %*% solve(C.star %*% solve(t(X) %*% X) %*% t(C.star)) %*% C.star %*% theta
s2 <- 1/(n-p) * t(Y - X %*% theta) %*% (Y - X %*% theta)
F.stat <- F.num/(df*s2)
F.stat
1 - pf(F.stat,df,n-p)

# Check answer with anova function
fit.add <- lm(IMDb~Studio+Budget+Tomato,data=superhero)
anova(fit.add,fit)

# Plot the effect
# calculate the group means and confidence intervals to plot in the bar-error plot
meanci.comic <- group.CI(IMDb ~ Comic, data=superhero, ci = 0.95)
for(i in 1:2) meanci.comic[i,2] <- meanci.comic[i,2]-meanci.comic[i,3]
dat.comic <- setNames(meanci.comic, c("Comic","u", "IMDb","l"))

pdf("comiceffect.pdf")
ggplot(dat.comic, aes(x=Comic, y=IMDb)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_errorbar(aes(ymin=IMDb+u,ymax=IMDb-u),colour="black",width = .5) +
  geom_point(position=position_dodge(.9), aes(y=IMDb))
dev.off()

##### Test for Interaction of Comic and Budget #####

C <- matrix(c(0,0,0,0,0,0,0,0,1,0,0,-1), byrow=TRUE, ncol=12)
C.star <- C%*%M.matrix
theta <- coef(fit)
df <- nrow(C)
n <- nrow(fit.matrix)
p <- ncol(fit.matrix)

F.num <- t(C.star %*% theta) %*% solve(C.star %*% solve(t(X) %*% X) %*% t(C.star)) %*% C.star %*% theta
s2 <- 1/(n-p) * t(Y - X %*% theta) %*% (Y - X %*% theta)
F.stat <- F.num/(df*s2)
F.stat
1 - pf(F.stat,df,n-p)

# Check answer with anova function
fit.add <- lm(IMDb~Studio+Budget+Tomato+Comic,data=superhero)
anova(fit.add,fit)

# Plot the interaction effect
pdf("interact.pdf")
ggplot(superhero, aes(x = Budget, y = IMDb,color=Comic)) + 
  geom_point()  + 
  geom_abline(intercept = coef(fit)["(Intercept)"], slope = coef(fit)["Budget"],col="tomato1") +
  geom_abline(intercept = coef(fit)["(Intercept)"] + coef(fit)["ComicMarvel"],slope=coef(fit)["Budget"]+coef(fit)["Budget:ComicMarvel"],col="turquoise3")
dev.off()

##### Test for Main Effect of Budget on IMDb #####

C <- matrix(c(0,0,0,0,0,0,0,0,1,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,1), byrow=TRUE, ncol=12)
C.star <- C%*%M.matrix
theta <- coef(fit)
df <- nrow(C)
n <- nrow(fit.matrix)
p <- ncol(fit.matrix)

F.num <- t(C.star %*% theta) %*% solve(C.star %*% solve(t(X) %*% X) %*% t(C.star)) %*% C.star %*% theta
s2 <- 1/(n-p) * t(Y - X %*% theta) %*% (Y - X %*% theta)
F.stat <- F.num/(df*s2)
F.stat
1 - pf(F.stat,df,n-p)

# Check answer with anova function
fit.add <- lm(IMDb~Studio+Tomato+Comic,data=superhero)
anova(fit.add,fit)

##### Test for Main Effect of Tomato on IMDb #####

C <- matrix(c(0,0,0,0,0,0,0,0,0,1,0,0), byrow=TRUE, ncol=12)
C.star <- C%*%M.matrix
theta <- coef(fit)
df <- nrow(C)
n <- nrow(fit.matrix)
p <- ncol(fit.matrix)

F.num <- t(C.star %*% theta) %*% solve(C.star %*% solve(t(X) %*% X) %*% t(C.star)) %*% C.star %*% theta
s2 <- 1/(n-p) * t(Y - X %*% theta) %*% (Y - X %*% theta)
F.stat <- F.num/(df*s2)
F.stat
1 - pf(F.stat,df,n-p)

# Check answer with anova function
fit.add <- lm(IMDb~Studio+Comic+Budget+Comic*Budget,data=superhero)
anova(fit.add,fit)

pdf("tomato.pdf")
ggplot(superhero, aes(x = Tomato, y = IMDb)) + 
  geom_point()  + geom_abline(intercept = coef(fit)["(Intercept)"], slope = coef(fit)["Tomato"]) 
dev.off()

##### Test for any effect of Studio #####

C <- matrix(c(1,-1,0,0,0,0,0,0,0,0,0,0,
              0,1,-1,0,0,0,0,0,0,0,0,0,
              0,0,1,-1,0,0,0,0,0,0,0,0,
              0,0,0,1,-1,0,0,0,0,0,0,0,
              0,0,0,0,1,-1,0,0,0,0,0,0,
              0,0,0,0,0,1,-1,0,0,0,0,0,
              0,0,0,0,0,0,1,-1,0,0,0,0), byrow=TRUE, ncol=12)
C.star <- C%*%M.matrix
theta <- coef(fit)
df <- nrow(C)
n <- nrow(fit.matrix)
p <- ncol(fit.matrix)

F.num <- t(C.star %*% theta) %*% solve(C.star %*% solve(t(X) %*% X) %*% t(C.star)) %*% C.star %*% theta
s2 <- 1/(n-p) * t(Y - X %*% theta) %*% (Y - X %*% theta)
F.stat <- F.num/(df*s2)
F.stat
1 - pf(F.stat,df,n-p)

# Check answer with anova function
fit.add <- lm(IMDb~Budget+Tomato+Comic+Comic:Budget,data=superhero)
anova(fit.add,fit)

# Plot the effect
# calculate the group means and confidence intervals to plot in the bar-error plot
meanci.studio <- group.CI(IMDb ~ Studio, data=superhero, ci = 0.95)
for(i in 1:8) meanci.studio[i,2] <- meanci.studio[i,2]-meanci.studio[i,3]
dat.studio <- setNames(meanci.studio, c("Studio","u", "IMDb","l"))

pdf("studioeffect.pdf")
ggplot(dat.studio, aes(x=Studio, y=IMDb)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_errorbar(aes(ymin=IMDb+u,ymax=IMDb-u),colour="black",width = .5) +
  geom_point(position=position_dodge(.9), aes(y=IMDb))
dev.off()

# Plot additive effect
intercepts <- c(coef(fit)["(Intercept)"],coef(fit)["(Intercept)"] + coef(fit)["StudioBV"],coef(fit)["(Intercept)"] + coef(fit)["StudioFox"],coef(fit)["(Intercept)"] + coef(fit)["StudioLions"],coef(fit)["(Intercept)"] + coef(fit)["StudioNL"],coef(fit)["(Intercept)"] + coef(fit)["StudioPar."],coef(fit)["(Intercept)"] + coef(fit)["StudioSony"],coef(fit)["(Intercept)"] + coef(fit)["StudioUni."])
pdf("add.pdf")
bud <- ggplot(superhero, aes(x = Budget, y = IMDb,color=Studio)) + 
  geom_point()  + geom_abline(intercept = intercepts, slope = rep(coef(fit)["Budget"],8),aes(color=Studio)) + theme(legend.position="none")
tom <- ggplot(superhero, aes(x = Tomato, y = IMDb,color=Studio)) + 
  geom_point()  + geom_abline(intercept = intercepts, slope = rep(coef(fit)["Tomato"],8),aes(color=Studio)) + theme(legend.position=c(0.8,0.2))  
grid.arrange(bud,tom,ncol=2)
dev.off()








