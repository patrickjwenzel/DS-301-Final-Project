## Question 1: 
##
## Can you predict the race of the person killed by
## the police based on several of the given predictors?
## 
## Authors: Adam Ford, Joseph Strobel

## Attempt 1
police = read.csv("~/Classwork/D S 301/DS-301-Final-Project/police_killings.csv", header=TRUE)
library(tidyr)
library(leaps)
library(neuralnet)
#LOGISTIC REGRESSION for Q1
#Remove missing rows
police = police[!police$name == "Unknown",]
police = police[!is.na(police$college),]
set.seed(7)

train = sample(1:nrow(police),nrow(police)/2, replace=FALSE)
test = (-train)

#Clean and scale variables
police$age = as.numeric(police$age)
meanAge = mean(police$age, na.rm = TRUE)
police$age = replace_na(police$age, meanAge)
police$age = scale(police$age)

police$pov = as.numeric(police$pov)
meanPov = mean(police$pov, na.rm = TRUE)
police$pov = replace_na(police$pov, meanPov)

police$college = scale(police$college)

police$urate = scale(police$urate)

police$pov = scale(police$pov)

police$white = as.numeric(police$raceethnicity == "White")

#Intial Run #1
glm.fit = glm(white~age+gender+armed, data=police,subset=train,family='binomial')
glm.prob = predict(glm.fit,police[test,],type='response')
glm.pred = rep(0,length(test))
glm.pred[glm.prob >0.5] = 1
table(glm.pred,police[test,]$white)
mean(glm.pred == police[test,]$white)

#Intial Run #2
glm.fit = glm(white~college*urate*pov, data=police,subset=train,family='binomial')
glm.prob = predict(glm.fit,police[test,],type='response')
glm.pred = rep(0,length(test))
glm.pred[glm.prob >0.5] = 1
table(glm.pred,police[test,]$white)
mean(glm.pred == police[test,]$white)

#Subset Selection
regfit = regsubsets(white~age+gender+armed+college*urate*pov, data=police, nvmax=6)
n = dim(police)[1]
regfit.sum = summary(regfit)
p = rowSums(regfit.sum$which)
adjr2 = regfit.sum$adjr2
cp = regfit.sum$cp
rss = regfit.sum$rss
AIC = n*log(rss/n) + 2*(p)
BIC = n*log(rss/n) + (p)*log(n)
barplot(AIC)
barplot(BIC)
coef(regfit, 4)

#Run the best subset
glm.fit = glm(white~armed+age+college*urate*pov, data=police,subset=train,family='binomial')
glm.prob = predict(glm.fit,police[test,],type='response')
glm.pred = rep(0,length(test))
glm.pred[glm.prob >0.5] = 1
table(glm.pred,police[test,]$white)
mean(glm.pred == police[test,]$white)

#NEURAL NETWORK for Q1
police$inter <- police$college * police$urate * police$pov
n = neuralnet(white~age+inter, data=police[train,], hidden=c(2,1), linear.output=FALSE, threshold=0.01)
plot(n)
temp = subset(police[test,], select = c(age, inter))
n.results = compute(n, temp)
res = data.frame(actual = police[test,]$white, prediction = n.results$net.result)
rounded = sapply(res, round, digits=0)
roun = data.frame(rounded)
attach(roun)
table(actual, prediction)
mean(roun$actual == roun$prediction)


## Attempt 2
library(MASS)
library(leaps)

Police <- read.csv("/Users/joestrobel/Desktop/DS-301-Final-Project/police_killings.csv")

Police$white = as.numeric(Police$raceethnicity == "White")
Police$armed = as.numeric(Police$armed == "Firearm")
Police$cause = as.numeric(Police$cause == "Gunshot")
Police$gender <- as.factor(Police$gender)
Police$armed <- as.factor(Police$armed)
Police$age <- as.numeric(Police$age, na.rm = TRUE)
Police$cause <- as.factor(Police$cause)
Police$pov <- as.numeric(Police$pov)
Police$share_white <- as.numeric(Police$share_white)
str(Police)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(Police))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(Police)), size = smp_size)

train <- Police[train_ind, ]
test <- Police[-train_ind, ]

### Try picking some predictors that we think may be useful

qda.fit = qda(white~age+pov+armed,data=train) 
qda.pred = predict(qda.fit,test)

mean(qda.pred$class==test$white)
table(qda.pred$class,test$white)

### Forward subset selection

regfit = regsubsets(white~age+cause+armed+pov+college+h_income,data=train,nbest=1,nvmax=6, method = "forward")
regfit.sum = summary(regfit)

n = dim(train)[1]
p = rowSums(regfit.sum$which) #number of predictors + intercept in the model 
adjr2 = regfit.sum$adjr2
cp = regfit.sum$cp
rss = regfit.sum$rss
AIC = n*log(rss/n) + 2*(p)
BIC = n*log(rss/n) + (p)*log(n)

plot(AIC)
plot(BIC)
plot(adjr2)
plot(cp)

coef(regfit, 3)

qda.fit = qda(white~age+pov+h_income,data=train)
qda.pred = predict(qda.fit,test)

mean(qda.pred$class==test$white)
table(qda.pred$class,test$white)