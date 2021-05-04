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
