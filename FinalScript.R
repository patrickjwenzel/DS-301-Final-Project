## Question 1: 
##
## Can you predict the race of the person killed by
## the police based on several of the given predictors?
## 
## Authors: Adam Ford, Joseph Strobel, Patrick Wenzel, Carter Wunsch, Allan Juarez

## EDA
knitr::opts_chunk$set(echo = FALSE)
options(dplyr.summarise.inform = FALSE)
options(dplyr.tibble.inform = FALSE)
options(scipen = 6)
library(tidyr)
library(dplyr)
library(ISLR)
library(kableExtra)
library(car)
library(leaps)
library(caret)
library(glmnet)
library(ROCR)
library(datasets)
library(tree)
library(class)
library(MASS)
library(RColorBrewer)



df <- read.csv("./thecounted-data/police_killings.csv", header = TRUE)



df$raceethnicity <- as.factor(df$raceethnicity)
df$state <- as.factor(df$state)
df$armed <- as.factor(df$armed)
df$date <- as.Date(paste(match(df$month, month.name), "/", df$day, "/", df$year, sep=""), format = "%m/%d/%Y")
str(df)



reGroup <- df %>% group_by(raceethnicity) %>%
  summarise(num_people = n())
ggplot(reGroup, aes(x = raceethnicity, y = num_people, color = raceethnicity, fill = raceethnicity)) + geom_bar(stat="identity") + ggtitle("Number of People Killed per Race/Ethnicity") + xlab("Race/Ethnicity") + ylab("Number of People") + theme(axis.text.x = element_text(angle = 90))



byMonth <- df %>% group_by(month) %>%
  summarise(numDeaths = n())
byMonth <- byMonth[order(match(byMonth$month, month.name)), ]
byMonth$month <- factor(byMonth$month, levels = month.name)
ggplot(byMonth, aes(x = month, y = numDeaths, group = 1)) + geom_line() + theme(axis.text.x = element_text(angle = 90))



armedGroup <- df %>% group_by(armed) %>%
  summarise(numPeople = n())
ggplot(armedGroup, aes(x = armed, y = numPeople, color = armed, fill = armed)) + geom_bar(stat="identity") + ggtitle("Number of People Killed per Armed Classification") + xlab("Armed Classification") + ylab("Number of People") + theme(axis.text.x = element_text(angle = 90))



classGroup <- df %>% group_by(cause) %>%
  summarise(numPeople = n())
classGroup
ggplot(classGroup, aes(x = cause, y = numPeople, color = cause, fill = cause)) + geom_bar(stat="identity") + ggtitle("Number of People per Cause of Death") + xlab("Cause of Death") + ylab("Number of People") + theme(axis.text.x = element_text(angle = 90))



ageGroup <- df[df$age != "Unknown",]
ageGroup$age[which(ageGroup$age == "40s")] <- "40"
ageGroup$age <- strtoi(ageGroup$age)
ggplot() + geom_histogram(data = ageGroup, aes(x=age), color = "black", fill = "blue", binwidth = 5)



df$state[which(df$state == "DC")] <- "VA"
df$State <- tolower(state.name[match(df$state, state.abb)])
df2 <- df
states <- map_data('state')
stateGroup <- df %>% group_by(State) %>%
  summarise(numPeople = n())
stateGroup <- full_join(states, stateGroup, by = c('region' = 'State'))
ggplot(stateGroup, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill=numPeople)) + 
  geom_path() + 
  scale_fill_gradientn(colours=colorRampPalette(brewer.pal(11, "Reds"))(100), na.value="grey90") + 
  coord_map() + 
  ggtitle('Number of People killed by State') + xlab('') + ylab('') + labs(fill = 'Number of People Killed') + 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())             



stateGroup2 <- df2 %>% group_by(State) %>%
  summarise(numPeople = n())
stateGroup2$numPeople[stateGroup2$State %in% c("california", "texas", "florida")] <- 0
stateGroup2 <- full_join(states, stateGroup2, by = c('region' = 'State'))
ggplot(stateGroup2, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill=numPeople)) + 
  geom_path() + 
  scale_fill_gradientn(colours=colorRampPalette(brewer.pal(9, "Reds"))(100), na.value="grey90") + 
  coord_map() + 
  ggtitle('Number of People killed by State') + xlab('') + ylab('') + labs(fill = 'Number of People Killed') + 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) 



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


## Question 2: 
##
## Can you predict if the victim was armed
## at the time of their death?
## 
## Authors: Allan Juarez, Carter Wunsch


# Cleaned data

set.seed(21)
killings = read.csv("C:\\Users\\Carter\\Documents\\ISU S2021\\DS 301\\final_project\\DS-301-Final-Project\\police_killings.csv")
sum(is.na(killings))
dim(killings)

killings = na.omit(killings)

sum(is.na(killings))
dim(killings)

killings$age = as.numeric(killings$age)
killings$pov = as.numeric(killings$pov)
killings$share_white = as.numeric(killings$share_white)
killings$share_black = as.numeric(killings$share_black)
killings$share_hispanic = as.numeric(killings$share_hispanic)
killings$p_income = as.numeric(killings$p_income)
killings$gender = as.numeric(killings$gender == "Male")
killings$armed = as.numeric(killings$armed != "No")

# Logistic regression with variables of interest

train = sample(1:nrow(killings),nrow(killings)/2, replace=FALSE)
test = (-train)

glm.fit = glm(armed~age+raceethnicity+comp_income+urate+cause+gender+college+pov, data=killings,subset=train,family='binomial')
glm.prob = predict(glm.fit,killings[test,],type='response') 
head(glm.prob)
glm.pred = rep(0,length(test))
glm.pred[glm.prob > .5] =1 
table(glm.pred,killings[test,]$armed)
1-mean(glm.pred == killings[test,]$armed)

# Trained lda model based on variables of interest

library(MASS)
lda.fit = lda(armed~age+raceethnicity+comp_income+urate+cause+gender+college+pov,data=killings, subset=train)
lda.pred = predict(lda.fit,killings[test,])
table(lda.pred$class,killings[test,]$armed)

# Subset selection to find optimal model predictors

library(leaps)
library(MASS)

regfit = regsubsets(armed~age+raceethnicity+comp_income+urate+cause+gender+college+pov,data=killings,nbest=1,nvmax=25, method="forward")

regfit.sum = summary(regfit)
regfit.sum

n = dim(killings)[1]
p = rowSums(regfit.sum$which) #number of predictors + intercept in the model 
adjr2 = regfit.sum$adjr2
cp = regfit.sum$cp
rss = regfit.sum$rss
AIC = n*log(rss/n) + 2*(p)
BIC = n*log(rss/n) + (p)*log(n)

c(AIC_min = which.min(AIC), BIC_min = which.min(BIC), adjr2_min = which.max(adjr2), cp_min = which.min(cp))
coef(regfit, 3)

# Trained log reg model based on 3 predictors from subset selection

train = sample(1:nrow(killings),nrow(killings)/2, replace=FALSE)
test = (-train)

glm.fit = glm(armed~age+raceethnicity+cause, data=killings,subset=train,family='binomial')
glm.prob = predict(glm.fit,killings[test,],type='response') 
head(glm.prob)

glm.pred = rep(0,length(test))
glm.pred[glm.prob > .3885] = 1 
table(glm.pred,killings[test,]$armed)
1-mean(glm.pred == killings[test,]$armed)


# Trained lda model based on 3 predictors from subset selection

lda.fit = lda(armed~age+raceethnicity+cause,data=killings, subset=train)
lda.pred = predict(lda.fit,killings[test,])
table(lda.pred$class,killings[test,]$armed)


library(e1071)
library(rpart)
library("rpart.plot")

training_indexes = sample(1:nrow(killings), size = nrow(killings)/2)
train = killings[training_indexes,]
test = killings[training_indexes,]

rpart.model <- rpart(armed ~ age+raceethnicity+cause, data = train)
rpart.pred <- predict(rpart.model, test[,-21], type = "vector")
rpart.plot(rpart.model)
