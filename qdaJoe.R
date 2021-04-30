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

### Try subset selection

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


