library(MASS)

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

qda.fit = qda(white~armed+cause+pov+h_income,data=train)
qda.pred = predict(qda.fit,test)

table(qda.pred$class,test$white)

mean(qda.pred$class==test$white)


