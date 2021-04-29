police = read.csv("~/Classwork/D S 301/DS-301-Final-Project/police_killings.csv", header=TRUE)
library(tidyr)
police = police[!police$name == "Unknown",]
set.seed(7)
train = sample(1:nrow(police),nrow(police)/2, replace=FALSE)
test = (-train)

police$age = as.numeric(police$age)
meanAge = mean(police$age, na.rm = TRUE)
police$age = replace_na(police$age, meanAge)

police$pov = as.numeric(police$pov)
meanPov = mean(police$pov, na.rm = TRUE)
police$pov = replace_na(police$pov, meanPov)

police$college = scale(police$college)[1,]

police$urate = scale(police$urate)[1,]
police$urate

police$white = as.numeric(police$raceethnicity == "White")


glm.fit = glm(white~age+gender, data=police,subset=train,family='binomial')
glm.prob = predict(glm.fit,police[test,],type='response')
glm.pred = rep(0,length(test))
glm.pred[glm.prob >0.5] = 1
table(glm.pred,police[test,]$white)
1-mean(glm.pred == police[test,]$white)


#No important type of error to avoid, neither is more important


glm.fit = glm(white~college+urate+pov, data=police,subset=train,family='binomial')
glm.prob = predict(glm.fit,police[test,],type='response')
glm.pred = rep(0,length(test))
glm.pred[glm.prob >0.5] = 1
table(glm.pred,police[test,]$white)
1-mean(glm.pred == police[test,]$white)

