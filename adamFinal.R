police = read.csv("~/Classwork/D S 301/DS-301-Final-Project/police_killings.csv", header=TRUE)

set.seed(7)
train = sample(1:nrow(police),nrow(police)/2, replace=FALSE)
test = (-train)
police$age = as.numeric(police$age)
police$pov = as.numeric(police$pov)
police$college = scale(police$college)
police$urate = scale(police$urate)
police$white = as.numeric(police$raceethnicity == "White")
glm.fit = glm(white~age+gender+college+urate, data=police,subset=train,family='binomial')
glm.prob = predict(glm.fit,police[test,],type='response')
glm.pred = rep(0,length(test))
glm.pred[glm.prob >0.5] = 1
table(glm.pred,police[test,]$white)
1-mean(glm.pred == police[test,]$white)

glm.prob
library(ROCR)

ROCRpred = prediction(glm.prob,police[test,]$white)
plot(performance(ROCRpred,'tpr','fpr'))
plot(performance(ROCRpred,'tpr','fpr'),colorize=TRUE,print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

plot(performance(ROCRpred,'tnr','fnr'),colorize=TRUE,print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

perf = performance(ROCRpred,'tnr','fnr')
thresholds = data.frame(threshold = perf@alpha.values[[1]],fnr = perf@x.values[[1]], tnr = perf@y.values[[1]])
head(thresholds)
subset(thresholds,fnr<0.1)
