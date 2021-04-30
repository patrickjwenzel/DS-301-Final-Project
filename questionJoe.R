## Question: 
##
## Can you predict the race of the person killed by
## the police based on several of the given predictors?
## 
## Authors: Adam Ford, Joseph Strobel

#install.packages('tree')
library(tree)

Police <- read.csv("/Users/joestrobel/Desktop/DS-301-Final-Project/police_killings.csv")
Police$white = as.numeric(Police$raceethnicity == "White")

str(Police)
Police$raceethnicity <- as.factor(Police$raceethnicity)
Police$gender <- as.factor(Police$gender)
Police$armed <- as.factor(Police$armed)
Police$age <- as.numeric(Police$age, na.rm = TRUE)
str(Police)

train = sample(1:nrow(Police),nrow(Police)/2)

tree.Police = tree(raceethnicity~age+gender+latitude+armed+h_income+college,data=Police, subset=train)

tree.Police
summary(tree.Police) # only 5 predictors used in tree construction
# deviance: think of as the sum squared errors for the tree

par(mfrow=c(1,1))
plot(tree.Police)
text(tree.Police,pretty=0)

test =  Police[-train,]
tree.pred = predict(tree.Police, newdata=test)

Y.test = Police[-train,"raceethnicity"]
mean((tree.pred - Y.test)^2)

## pruning ##
# alpha 
# Tree fit + alpha *tree size

cv.Police = cv.tree(tree.Police) 
#performs CV in order to determine the optimal level of tree complexity. 

cv.Police 

#size: number of terminal nodes
#dev: CV error
#k: corresponds to alpha in our notation (tuning parameter)

plot(cv.Police$size,cv.Police$dev,type='b') # plot CV error as a function of size of tree 

prune.Police= prune.tree(tree.Police,best=7) 
#prune tree to be a specific size

plot(prune.Police)
text(prune.Police,pretty=0)

tree.prune = predict(prune.Police,newdata=test)
mean((tree.prune - Y.test)^2)

