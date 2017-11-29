rm(list = ls())

require(ISLR)
require(MASS)
require(class)
require(boot)

confusion <- function(true, pred) {
  conf.table <- table(true, pred)
  
  acc <- (conf.table[1] + conf.table[4])/sum(conf.table)
  err <- (conf.table[2] + conf.table[3])/sum(conf.table)
  #type1 <- conf.table[2]/(conf.table[1] + conf.table[2])
  #type2 <- conf.table[3]/(conf.table[3] + conf.table[4])
  type1 <- conf.table[1,2]/sum(conf.table[1,])
  type2 <- conf.table[2,1]/sum(conf.table[2,])
  power <- 1 - type2
  #precision <- conf.table[4]/(conf.table[2] + conf.table[4])
  precision <- conf.table[2,2]/sum(conf.table[,2])
  
  print(paste('Overall Accuracy:', acc))
  print(paste('Overall Error Rate:', err))
  print(paste('Type 1 Error Rate:', type1))
  print(paste('Type 2 Error Rate:', type2))
  print(paste('Power:', power))
  print(paste('Precision:', precision))
  
  return(conf.table)
}

##############
##QUESTION 1##
##############
set.seed(5072)

weekly.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
print(summary(weekly.fit))

#Lag2 is the only significant predictor outside of the intercept
#decision boundary of .5

weekly.pred <- predict(weekly.fit, type = 'response')
weekly.class <- rep('Down', length(weekly.pred))
weekly.class[weekly.pred > .5] <- 'Up'

print('Model based on all predictors')
allpred <- confusion(Weekly$Direction, weekly.class)

yrweekly <- Weekly[Weekly$Year < 2009,]
rmweekly <- Weekly[Weekly$Year > 2008,]

yrweekly.fit <- glm(Direction ~ Lag2, data = yrweekly, family = binomial)

rmweekly.pred <- predict(yrweekly.fit, rmweekly, type = 'response')
rmweekly.class <- rep('Down', length(rmweekly.pred))
rmweekly.class[rmweekly.pred > .5] <- 'Up'

print('')
print('Model based on Lag2 and used 2009/2010 as test set')
rmtable <- confusion(rmweekly$Direction, rmweekly.class)

#LDA
yrweekly.fit <- lda(Direction ~ Lag2, data = yrweekly)
rmweekly.pred <- predict(yrweekly.fit, rmweekly)
rmweekly.class <- rmweekly.pred$class

print('')
print('LDA')
LDAtable <- confusion(rmweekly$Direction, rmweekly.class)
#identical to logistic regression

#QDA
yrweekly.fit <- qda(Direction ~ Lag2, data = yrweekly)
rmweekly.pred <- predict(yrweekly.fit, rmweekly)
rmweekly.class <- rmweekly.pred$class

print('')
print('QDA')
QDAtable <- confusion(rmweekly.class, rmweekly$Direction)
#predicts everything as up

#knn
train.x <- data.frame(yrweekly$Lag2)
test.x <- data.frame(rmweekly$Lag2)
train.direction <- yrweekly$Direction
test.direction <- rmweekly$Direction

#knn1
knn.pred <- knn(train.x, test.x, train.direction, k = 1)

print('')
print('KNN 1')
knntable1 <- confusion(test.direction, knn.pred)


#knn5
knn.pred <- knn(train.x, test.x, train.direction, k = 5)

print('')
print('KNN 5')
knntable5 <- confusion(test.direction, knn.pred)

#LDA and logistic regression seems to be the best method for classifying this data set as they provide the highest accuracy, lowest error rate
#and highest power while also having a fairly high precision rating as well. The only method that beats these two in precision is QDA, but 
#QDA simply predicted everything as up which is fairly unrealistic.


##############
##QUESTION 2##
##############
print('')
print('AUTO')
set.seed(5072)

mpg01 <- rep(0, nrow(Auto))
mpg01[which(Auto$mpg > median(Auto$mpg))] <- 1
mpgAuto <- cbind(Auto, mpg01)

train <- .8
test <- .2

train.idx <- sample(nrow(Auto), train * nrow(Auto))
test.idx <- setdiff(1:nrow(Auto), train.idx)

train.set <- mpgAuto[train.idx,]
test.set <- mpgAuto[test.idx,]

#Logistic Regression
auto.mod <- glm(mpg01 ~ cylinders + weight + displacement, family = binomial, data = train.set)
auto.mod.pred <- predict(auto.mod, test.set, type = 'response')
auto.mod.class <- rep(0, length(auto.mod.pred))
auto.mod.class[auto.mod.pred > .5] <- 1

print('')
print('Auto: Logistic Regression')
auto.log.table <- confusion(test.set$mpg01, auto.mod.class)

#LDA
auto.mod <- lda(mpg01 ~ cylinders + weight + displacement, data = train.set)
auto.mod.pred <- predict(auto.mod, test.set)
auto.mod.class <- auto.mod.pred$class

print('')
print('Auto: LDA')
auto.lda.table <- confusion(test.set$mpg01, auto.mod.class)

#QDA
auto.mod <- qda(mpg01 ~ cylinders + weight + displacement, data = train.set)
auto.mod.pred <- predict(auto.mod, test.set)
auto.mod.class <- auto.mod.pred$class

print('')
print('Auto: QDA')
auto.qda.table <- confusion(test.set$mpg01, auto.mod.class)

#knn
train.x <- data.frame(train.set$cylinders, train.set$weight, train.set$displacement)
test.x <- data.frame(test.set$cylinders, test.set$weight, test.set$displacement)
train.mpg01 <- train.set$mpg01
test.mpg01 <- test.set$mpg01

#knn = 1
knn.pred <- knn(train.x, test.x, train.mpg01, k = 1)

print('')
print('Auto: KNN 1')
knn1.table <- confusion(test.mpg01, knn.pred)

#knn = selection
i = 10
errors <- rep(0, i)

for (p in 1:length(errors)) {
  train.knn <- knn(train.x, test.x, train.mpg01, k = p)
  errors[p] <- mean(test.mpg01 != train.knn)
}

besterror <- errors[which.min(errors)]
bestk <- which.min(errors)

knn.pred <- knn(train.x, test.x, train.mpg01, k = bestk)

print('')
print(paste('Auto: Best K =', bestk))
knn.table <- confusion(test.mpg01, knn.pred)

#For this data set, QDA provides the best results as it leads in accuracy, error rates, power, and precision with power and type 2 error rate 
#being only slightly behind KNN = 2

##############
##QUESTION 3##
##############
print('')
print('BOSTON')
set.seed(5072)

bost <- rep(0, nrow(Boston))
bost[which(Boston$crim > median(Boston$crim))] <- 1
bost.mod <- cbind(Boston, bost)

train <- .8
test <- .2

train.idx <- sample(nrow(Boston), train * nrow(Boston))
test.idx <- setdiff(1:nrow(Boston), train.idx)

train.set <- bost.mod[train.idx,]
test.set <- bost.mod[test.idx,]


#Boston: Logistic Regression
print('')
print('Boston: Logistic Regression')
boston.mod <- glm(bost ~ nox + rad + dis, family = binomial, data = train.set)
boston.pred <- predict(boston.mod, test.set, type = 'response')
boston.class <- rep(0, length(boston.pred))
boston.class[boston.pred > .5] <- 1

boston.log.table <- confusion(test.set$bost, boston.class)

#Boston: LDA
print('')
print('Boston: LDA')
boston.mod <- lda(bost ~ nox + rad + dis, data = train.set)
boston.pred <- predict(boston.mod, test.set)
boston.class <- boston.pred$class

boston.lda.table <- confusion(test.set$bost, boston.class)

#Boston: KNN
train.x <- data.frame(train.set$nox, train.set$rad, train.set$dis)
test.x <- data.frame(test.set$nox, test.set$rad, test.set$dis)
train.bost <- train.set$bost
test.bost <- test.set$bost

print('')
print('Boston: KNN')

i = 50
errors <- rep(0, i)

for (p in 1:length(errors)) {
  bost.knn <- knn(train.x, test.x, train.bost, k = p)
  errors[p] <- mean(test.bost != bost.knn)
}

besterror <- errors[which.min(errors)]
bestk <- which.min(errors)

bost.knn <- knn(train.x, test.x, train.bost, k = bestk)

bost.knn.table <- confusion(test.set$bost, bost.knn)

#In terms of overall performance, KNN has performed the best. However, the best test error rate using KNN resulted from k = 1 so it may not be
#reliable when including additional data. All this means is that a k of 1 fit the test set data well.
#Following KNN in performance is LDA which was fairly close to logistic regression in addition to having some tradeoffs between the two in the
#sense that logistic regression was more precise, but LDA resulted in a higher power rating. In terms of accuracy and error rate, logistic
#regression and LDA were identical, but LDA having a higher type 1 error while logistic regression had a higher type 2 error rate. Additionally,
#the high type 1 errors in both logistic regression and LDA suggest that the dataset might not have a linear relationship and that the use of
#LDA and logistic regression may result in too high of a bias.

##############
##QUESTION 4##
##############
print('')
set.seed(5072)

x = rnorm(100)
y = x - (2*(x^2)) + rnorm(100)

cv.df <- data.frame(x, y)
plot(x,y)

set.seed(123)
print('Seed: 123')

#Linear
lin.fit <- glm(y ~ x, data = cv.df)
cv.lin <- cv.glm(cv.df, lin.fit)
print(paste('Linear LOOCV Errors:', cv.lin$delta[1]))

#Quadratic
quad.fit <- glm(y ~ poly(x, 2), data = cv.df)
cv.quad <- cv.glm(cv.df, quad.fit)
print(paste('Quad LOOCV Errors:', cv.quad$delta[1]))

#Cubic
cub.fit <- glm(y ~ poly(x, 3), data = cv.df)
cv.cub <- cv.glm(cv.df, cub.fit)
print(paste('Cubic LOOCV Errors:', cv.cub$delta[1]))

#Quartic
quart.fit <- glm(y ~ poly(x, 4), data = cv.df)
cv.quart <- cv.glm(cv.df, quart.fit)
print(paste('Quartic LOOCV Errors:', cv.quart$delta[1]))

set.seed(456)
print('')
print('Seed:456')
#Linear
lin.fit <- glm(y ~ x, data = cv.df)
cv.lin <- cv.glm(cv.df, lin.fit)
print(paste('Linear LOOCV Errors:', cv.lin$delta[1]))

#Quadratic
quad.fit <- glm(y ~ poly(x, 2), data = cv.df)
cv.quad <- cv.glm(cv.df, quad.fit)
print(paste('Quad LOOCV Errors:', cv.quad$delta[1]))

#Cubic
cub.fit <- glm(y ~ poly(x, 3), data = cv.df)
cv.cub <- cv.glm(cv.df, quad.fit)
print(paste('Cubic LOOCV Errors:', cv.cub$delta[1]))

#Quartic
quart.fit <- glm(y ~ poly(x, 4), data = cv.df)
cv.quart <- cv.glm(cv.df, quart.fit)
print(paste('Quartic LOOCV Errors:', cv.quart$delta[1]))

print(summary(quart.fit))
#The results are the same as LOOCV returns the same MSE in every situation since in every case, our fit will go through every single observation 
#one at a time, leading to the same average MSE in every case regardless of whichever observation we started from first.

#The quadratic and cubic models had the lowest LOOCV errors which is in line with what was expected due to the concave distribution of points,
#so naturally, a linear fit wouldn't provide the best estimates of the points

#In all cases, the 1st and 2nd order polynomial terms are the only significant terms in the model, which does agree with how the LOOCV MSE
#drops significantly after the linear model and rises as more unnecessary terms are added as in the case of the quartic model