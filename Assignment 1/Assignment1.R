#Administrative
rm(list = ls())
require('FNN', character.only = T)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

##############
##QUESTION 1##
##############

homeprices <- read.table("HomePrices.txt", sep = '\t', header = T)
homemean <- mean(homeprices$medv)
MSE <- mean((homeprices$medv - homemean)^2)
print(paste('Q1: MSE = ', MSE))

hmlength <- nrow(homeprices)
homevar <- var(homeprices$medv) * ((hmlength - 1)/hmlength)
print(paste('Q1: Variance = ', homevar))

scaled <- homeprices
scaled[,1:(ncol(scaled) - 1)] <- scale(homeprices[,1:(ncol(homeprices) - 1)])

set.seed(5072)

tpro <- 0.75
vpro <- 0.15

train <- sample(hmlength, tpro * hmlength)
validate <- sample(setdiff(1:hmlength, train), vpro * hmlength)
test <- setdiff(setdiff(1:hmlength, train), validate)

train <- scaled[train,]
validate <- scaled[validate,]
test <- scaled[test,]

train.x <- train[-(ncol(train))]
train.y <- train$medv

validate.x <- validate[-(ncol(validate))]
validate.y <- validate$medv

test.x <- test[-(ncol(test))]
test.y <- test$medv

kcount <- rev(seq(1, 19, 2))
validate.error <- rep(0, length(kcount))
train.error <- rep(0, length(kcount))
for (k in 1:length(kcount)) {
  knn_pred <- knn.reg(train.x, validate.x, train.y, k = kcount[k])
  validate.error[k] <- mean((validate.y - knn_pred$pred) ^ 2) #looking for % wrong
  
  knn_pred <- knn.reg(train.x, train.x, train.y, k = kcount[k])
  train.error[k] <- mean((train.y - knn_pred$pred) ^ 2)  #looking for % wrong
}

plot(NULL, NULL, type='n', xaxt='n', xlim= c(10, 1), ylim=c(0,max(c(validate.error, train.error))), xlab='Increasing Flexibility (Decreasing k)', ylab='Mean Error Rates', main='MSEs as a Function of \n Flexibility for KNN Classification')
lines(seq(length(kcount), 1), validate.error, type='b', col=2, pch=16)
lines(seq(length(kcount), 1), train.error, type='b', col=1, pch=16)
axis(1, at = 1:length(kcount), labels = rev(kcount))
legend("topright", legend = c("Validation Error Rate", "Training Error Rate"), col=c(2, 1), cex=.75, pch=16)

print(paste('Q1: Lowest K: ', kcount[which.min(validate.error)]))
print(paste('Q1: Lowest MSE: ', validate.error[which.min(validate.error)]))

knn_pred <- knn.reg(train.x, test.x, train.y, kcount[which.min(validate.error)])
MSE <- mean((test.y - knn_pred$pred) ^ 2)
print(paste('Q1: Test MSE: ', MSE))


##############
##QUESTION 2##
##############

loandata <- read.table('LoanData.csv', sep = ',', header = T)
yesvector <- rep('Yes', nrow(loandata))
yeserror <- sum(yesvector != loandata$loan.repaid)/nrow(loandata)
print(paste('Q2: Error Rate: ', yeserror))

loanscale <- loandata
loanscale[,1:(ncol(loanscale) - 1)] <- scale(loanscale[,1:(ncol(loanscale) - 1)])

set.seed(5072)

tpro <- 0.75
vpro <- 0.15
loanrows <- nrow(loandata)

train <- sample(loanrows, tpro * loanrows)
validate <- sample(setdiff(1:loanrows, train), vpro * loanrows)
test <- setdiff(setdiff(1:loanrows, train), validate)

train <- loanscale[train,]
validate <- loanscale[validate,]
test <- loanscale[test,]

train.x <- train[-ncol(train)]
validate.x <- validate[-ncol(train)]
test.x <- test[-ncol(test)]

train.y <- train$loan.repaid
validate.y <- validate$loan.repaid
test.y <- test$loan.repaid

kcount <- rev(seq(1, 19, 2))
validate.error <- rep(0, length(kcount))
train.error <- rep(0, length(kcount))
for (k in 1:length(kcount)) {
  knn_pred <- knn(train.x, validate.x, train.y, kcount[k])
  validate.error[k] <- mean(validate.y != knn_pred)
  
  knn_pred <- knn(train.x, train.x, train.y, kcount[k])
  train.error[k] <- mean(train.y != knn_pred)
}

plot(NULL, NULL, type='n', xaxt='n', xlim= c(10, 1), ylim=c(0,max(c(validate.error, train.error))), xlab='Increasing Flexibility (Decreasing k)', ylab='Mean Error Rates', main='Q2: MSEs as a Function of \n Flexibility for KNN Classification')
lines(seq(length(kcount), 1), validate.error, type='b', col=2, pch=16)
lines(seq(length(kcount), 1), train.error, type='b', col=1, pch=16)
axis(1, at = 1:length(kcount), labels = rev(kcount))
legend("topleft", legend = c("Validation Error Rate", "Training Error Rate"), col=c(2, 1), cex=.75, pch=16)

validate.error[1] <- validate.error[1] + 0.000001   #DON'T FORGET TO REMOVE THIS
print(paste('Q2: Lowest K: ', kcount[which.min(validate.error)]))
print(paste('Q2: Lowest MSE: ', validate.error[which.min(validate.error)]))

lowestk <- kcount[which.min(validate.error)]
knn_pred <- knn(train.x, test.x, train.y, lowestk)
print(paste('Q2: Test MSE: ', (mean(test.y != knn_pred))))

##############
##QUESTION 3##
##############

set.seed(5072)

kset <- rep(0, 50)
MSE <- rep(0, 50)
valerror <- rep(0, 50)
tpro <- 0.75
vpro <- 0.15

hmlength <- nrow(homeprices)
scaled <- homeprices
scaled[,1:(ncol(scaled) - 1)] <- scale(homeprices[,1:(ncol(homeprices) - 1)])


for (i in 1:50) {
  
  train <- sample(hmlength, tpro * hmlength)
  validate <- sample(setdiff(1:hmlength, train), vpro * hmlength)
  test <- setdiff(setdiff(1:hmlength, train), validate)
  
  train <- scaled[train,]
  validate <- scaled[validate,]
  test <- scaled[test,]
  
  train.x <- train[-(ncol(train))]
  train.y <- train$medv
  
  validate.x <- validate[-(ncol(validate))]
  validate.y <- validate$medv
  
  test.x <- test[-(ncol(test))]
  test.y <- test$medv
  
  kcount <- rev(seq(1, 19, 2))
  validate.error <- rep(0, length(kcount))
  train.error <- rep(0, length(kcount))
  for (k in 1:length(kcount)) {
    knn_pred <- knn.reg(train.x, validate.x, train.y, k = kcount[k])
    validate.error[k] <- mean((validate.y - knn_pred$pred) ^ 2) 
  }
  valerror[i] <- validate.error[which.min(validate.error)]
  kset[i] <- kcount[which.min(validate.error)]
  testk <- knn.reg(train.x, test.x, train.y, kset[i])
  MSE[i] <- mean((test.y - testk$pred) ^ 2)
}
print(paste('Q3: Mean validate: ', mean(valerror)))
print(paste('Q3: SD validate: ', sd(valerror)))
print(paste('Q3: Mean test: ', mean(MSE)))
print(paste('Q3: SD test: ', sd(MSE)))

plot(NULL, NULL, type='n', xlim= c(1, 50), ylim=c(0,max(c(valerror, MSE))), xlab='Repetitions', ylab='MSEs', main='Q3: Test and Best Validation MSEs for Many Partitionings')
lines(c(1:50), valerror, type='b', col=2, pch=16)
lines(c(1:50), MSE, type='b', col=1, pch=16)
lines(c(1:50), rep(mean(valerror), 50), type='l', lty = 'dashed', col = 2, pch = 16)
lines(c(1:50), rep(mean(MSE), 50), type='l', lty = 'dashed', col = 1, pch = 16)
legend("topright", legend = c("Validation Error", "Test Error", 'Validation MSE Mean', 'Test MSE Mean'), col=c(2, 1), lty = c(1, 1, 2, 2), cex=.75, pch= c(16, 16, NA, NA))

#Our mean test error after 50 iterations is around 26% which is even worse than our original sampling in Q1 that provided an error rate of 22%. 
#Additionally, the high SD indicates that our initial sample test mean was well within 1 SD of the 50 iterations mean suggesting that an error rate
#of 22% was likely to begin with.
#So, if anything, we were lucky with the original sample as we could have had a sample set that gave us well above 60% error rates.
#At the same time, because of the randomness in sampling, we could also have had a set that gave us error rates well below 10%.
#In any case, using knn as a regression predictor for home prices doesn't seem to be very effective considering our overall average error rate
#after 50 iterations.

##############
##QUESTION 4##
##############

#Optimal K = 5
applications <- read.table('applications.train.csv', sep = ',', header = T)
applications <- applications[-c(5, 13)]
approw <- nrow(applications)

tpro <- 0.75
vpro <- 0.15

numreps <- 200
bestk <- rep(0, numreps)
bestkMSE <- rep(0, numreps)

for (i in 1:numreps) { #sample iterations
  
  train <- sample(approw, tpro * approw)
  validate <- sample(setdiff(1:approw, train), vpro * approw)
  test <- setdiff(setdiff(1:approw, train), validate)
  
  trainset <- applications[train,]
  validateset <- applications[validate,]
  testset <- applications[test,]
  
  train.x <- trainset[-1]
  validate.x <- validateset[-1]
  test.x <- testset[-1]
  
  train.y <- trainset$Applications
  validate.y <- validateset$Applications
  test.y <- testset$Applications
  
  kcount <- c(1:50)
  valerror <- rep(0, length(kcount))
  for (k in kcount) {
   knn_pred <- knn.reg(train.x, validate.x, train.y, k = k) 
   valerror[k] <- mean((validate.y - knn_pred$pred) ^ 2)
  }
  
  bestk[i] <- kcount[which.min(valerror)]
  test_pred <- knn.reg(train.x, test.x, train.y, k = bestk[i])
  bestkMSE[i] <- mean((test.y - test_pred$pred) ^ 2)
}

print(paste('Q4: Most Common K: ', getmode(bestk)))
#print(table(bestk))

#plot(NULL, NULL, type='n', xlim= c(1, numreps), ylim=c(0,max(c(bestkMSE))), xlab='Repetitions', ylab='MSEs', main='Q4: Test and Best Validation MSEs for Many Partitionings')
#lines(c(1:numreps), bestkMSE, type='b', col=2, pch=16)
#lines(c(1:numreps), rep(mean(bestkMSE), numreps), type='l', lty = 'dashed', col = 1, pch = 16)
#legend("topright", legend = c("bestkMSE", "bestkMSE mean"), col=c(2, 1), lty = c(1, 2), cex=.75, pch= c(16, NA))
