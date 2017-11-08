rm(list = ls())


##############
##QUESTION 1##
##############
set.seed(5072)

x <- rnorm(100, 0, 1)
eps <- rnorm(100, 0, sqrt(0.25))

y <- -1 + (.5 * x) + eps
print(paste('Length:', length(y)))
#B0 is -1 and B1 is .5

plot(x,y)

#Largely positive linear relationship with a fairly wide variance

lslm <- lm(y ~ x)
#print(coef(lslm))

#B0hat is -1.003 and B1hat is .435
#it is fairly close to the population B0 and B1

abline(lslm)
abline(a = -1, b = .5, col = 'red')

legend('bottomright', legend = c('Least Squares', 'Population'), col = c('black', 'red'), lty = 1)

sq <- x^2
poly <- lm(y ~ x + sq)

#print(coef(poly))
#print('eps .25')
#print(anova(poly))
#the squared term does not significantly improve the model as it has neglible impact on the y value for every unit change in the squared term
#in essence, all the squared term does is decrease the R^2 accuracy

#eps .1
eps.1 <- rnorm(100, 0, sqrt(0.1))
y.1 <- -1 + (.5 * x) + eps.1
plot(x,y.1)
lslm.1 <- lm(y.1 ~ x)
abline(lslm.1)
abline(a = -1, b = .5, col = 'red')
sq <- x^2
poly.1 <- lm(y.1 ~ x + sq)
#print('eps .1')
#print(anova(poly.1))

#eps .5
eps.5 <- rnorm(100, 0, sqrt(0.5))
y.5 <- -1 + (.5 * x) + eps.5
plot(x,y.5)
lslm.5 <- lm(y.5 ~ x)
abline(lslm.5)
abline(a = -1, b = .5, col = 'red')
sq <- x^2
poly.5 <- lm(y.5 ~ x + sq)
#print('eps .5')
#print(anova(poly.5))

#Naturally, as variance increases, we see the least squares regression line deviate from the population regression line. As such, it's clear that the
#best fit line is the one where variance is lowest, in our case the dataset with variance of 0.1, followed by 0.25, then 0.5.

orig <- confint(lslm, level = .95)
noisy <- confint(lslm.1, level = .95)
mostnoise <- confint(lslm.5, level = .95)
#print(orig)
#print(noisy)
#print(mostnoise)

#Due to the increasing variance levels from the original to the noisest data set, the confidence interval for the predictor becomes increasingly wide
#as the standard deviation of each predictor increases, skewed by observations averaging further from the mean.
#Additionally, as the variance increases, values that may have been significant may become insignificant, suggesting further implications of the
#effects of variance.

##############
##QUESTION 2##
##############

set.seed(5072)
x1 = runif(100) #random uniform distribution
x2 = (.5 * x1) + (rnorm(100)/10)
y = 2 + (2 * x1) + (0.3 * x2) + rnorm(100)
#Parameter B0 is 2 + rnorm(100), parameter B1 is 2, parameter B2 is .3

print(paste('Cor(x1, y) = ', cor(x1, y)))
print(paste('Cor(x2, y) = ', cor(x2, y)))
print(paste('Cor(x1, x2) = ', cor(x1, x2)))

pairs(~y + x1 + x2)
#It seems that x1 and x2 have a clear positive correlation with each other while neither x1 nor x2 fully explain y by themselves.
#However, there still remains a somewhat positive correlation between x1/x2 and y though variances seem to be large for every point of x1/x2.

lm.fit.both <- lm(y ~ x1 + x2)
print(coef(lm.fit.both))
#According to statistical t-testing on the intercept and the variables x1/x2, B0hat and B1hat are highly significant factors in predicting y.
#On the other hand, B2hat is not significant according to the t-test in predicting y. One noticeable observation about B2hat is the very wide
#variance in comparison to the B0hat and B1hat, suggesting that B2hat may be a biased predictor for y.

#According to T-testing and F-testing the variables, we cannot conclusively reject B1hat as a significant predictor. However, we can reject B2hat
#as a significant predictor due to the low T and F values, suggesting that B2hat is not meaningful in predicting y, especially since width of 
#B2hat's interval is wide, making it unlikely that B2hat is statistically different from the expected mean of 0.

lm.fit.x1 <- lm(y ~ x1)
#We cannot reject B1hat(x1) as T-testing and F-testing indicate that the predictor is highly significant in predicting y.

lm.fit.x2 <- lm(y ~ x2)
#We cannot reject B1hat(x2) as T-testing and F-testing indicate that the predictor is highly significant in predicting y.

#The results in lm.fit.x2 does contradict our earlier results. The reason for why this happened is possibly due to x1 and x2 being signficantly
#correlated with one another and as a result, since x1 already explained most of y, the inclusion of x2 was not as significant since x1 and x2
#explain the same level of y.
#For similar reasons, if we used x2 as the initial predictor for y, x1 would likely have been insignificant in predicting y.

x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)

lm.fit.both2 <- lm(y ~ x1 + x2)

lm.fit.x12 <- lm(y ~ x1)

lm.fit.x22 <- lm(y ~ x2)

#For the overall model including x1 and x2, the inclusion of this new observation has reduced the significance of x1 while having increased the
#significance of x2. This had the effect of bringing the relevance of x1 into near non-signficant status for a 95% confidence interval 
#though F-testing still indicates that x1 is still highly relevant.
#Individually, both x1 and x2 still remain highly significant with B1hat weights comparable to the weights prior to the new observation.

par(mfrow = c(2,2))
plot(lm.fit.both2)
plot(lm.fit.x12)
plot(lm.fit.x22)

#For the overall model, the 101st observation isn't necessarily an outlier as it's residuals lie somewhat closely to the other observed responses.
#Additionally, the spread of the residuals seems fairly even and random across the fitted values.
#However, it is a highly leveraged point relative to other x values as it is beyond the Cook's distance line, suggesting that it is a fairly
#influential point. This is possibly due to the combination of the 101st x1 and x2 values being significantly far from all other combinations.

#In the case where only x1 is considered, while its residuals are higher than the other observations, it's still close enough that it possibly
#may not be an outlier. Similarly to the overall model, the residuals seems randomly spread out across all the fitted values along with its
#x value being well within Cook's distance, so x1's 101st observation is neither an outlier nor a high leverage point.

#In the case where only x2 is considered, the inclusion of the 101st observation is neither an outlier nor a high leverage point as it is well
#within the range of residuals shown by all fitted values. It is also well within Cook's distance indicating that x value of the 101st is not a
#high leverage point.

par(mfrow = c(1,1))

##############
##QUESTION 3##
##############
set.seed(5072)
library(MASS)

BostonTable <- matrix(0, nrow = 13, ncol = 4)
colnames(BostonTable) <- c('Predictor', 'F-Stat', 'P-Value', 'Coef')
columns <- colnames(Boston)
for (i in 2:length(columns)) {
  BostonTable[i-1,1] = columns[i]
  
  form <- paste('crim ~', columns[i])
  Bostonfit <- lm(as.formula(form), data = Boston)
  BostonTable[i-1,2] = as.numeric(summary(Bostonfit)$fstatistic[1])
  BostonTable[i-1,3] = as.numeric(anova(Bostonfit)$'Pr(>F)'[1])
  BostonTable[i-1,4] = as.numeric(coef(Bostonfit)[2])
}

BostonTable <- as.data.frame(BostonTable, stringsAsFactors = FALSE)
print(BostonTable)
sigpred <- BostonTable[as.numeric(BostonTable$`P-Value`) < .05,][,1]
#sigpred <- paste(sigpred, collapse = ', ')
#print(paste('Significant predictors:', sigpred))
#The significant predictors are: zn, indus, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv

par(mfrow=c(3, 4))
for (i in 1:length(sigpred)) {
  bostlab <- paste('Boston$', sigpred[i], sep = '')
  plot(x = eval(parse(text = bostlab)), y = Boston$crim, xlab = 'x', ylab = 'Boston$crim', ylim = c(0,80), main = bostlab)
  form <- paste('crim ~', sigpred[i])
  Bostonfit <- lm(as.formula(form), data = Boston)
  abline(Bostonfit, col = 'red')
}
par(mfrow = c(1,1))

sigpredmlm <- paste(sigpred, collapse = ' + ')
multilm <- paste('crim ~ ', sigpredmlm)
mulboston <- lm(as.formula(multilm), data = Boston)

sigmlm <- as.data.frame(coef(summary(mulboston)))
print(sigmlm[sigmlm$'Pr(>|t|)' < .05,])
sigmlmint <- sigmlm[-1,]
BostonTableint <- BostonTable[-1,]
plot(BostonTableint[,4], sigmlmint[,1], xlab = 'Simple', ylab = 'Multi')
#Both the multiple regression and simple regression approaches show agreement on the overpowering influence of one predictor. However, 
#multiple regression would still be the more appropriate approach as the method would take into effect the influence of other predictors
#on the output and thus provide us with predictors that more accurately reflects the relationships between population parameters

nonlin <- matrix(0, nrow = 13, ncol = 3)
colnames(nonlin) <- c('predictor', 'fstat', 'pvalueofFstat')
for (i in 2:length(columns)) {
  nonlin[i-1,1] = columns[i]
  
  polyform <- paste(c('crim ~ ', columns[i], ' + ', 'I(' ,columns[i], '^2)', ' + ', 'I(', columns[i], '^3)'), collapse = '')
  form <- paste(c('crim ~ ', columns[i]), collapse = '')
  fit <- lm(as.formula(form), data = Boston)
  polyfit <- lm(as.formula(polyform), data = Boston)
  nonlin[i-1,2] <- anova(polyfit, fit)$F[2]
  nonlin[i-1,3] <- anova(polyfit, fit)$`Pr(>F)`[2]
}

nonlin <- nonlin[!is.na(nonlin[,2]),]
nonlin <- as.data.frame(nonlin, stringsAsFactors = FALSE)
nonlin[,2] <- as.numeric(nonlin[,2])
nonlin[,3] <- as.numeric(nonlin[,3])
nonlin <- nonlin[order(nonlin[,2], decreasing = TRUE),]
#At the .05 level, black is the only predictor that does not have a significant predictor between the two models. As a result, all the other
#predictors seem to have a non-linear association with the response.