rm(list = ls())

library(boot)


compfunc <- function(x, d){
  x <- x[d]
  y <- -1 + (.5 * x) + eps 
  fit <- lm(y ~ x)$coef
  return(fit)
}

bootmean <- function(x, d) {
  return(mean(x[d]))
}

###############DATA
x <- rnorm(100, 5, 1)
eps <- rnorm(100, 5, sqrt(0.25))

y <- -1 + (.5 * x) + eps

#######LINEAR REGRESSION
lin.fit <- lm(y ~ x)

print(confint(lin.fit, level = .95))


#######BOOTSTRAP
k <- boot(x, compfunc, R = 1000)

print(boot.ci(k, type = 'bca', index = 2))