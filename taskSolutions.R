# taskSolutions.R
#
# Code for (some of) the workshop tasks


# Example: fit vs. residuals for a non-linear dataset
x<-runif(50,-1,1)
r <- 0.9
y <- (r * exp(5*x)) + ((1-r) * rnorm(50)); plot(x,y); cor(x,y)
lm(y ~ x)
abline(lm(y ~ x), col="skyblue")
res <- resid(lm(y ~ x))

# calculate idealized values
fit <- fitted(lm(y ~ x))

# plot differences
segments(x, y, x, fit, col="#AA000044")

# plot and analyze residuals
plot(fit, res)
cor(fit, res)


# TASK:
# calculate the coefficient of correlation between Mo.ctrl and MF.ctrl and plot
# the values as a scatterplot
x <- LPSdat$Mo.ctrl
y <- LPSdat$MF.ctrl
cor(x, y)
plot(x, y)
abline(0, 1, col="skyblue")
lm(y ~ x)
abline(lm(y ~ x), col="red")
