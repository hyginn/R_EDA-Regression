# R_EDA-Regression.R
# Purpose:
#     Introduction to regression analysis in biological data with R
#
# Version: 1.0
#
# Date:    2016  06  01
# Author:  Boris Steipe (boris.steipe@utoronto.ca)
#          Some prior contributions by:
#            Raphael Gottardo, FHCRC
#            Sohrab Shah, UBC
# V 1.0    First code
#
# TODO:
#
#
# == HOW TO WORK WITH THIS FILE ======================================
#
#  Go through this script line by line to read and understand the
#  code. Execute code by typing <cmd><enter>. When nothing is
#  selected, that will execute the current line and move the cursor to
#  the next line. You can also select more than one line, e.g. to
#  execute a block of code, or less than one line, e.g. to execute
#  only the core of a nested expression.
#
#  Edit code, as required, experiment with options, or just play.
#  Especially play.
#
#  DO NOT simply source() this whole file!
#
#  If there are portions you don't understand, use R's help system,
#  Google for an answer, or ask me. Don't continue if you don't
#  understand what's going on. That's not how it works ...
#
#  This is YOUR file. Write your notes in here and add as many
#  comments as you need to understand what's going on here when you
#  come back to it in a year. That's the right way: keep code, data
#  and notes all in one place.
#
# ====================================================================
#
# Module 2: Regression
#
# ====================================================================


# =============================================
# Scenario
# =============================================



# =============================================
# Synthetic data: linear model
# =============================================

# This synthetic sample generates observations that
# could come from measuring height and weight of
# a human cohort.

# We generate random heights in an interval, then
# calculate hypothetical weights according to a simple
# linear equation. Finally we add "errors" to the
# weights.

# The goal of our analysis is to recover the parameters
# of our synthetic data.

synthHWsamples <- function(n) {
    set.seed(83)
    # parameters for a height vs. weight plot
    hmin <- 1.5
    hmax <- 2.3
    M <- matrix(nrow=n,ncol=2)
    # generate a column of numbers in the interval
    M[,1] <- runif(n, hmin, hmax)
    # generate a column of numbers with a linear model
    M[,2] <- 40 * M[,1] + 1
    # add some errors
    M[,2] <- M[,2] + rnorm(n, 0, 15)
    return(M)
}

HW<-synthHWsamples(50) # generate 50 proband's heights and weights
plot(HW, xlab="Height (m)", ylab="Weight (kg)") # plot with labels

cor(HW[,1], HW[,2]) # calculate correlation

# =============================================
# What does a correlation coefficient "mean"?

x<-rnorm(50) # 50 random deviates from a N(0,1)

# y values: a fraction is y<-x and a fraction is random
r <- 0.99; y <- (r * x) + ((1-r) * rnorm(50)); plot(x,y); cor(x,y)
r <- 0.8;  y <- (r * x) + ((1-r) * rnorm(50)); plot(x,y); cor(x,y)
r <- 0.4;  y <- (r * x) + ((1-r) * rnorm(50)); plot(x,y); cor(x,y)
r <- 0.01; y <- (r * x) + ((1-r) * rnorm(50)); plot(x,y); cor(x,y)

# What if the relation is not linear?
x<-runif(50,-1,1)
r <- 0.9

y <- (r * x) + ((1-r) * rnorm(50)); plot(x,y); cor(x,y)


# periodic ...
y <- (r * cos(x*pi)) + ((1-r) * rnorm(50)); plot(x,y); cor(x,y)
# polynomial ...
y <- (r * x*x) + ((1-r) * rnorm(50)); plot(x,y); cor(x,y)
# exponential
y <- (r * exp(5*x)) + ((1-r) * rnorm(50)); plot(x,y); cor(x,y)

# circular ...
a <- (r * cos(x*pi)) + ((1-r) * rnorm(50))
b <- (r * sin(x*pi)) + ((1-r) * rnorm(50))
plot(a,b); cor(a,b)


# =============================================
# After becoming more familiar with correlations,
# we return to analyzing our data.

# R provides lm() for regression analysis:
# Remember: the true parameters were weight = 40 * height + 1
?lm
lm(HW[,2] ~ HW[,1])

# What are these numbers?
# How do they relate to our question?
# Is the estimate any good?

# replot the data
plot(HW, xlab="Height (m)", ylab="Weight (kg)") # plot with labels

# plot a regression line
abline(lm(HW[,2] ~ HW[,1]), col="firebrick", lwd=3)

# calculate residuals
res <- resid(lm(HW[,2] ~ HW[,1]))

# calculate idealized values
fit <- fitted(lm(HW[,2] ~ HW[,1]))

# plot differences
segments(HW[,1], HW[,2], HW[,1], fit, col="#AA000044")

# plot and analyze residuals
plot(fit, res)
cor(fit, res)

# Calculate and plot prediction and confidence limits.
# PREDICTION limits give boundaries on future observations,
# they characterize how well the model is expected to
# accommodate new data.
#
# CONFIDENCE limits give boundaries on adequate models.
# They characterize how well we can trust our model
# parameters.

pp<-predict(lm(HW[,2] ~ HW[,1]), int="p")
pc<-predict(lm(HW[,2] ~ HW[,1]), int="c")
head(pc)

# Now plot pp and pc limits
# first sort on x
o <- order(HW[,1]) # o is an index vector, sorted on x-values
HW2 <- HW[o,]

# second, recompute pp, pc in sorted order
pc<-predict(lm(HW2[,2] ~ HW2[,1]), int="c")
pp<-predict(lm(HW2[,2] ~ HW2[,1]), int="p")

# Then plot
plot(HW2, xlab="Height (m)", ylab="Weight (kg)", ylim=range(HW2[,2], pp))
matlines(HW2[,1], pc, lty=c(1,2,2), col="slategrey")
matlines(HW2[,1], pp, lty=c(1,3,3), col="firebrick")

# =============================================
# Non linear least-squares fit
# =============================================

# Here is an example for a non-linear least-squares fit
# to a set of observations, using a formula that we
# define ourselves.

# We simulate the risk of contracting a disease at
# a certain age according to a logistic function
# centred on 50 years and spread in onset approximately
# between 0 and 100 years.

# =============================================
# Synthetic data: arbitrary model
# =============================================

# We employ a strategy that can be used to generate samples
# for any arbitrary target distribution. To achieve this
# we generate a random, uniformly distributed variate x
# in an interval in which we are interested. For each
# variate, we calculate the corresponding function value f(x). Then
# we "roll dice" whether to accept or reject the first variate:
# We generate a second uniform variate z in the range of the
# target function in our interval. If f(x) is smaller than z, we
# accept x as a sample in our distribution.

# Consider this code:

ageOfOnset <- function (n) {
    X <- c()
    i <- 1
    while (i < n) {
        age<-floor(runif(1,1,100))
        s<-runif(1)	# sample uniform between 0 and 1
        # i.e. in the range of the function
        if (s < 1-(1/(1+exp(0.1*(age-50))))) {
            # if s is smaller than the function value ...
            X <- append(X, age) # ... add this age to the vector
            i<-i+1 # ... and increment i.
        }
        # Else, try again until n successful attempts.
    }
    return(X)
}

fx <- function(x) {1-(1/(1+exp(0.1*(x))))}
curve(fx, xlim=c(-50,50), col="red", lwd="2")

ages <- ageOfOnset(10000)
head(ages, 20)
myCounts <- tabulate(ages)
head(myCounts)
plot(myCounts)

# How can we recover the parameters 0.1 and 50 ...
#   1-(1/(1+exp(0.1*(age-50)))))
#               ^^^      ^^
# ... from the dataset?
# Non-linear least-squares fit of a self-defined function!
fz <- function(t, S, tm, B) { S*(1-(1/(1+exp(B*(t-tm))))) }
# here:
#   t is the time
#   S is a scaling factor
#   tm is the mean age of onset
#   B is the risk

# Try some "reasonable" starting parameters, otherwise the
# curve-fit might not converge.

curve(fz(x, S=180, tm=48, B=0.2), add=TRUE, col="slategrey", lwd=1, lty=2)

# now use nls() to fit the formula against our data

age <- c(1:99)
nlsFz <- nls(myCounts ~ fz(age, S, med, B), start=c(S=120, med=30, B=0.9))

nlsFz

summary(nlsFz)
coef(nlsFz)

p <- coef(nlsFz)

# plot the curve with the fitted parameters
curve(fz(x, S=p[1], tm=p[2], B=p[3]), add=TRUE, col="firebrick", lwd=2)

# Let us explore the set of curves that are compatible with the solution.
# For this, we calculate a number of curves with randomly modified parameters
# and plot them with semitransparent lines.

sdev <- confint(nlsFz)
sdev

# calculate the mean and standard deviation for all three parameters
a <- p - sdev[,1]
b <- sdev[,2] - p
a; b
sdev <- (a+b)/2

plot(myCounts)
n <- 5
for (i in 1:n) {
    rS <- rnorm(1, mean=p[1], sd=sdev[1])
    rT <- rnorm(1, mean=p[2], sd=sdev[2])
    rB <- rnorm(1, mean=p[3], sd=sdev[3])
    curve(fz(x, S=rS, tm=rT, B=rB), add=TRUE,
          col=cm.colors(n)[i], lwd=3)
}


# do this many times and plot the curve with a thin, blue line
plot(myCounts, type="n")
n <- 800
for (i in 1:n) {
    rS <- rnorm(1, mean=p[1], sd=sdev[1])
    rT <- rnorm(1, mean=p[2], sd=sdev[2])
    rB <- rnorm(1, mean=p[3], sd=sdev[3])
    curve(fz(x, S=rS, tm=rT, B=rB), add=TRUE,
          col="#77BEFB", lwd=0.03)
}
points(myCounts)



# For a discussion of how to apply a weighting function to the weights =
# parameter of nls(), including a good use of deparse() and sys.calls(),
# see: http://www.r-bloggers.com/a-weighting-function-for-nls-nlslm/



# =============================================
# Alternatives to Pearson correlation - the MIC
# =============================================

# The Maximal Information Coefficient is implemented
# in the R package Minerva. Let's try it out with
# an example.

# We consider which cell types in the LPSdat data are most highly correlated
# regarding their expression differences.
#
# I have saved the LPSdat object from our previous session with the following
# command:
#    save(LPSdat, file="LPSdat.RData")
# You can conveniently recreate it:
load(file="LPSdat.RData")

# Note that you don't need to assign the contents of the file - the R objects
# are recreated into exactly the same name and state they had when you saved
# them. Nb. you can use the save() command to save more than one object. The
# procedure is efficient, since the .RData filesare compressed binary files.


# Let's first look at one cell pair: B-cells and
# macrophages.

dB  <- LPSdat[ , "B.ctrl"] - LPSdat[ , "B.LPS"]
dMF <- LPSdat[ ,"MF.ctrl"] - LPSdat[ ,"MF.LPS"]

plot(dB, dMF)
cor(dB, dMF)      # 0.461983

# That seems like a very significant correlation. What
# value could we expect to observe by chance? To evaluate
# that, we can shuffle the data and compile the distribution
# of correlation coefficient from random comparison of
# genes:

L <- length(dMF)

R_results <- numeric(10000)
for (i in 1:10000) {
    R_results[i] <- cor(dB, dMF[sample(1:L)])
}
hist(R_results, xlim = c(-0.6, 0.6))
abline(v = cor(dB, dMF), col = "firebrick")

# Let's try the same thing with the MIC:

if (!require(minerva, quietly = TRUE)) {
    install.packages("minerva")
    library(minerva)
}

?mine

mine(dB, dMF)  # 0.1468241

# This will take MUCH longer, thus run for 1000
# trials only...
MIC_results <- numeric(1000)
for (i in 1:1000) {
    MIC_results[i] <- mine(dB, dMF[sample(1:L)])$MIC
}
hist(MIC_results, xlim = c(0.0, 0.2))
abline(v = mine(dB, dMF)$MIC, col = "turquoise")


# Try to calculate R and MIC for all pairs...
# The strategy is as follows:
# 1 - create a dataframe that contains each cell type's
#     differential expression values for all genes.
# 2 - calculate correlations for all unique pairs and
#     save the values
# 3 - calculate MIC for same
# 4 - evaluate correlations.


# Which two cell types are the most highly correlated?

# You can also calculate correlations for genes -
# Which genes are the most similar in their response?


# [End]
