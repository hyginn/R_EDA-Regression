# sample-solution-RegressionPlot.R
#
# Regression plot with confidence and prediction intervals
# of cell-line ctrl and LPS expression enrichment values.
# ====================================================================

load("../R_EDA-Introduction/LPSdat.RData")

HW <- matrix(nrow = nrow(LPSdat), ncol = 2) # make a matrix
HW[ ,1] <- LPSdat$MF.ctrl                   # extract the data into
HW[ ,2] <- LPSdat$MF.LPS                    #   the matrix

# Now plot pp and pc limits
# 1: sort on values in the first column
o <- order(HW[,1]) # o is an index vector
HW2 <- HW[o,]

# 2: compute pp, pc in sorted order
pc<-predict(lm(HW2[,2] ~ HW2[,1]), int="c")
pp<-predict(lm(HW2[,2] ~ HW2[,1]), int="p")

# 3: plot the points
plot(HW2,
     col=densCols(HW2[, 1], HW2[, 2]),
     xlab = "MF.ctrl",
     ylab = "MF.LPS",
     ylim=range(HW2[,2], pp),
     pch=16,
     cex=1)

# 4: add the lines of the confidence limits
matlines(HW2[,1], pc, lty=c(1,2,2), col="slategrey")
matlines(HW2[,1], pp, lty=c(1,3,3), col="firebrick")




# [END]

