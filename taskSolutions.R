# taskSolutions.R
#
# Code for (some of) the workshop tasks


source("readS3.R")
head(LPSdat)
typeInfo(LPSdat)

# === "Quiz" on subsetting and filtering values
#     in LPSdat

# rows 1:10 of the first two columns in reverse order
LPSdat[10:1, c(1, 2)]

# gene names and the expression values for Mo.LPS
# for the top ten expression values.
# hint: use order() .. like so:
x <- sample(LETTERS, 5)
x
order(x)
x[order(x)]
o <- order(x)
o[1:3]

LPSdat[order(LPSdat$Mo.LPS, decreasing = TRUE)[1:10],
       c("genes", "Mo.LPS" )]


# All genes for which B-cells are stimulated by LPS by
# more than 2 log units.

LPSdat[LPSdat$B.LPS - LPSdat[ , "B.ctrl" ] > 2, "genes"]

# get the means for each cell type
#

summary(LPSdat)


# 1 - What columns of LPSdat could be compared with
#     a qqplot? Explore this. Interpret the result.
#     Compare cell-type against cell-type?
#     Compare ctrl against stimulated?
plot(LPSdat$MF.ctrl, LPSdat$MF.LPS)
abline(0, 1, col="#0055CC")
qqplot(LPSdat$MF.ctrl, LPSdat$MF.LPS)
# The plot shows that ...

abline(0, 1, col="firebrick")
# The comparison with expectation shows that ...
#
