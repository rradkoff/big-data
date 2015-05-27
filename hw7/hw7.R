##
## housekeeping
##

rm(list = ls())
source('../utils/source_me.R', chdir = T)
OutputToFile = F
plotOpts$Prefix = "output/"


###############################################################################
# Read Data
###############################################################################
sp <- read.csv('sp500.csv')
fx <- read.csv("FXmonthly.csv")

# Convert to a percentage return over the previous month.
fx <- (fx[2:120,]-fx[1:119,])/(fx[1:119,])

heatmap(cor(fx), Rowv = NA, Colv = NA, symm = T)
