##
# Big Data HW 3
##

rm(list=ls())

source('../utils/source_me.R', chdir = T)
source("../utils/fdr.R")
source("../utils/deviance.R")

plotOpts$Prefix="writeup/"
OutputToFile = T

library(gamlr) # loads Matrix as well
library(xtable)
library(stringr)

# Load data directly from gamlr package
data(hockey)

##
# Q1 
# Interpret AICc selected model from my nhlreg lasso
# Just tell some stories about what the model tells you
##

# Combine the covariates all together
x <- cBind(config,team,player) # cBind binds together two sparse matrices

# build 'y': home vs away, binary response
y <- goal$homegoal

nhlreg <- gamlr(x, y, 
                free=1:(ncol(config)+ncol(team)), ## free denotes unpenalized columns
                family="binomial", standardize=FALSE)

## coefficients (grab only the players)
# AICc selection 
Baicc <- coef(nhlreg)[colnames(player),]   
Baicc <- sort(Baicc, decreasing=TRUE)

# best and worst
print("The 10 best players are:")
best10 <- head(Baicc,n=10)
print(cbind(str_replace(names(best10), "_", " "), "&", 1:10, "&", 
            sprintf(best10, fmt='%#.4f'), "&", 
            sprintf(exp(best10), fmt='%#.4f'), "\\"), quote=FALSE)

n <- length(Baicc)
print("The 10 worst players are:")
worst10 <- tail(Baicc,n=10)
print(cbind(str_replace(names(worst10), "_", " "), "&", (n-9):n, "&", 
            sprintf(worst10, fmt='%#.4f'), "&", 
            sprintf(exp(worst10), fmt='%#.4f'), "\\"), quote=FALSE)

# vizualize
#PlotSetup('player_rtg')
plot(Baicc, main="Player Rating Coefficients",
     xlab="Players [Ranked Best to Worst]", ylab="Rating Coefficient")
#PlotDone()