##
## Big Data HW 3
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
## Q1 
## Interpret AICc selected model from my nhlreg lasso
## Just tell some stories about what the model tells you
##

## combine the covariates all together
x <- cBind(config,team,player) # cBind binds together two sparse matrices

## build 'y': home vs away, binary response
y <- goal$homegoal

## run gamma-lasso regression
nhlreg <- gamlr(x, y, 
                free=1:(ncol(config)+ncol(team)), ## free denotes unpenalized columns
                family="binomial", standardize=FALSE)

## coefficients (grab only the players)
## AICc selection 
Baicc <- coef(nhlreg)[colnames(player),]   
Baicc <- sort(Baicc, decreasing=TRUE)

## best and worst
best10 <- head(Baicc,n=10)
best_tab <- (cbind(str_replace(names(best10), "_", " "), 1:10, 
            sprintf(best10, fmt='%#.4f'),
            sprintf(exp(best10), fmt='%#.4f'),
            colSums(abs(player[, match(names(best10), colnames(player))]))))
colnames(best_tab) <- c("Player", "Rank", "$\\beta_j$", "$\\exp(\\beta_j)$", "G")
print(xtable(best_tab, label="tab:best10", caption="Top 10 NHL Players (2002-2014)"),
      sanitize.text.function=function(x){x}, file=GetFilename('best10.tex'))

worst10 <- tail(Baicc,n=10)
n <- length(Baicc)
worst_tab <- (cbind(str_replace(names(worst10), "_", " "), (n-9):n, 
                   sprintf(worst10, fmt='%#.4f'),
                   sprintf(exp(worst10), fmt='%#.4f'),
                   colSums(abs(player[, match(names(worst10), colnames(player))]))))
colnames(worst_tab) <- c("Player", "Rank", "$\\beta_j$", "$\\exp(\\beta_j)$", "G")
print(xtable(worst_tab, label="tab:worst10", caption="Bottom 10 NHL Players (2002-2014)"),
      sanitize.text.function=function(x){x}, file=GetFilename('worst10.tex'))

## vizualize
PlotSetup('player_rtg')
plot(Baicc, main="Player Rating Coefficients",
     xlab="Players [Ranked Best to Worst]", ylab="Rating Coefficient")
PlotDone()

## 
## Q2
## The gamlr run for nhlreg uses standardize=FALSE -- why?
## What happens if you do standardize=TRUE
##

## run gamma-lasso regression with standardize = TRUE
nhlreg_std <- gamlr(x, y, 
                free=1:(ncol(config)+ncol(team)), ## free denotes unpenalized columns
                family="binomial", standardize=TRUE)

## coefficients (grab only the players)
## AICc selection 
Baicc_std <- coef(nhlreg_std)[colnames(player),]   
Baicc_std <- sort(Baicc_std, decreasing=TRUE)

best10 <- head(Baicc_std,n=10)
best_tab <- (cbind(str_replace(names(best10), "_", " "), 1:10, 
                   sprintf(best10, fmt='%#.4f'),
                   sprintf(exp(best10), fmt='%#.4f'),
                   colSums(abs(player[, match(names(best10), colnames(player))]))))
colnames(best_tab) <- c("Player", "Rank", "$\\beta_j$", "$\\exp(\\beta_j)$", "G")
print(xtable(best_tab, label="tab:best10", caption="Top 10 NHL Players (2002-2014)"),
      sanitize.text.function=function(x){x}, file=GetFilename('best10_std.tex'))

worst10 <- tail(Baicc_std,n=10)
worst_tab <- (cbind(str_replace(names(worst10), "_", " "), (n-9):n, 
                    sprintf(worst10, fmt='%#.4f'),
                    sprintf(exp(worst10), fmt='%#.4f'),
                    colSums(abs(player[, match(names(worst10), colnames(player))]))))
colnames(worst_tab) <- c("Player", "Rank", "$\\beta_j$", "$\\exp(\\beta_j)$", "G")
print(xtable(worst_tab, label="tab:worst10", caption="Bottom 10 NHL Players (2002-2014)"),
      sanitize.text.function=function(x){x}, file=GetFilename('worst10_std.tex'))

## plot of lasso to build intuition
## example players: Pavel Datsyuk, Jeff Toms
bet <- seq(-20, 20, 0.1)
pen_pd <- abs(bet)*sqrt(var(player[,"PAVEL_DATSYUK"]))
pen_jt <- abs(bet)*sqrt(var(player[,"JEFF_TOMS"]))

PlotSetup('cost_fcn')
plot(pen_pd ~ bet, type="l", col=1, main="Cost Function with Standardization", 
     xlab="beta", ylab="penalty")
lines(pen_jt ~ bet, col=2)
legend("top", c("Pavel Datsyuk", "Jeff Toms"), lty=c(1,1), col=c(1,2), bty = "n")
PlotDone()