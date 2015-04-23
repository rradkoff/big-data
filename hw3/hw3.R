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

##
## Compare model selection methods for the nhlreg lasso.
## Consider both IC and CV (you’ll want to create cv.nhlreg).
##
cv.nhlreg <- cv.gamlr(x, y, 
                      free=1:(ncol(config)+ncol(team)), ## free denotes unpenalized columns
                      family="binomial", verb=T)
PlotSetup('cv_nhl_gamlr_a')
plot(cv.nhlreg)
PlotDone()

PlotSetup('cv_nhl_gamlr_b')
plot(cv.nhlreg$gamlr)
PlotDone()

## log lambdas selected under various criteria
GetICs <- function(reg.gamlr, reg.cv.gamlr) {
  return(data.frame(
    ic = c('AICc', 'AIC', 'BIC', 'CV.Min', 'CV.1se'),
    lambda = c(log(reg.gamlr$lambda[which.min(AICc(reg.gamlr))]),
               log(reg.gamlr$lambda[which.min(AIC(reg.gamlr))]),
               log(reg.gamlr$lambda[which.min(BIC(reg.gamlr))]),
               log(reg.cv.gamlr$lambda.min),
               log(reg.cv.gamlr$lambda.1se)),
    row.names = 'ic'))
}
print(GetICs(nhlreg_std, cv.nhlreg))
ICs <- GetICs(nhlreg_std, cv.nhlreg)
colnames(ICs) <- c('$log(\\lambda)$')
print(xtable(ICs, label="tab:ic", caption="ICs for NHL Data"),
      sanitize.text.function=function(x){x}, file=GetFilename('ic.tex'))

PlotIC <- function(reg.gamlr, reg.cv.gamlr, n, file, ylim=NULL) {
  ll <- log(reg.gamlr$lambda) ## the sequence of lambdas
  
  PlotSetup(paste(file, "_a", sep=''))
  plot(reg.cv.gamlr)
  PlotDone()
  
  PlotSetup(paste(file, "_b", sep=''))
  plot(ll, AIC(reg.gamlr)/n, ylim = ylim,
       xlab="log lambda", ylab="IC/n", pch=19, col="orange")
  abline(v=ll[which.min(AIC(reg.gamlr))], col="orange", lty=3)
  abline(v=ll[which.min(BIC(reg.gamlr))], col="green", lty=3)
  abline(v=ll[which.min(AICc(reg.gamlr))], col="black", lty=3)
  points(ll, BIC(reg.gamlr)/n, pch=19, col="green")
  points(ll, AICc(reg.gamlr)/n, pch=19, col="black")
  legend("topleft", bty="n",
         fill=c("black","orange","green"),legend=c("AICc","AIC","BIC"))
  PlotDone()

  colors <- c('orange', 'green', 'black', 'blue', 'purple')
  ics <- GetICs(nhlreg_std, cv.nhlreg)
  PlotSetup(paste(file, "_c", sep=''))
  plot(reg.gamlr, col='darkgray', select=0)
  abline(v=ics$lambda, col=colors, lty=5, lwd=2)
  legend("bottomleft", bty="n", fill=colors, legend=row.names(ics))
  PlotDone()
}
PlotIC(nhlreg_std, cv.nhlreg, n, 'ic_nhl', ylim = c(33, 35.5))

##
## We’ve controlled our estimates for confounding information
## from team effects and special play configuration. How do things
## change if we ignored this info (i.e., fit a player-only model)?
## Which scheme is better (interpretability, CV, and IC)?
##
log_lambda <- -12
pl.nhlreg <- gamlr(player, y, family="binomial", lambda.min.ratio=exp(log_lambda))
cv.pl.nhlreg <- cv.gamlr(player, y, family="binomial", verb=T, lambda.min.ratio=exp(log_lambda))
PlotSetup('pl_cv_nhl_gamlr_a')
plot(cv.pl.nhlreg)
PlotDone()

PlotSetup('pl_cv_nhl_gamlr_b')
plot(cv.pl.nhlreg$gamlr)
PlotDone()

print(GetICs(pl.nhlreg, cv.pl.nhlreg))
ICs <- GetICs(pl.nhlreg, cv.pl.nhlreg)
colnames(ICs) <- c('$log(\\lambda)$')
print(xtable(ICs, label="tab:pl_ic", caption="ICs for Player-Only Data"),
      sanitize.text.function=function(x){x}, file=GetFilename('pl_ic.tex'))

PlotIC(pl.nhlreg, cv.pl.nhlreg, n, 'ic_pl_nhl')
