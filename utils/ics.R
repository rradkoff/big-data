require(gamlr)
require(xtable)

## log lambdas selected under various criteria
GetICs <- function(reg.gamlr, reg.cv.gamlr) {
  return(data.frame(
    ic = c('AICc', 'AIC', 'BIC', 'CV.Min', 'CV.1se'),
    lambda = c(log(reg.gamlr$lambda[which.min(AICc(reg.gamlr))]),
               log(reg.gamlr$lambda[which.min(AIC(reg.gamlr))]),
               log(reg.gamlr$lambda[which.min(BIC(reg.gamlr))]),
               log(reg.cv.gamlr$lambda.min),
               log(reg.cv.gamlr$lambda.1se)),
    rsquared = c(1-reg.gamlr$deviance[which.min(AICc(reg.gamlr))]/reg.gamlr$deviance[1],
                 1-reg.gamlr$deviance[which.min(AIC(reg.gamlr))]/reg.gamlr$deviance[1],
                 1-reg.gamlr$deviance[which.min(BIC(reg.gamlr))]/reg.gamlr$deviance[1],
                 1-reg.cv.gamlr$cvm[reg.cv.gamlr$seg.min]/reg.cv.gamlr$cvm[1],
                 1-reg.cv.gamlr$cvm[reg.cv.gamlr$seg.1se]/reg.cv.gamlr$cvm[1]),
    cov = c(sum(coef(reg.gamlr, select=which.min(AICc(reg.gamlr))) != 0),
            sum(coef(reg.gamlr, select=which.min(AIC(reg.gamlr))) != 0),
            sum(coef(reg.gamlr, select=which.min(BIC(reg.gamlr))) != 0),
            sum(coef(reg.cv.gamlr, select="min") != 0),
            sum(coef(reg.cv.gamlr, select="1se") != 0)),
    row.names = 'ic'))
}

SaveICTable <- function(reg.gamlr, reg.cv.gamlr, file, caption) {
  ics <- GetICs(reg.gamlr, reg.cv.gamlr)
  colnames(ics) <- c('$\\log(\\lambda)$', '$R^2$', 'Covariates Selected')
  print(xtable(ics, label=paste('tab:', file, sep=''), caption=caption),
        sanitize.text.function=function(x){x}, 
        file=GetFilename(paste(file, '.tex', sep='')))
}

PlotICs <- function(reg.gamlr, reg.cv.gamlr, file, legend_loc = "bottomleft") {
  colors <- c('orange', 'green', 'black', 'blue', 'purple')
  ics <- GetICs(reg.gamlr, reg.cv.gamlr)
  PlotSetup(file)
  plot(reg.gamlr, col='darkgray', select=0)
  abline(v=ics$lambda, col=colors, 
         lty=c(2, 3, 2, 2, 2), lwd=2)
  legend(legend_loc, bty="n", fill=colors, legend=row.names(ics))
  PlotDone()
}

PlotICFunctions <- function(reg.gamlr, reg.cv.gamlr, file) {
  ll <- log(reg.gamlr$lambda) ## the sequence of lambdas
  n <- reg.gamlr$nobs
  
  PlotSetup(paste(file, 'a', sep='_'))
  plot(reg.cv.gamlr)
  PlotDone()
  
  PlotSetup(paste(file, 'b', sep='_'))
  plot(ll, AIC(reg.gamlr)/n, 
       xlab="log lambda", ylab="IC/n", pch=21, bg="orange")
  abline(v=ll[which.min(AIC(reg.gamlr))], col="orange", lty=3)
  abline(v=ll[which.min(BIC(reg.gamlr))], col="green", lty=3)
  abline(v=ll[which.min(AICc(reg.gamlr))], col="black", lty=3)
  points(ll, BIC(reg.gamlr)/n, pch=21, bg="green")
  points(ll, AICc(reg.gamlr)/n, pch=21, bg="black")
  legend("topleft", bty="n",
         fill=c("black","orange","green"),legend=c("AICc","AIC","BIC"))
  PlotDone()
}