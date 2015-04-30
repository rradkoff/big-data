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
    cov = c(sum(coef(reg.gamlr, select=which.min(AICc(reg.gamlr))) != 0),
            sum(coef(reg.gamlr, select=which.min(AIC(reg.gamlr))) != 0),
            sum(coef(reg.gamlr, select=which.min(BIC(reg.gamlr))) != 0),
            sum(coef(reg.cv.gamlr, select="min") != 0),
            sum(coef(reg.cv.gamlr, select="1se") != 0)),
    row.names = 'ic'))
}

SaveICTable <- function(reg.gamlr, reg.cv.gamlr, label, caption, file) {
  ics <- GetICs(reg.gamlr, reg.cv.gamlr)
  colnames(ics) <- c('$\\log(\\lambda)$', 'Covariates Selected')
  print(xtable(ics, label=label, caption=caption),
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