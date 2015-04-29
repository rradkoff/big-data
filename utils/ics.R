require(gamlr)

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