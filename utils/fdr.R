## extract p-value cutoff for E[fdf] < q
fdr_cut <- function(pvals, q, plotit=FALSE, filename='fdr_cut') {
  pvals <- pvals[!is.na(pvals)]
  N <- length(pvals)
  
  k <- rank(pvals, ties.method="min")
  alpha <- max(pvals[ pvals <= (q*k/N) ])
  
  if (plotit) {
    sig <- factor(pvals<=alpha)
    o <- order(pvals)

    PlotSetup(filename)
    plot(pvals[o], log="xy", col=c("grey60","red")[sig[o]], pch=20, 
       ylab="p-values", xlab="tests ordered by p-value", main = paste('FDR =',q))
    lines(1:N, q*(1:N)/N)
    PlotDone()
  }
  
  return(alpha)
}

## re-compute glm for covariates above p-value cutoff for a given FDR (q)
fdr_signif <- function(reg_in, q, plotit=FALSE, filename='fdr_cut') {
  
  coefs <- summary(reg_in)$coef # -1 to drop the intercept
  coefs <- coefs[-1,]
  
  ## grab the non-intercept p-values from a glm
  # Extract just the p-values from the 4th column
  pvals <- coefs[,4]
  
  # Calculate the False Discovery Rate Alpha Parameter
  alpha <- fdr_cut(pvals, q = q, plotit, filename)
  
  # Get a list of just the significant variables.
  signif <- coef(reg_in)[-1][pvals<=alpha]
}

glm_fdr <- function(reg_in, q, plotit=FALSE, filename='fdr_cut') {
  signif <- fdr_signif(reg_in, q, plotit, filename)
  
  ## Re-fit model with only significant covariates
  mm <- model.matrix(reg_in$formula, data=reg_in$model) #[,-1] # -1 to drop the intercept
  reg_out <- glm( reg_in$y ~ ., data=as.data.frame(mm)[, names(signif)])
   
  return(reg_out)
}
