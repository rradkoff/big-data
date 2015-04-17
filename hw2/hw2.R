## Big Data HW 2
rm(list=ls())

source('../utils/source_me.R', chdir = T)
source("../utils/fdr.R")
source("../utils/deviance.R")
require('xtable')
plotOpts$Prefix="writeup/"
OutputToFile = T

q=0.1

## READ DATA
homes <- read.csv('homes2004.csv')

## ORGANIZE DATA

# dependent variable: log of price 
y <- log(homes$VALUE)

##
# Q1
# Plot some relationships and tell a story
##
Q1 <- function() {
  #par(mfrow=c(1,2))
  
  # plot home value vs education level
  homes$HHGRAD <- factor(homes$HHGRAD, levels=c("No HS", "HS Grad", "Assoc", "Bach", "Grad"))
  PlotSetup('hhgrad')
  plot(y~homes$HHGRAD, col=rainbow(nlevels(homes$HHGRAD)), outline=F, 
       main="", xlab="Education Level of Homeowner", ylab="Current market value (log)")
  PlotDone()
  
  # plot home value vs household income and show college degree status
  # for plotting only, make a dummy var for has college degree
  homes$COLLEGE <- homes$HHGRAD
  levels(homes$COLLEGE) <- list("No College"=c("No HS", "HS Grad", "Assoc"), 
                                "College"=c("Bach", "Grad"))
  
  PlotSetup('income')
  plot(y~log(homes$ZINC2), col=c("blue","red")[as.numeric(homes$COLLEGE)],
       main="", xlab="Income of Homeowner (log)", ylab="Current market value (log)", pch=16)
  legend("bottomleft", legend=levels(homes$COLLEGE), pch=16,
         col=c("blue","red"))
  PlotDone()
  
  homes$COLLEGE <- NULL
  
  # first home and downpayment source (same story)
  PlotSetup('frstho')
  plot(y~homes$FRSTHO, outline=F, varwidth=T, col=c("red","blue"),
       main="", xlab="Buyer's First Home?", ylab="Current market value (log)")
  PlotDone()
  
  # change order of level to match first home y/n
  homes$DWNPAY <- factor(homes$DWNPAY, c("prev home","other"))
  PlotSetup('dwnpay')
  plot(y~homes$DWNPAY, outline=F, varwidth=T, col=c("red","blue"),
       main="", xlab="Downpayment Source", ylab="Current market value (log)")
  PlotDone()
  
  # of bathrooms/bedrooms (same story as well)
  PlotSetup('baths')
  boxplot(y~homes$BATHS, outline=F, varwidth=T, col=rainbow(nlevels(as.factor(homes$BATHS))),
          main="", xlab="# of Bathrooms", ylab="Current market value (log)")
  PlotDone()
  
  PlotSetup('bedrms')
  boxplot(y~homes$BEDRMS, outline=F, varwidth=T, col=rainbow(nlevels(as.factor(homes$BEDRMS))),
          main="", xlab="# of Bedrooms", ylab="Current market value (log)")
  PlotDone()
  
  # neighborhood quality factors
  # howH, howN, eaban, ejunk, ecom2
  PlotSetup('howh')
  boxplot(y~homes$HOWH, outline=F, varwidth=T, col=c("red", "blue"),
          main="", xlab="Unit Quality", ylab="Current market value (log)")
  PlotDone()
  
  PlotSetup('hown')
  boxplot(y~homes$HOWN, outline=F, varwidth=T, col=c("red", "blue"),
          main="", xlab="Neighborhood Quality", ylab="Current market value (log)")
  PlotDone()
  
  PlotSetup('first_home_vs_property_type')
  multifamily = factor(homes$NUNITS>1, labels = c('House', 'Condo'))
  plot(homes$FRSTHO ~ multifamily, xlab="Property Type", ylab="Buyer's First Home?")
  PlotDone()
  
  PlotSetup('home_quality_vs_education')
  plot(homes$HOWH ~ homes$HHGRAD, xlab="Education Level", ylab="Unit Quality")
  PlotDone()
  
  PlotSetup('neighborhood_quality_vs_education')
  plot(homes$HOWN ~ homes$HHGRAD, xlab="Education Level", ylab="Neighborhood Quality")
  PlotDone()
  
  PlotSetup('first_home_vs_home_quality')
  plot(homes$FRSTHO ~ homes$HOWH, xlab="Unit Quality", ylab="Buyer's First Home?")
  PlotDone()
  
  PlotSetup('home_value_vs_20_down')
  plot(factor(0.2<(homes$LPRICE-homes$AMMORT)/homes$LPRICE, labels= c("No", "Yes")) ~ 
         factor(homes$VALUE > 1e5, labels = c("No", "Yes")),
       xlab="Home Value > 100k", ylab="20% Down Payment")
  PlotDone()
  
  # plot some neighborhood quality metrics
  
  PlotSetup('eaban')
  plot(y~homes$EABAN, outline=F, varwidth=T, col=c("red", "blue"),
       main="", xlab="Close to Abandoned Building?", ylab="Current market value (log)")
  PlotDone()
  
  PlotSetup('ejunk')
  plot(y~homes$EJUNK, outline=F, varwidth=T, col=c("red", "blue"),
       main="", xlab="Trash/Junk in Street?", ylab="Current market value (log)")
  PlotDone()
}
# Q1()

##
# Q2
# Regress log value onto all but mortgage and purchase $
# How many coefficients are significant at 10% FDR?
# Re-run regression with only the significant covariates, and compare R^2 to the full model
##
Q2 <- function() {
  # set FDR cutoff
  q = 10/100
  
  reg1 <- glm(log(VALUE) ~ .-AMMORT-LPRICE, data=homes)
  reg1_fdr <- glm_fdr(reg1, q)
  print(xtable(reg1_fdr, label = 'tab:reg1_fdr',
               caption = 'Value of Purchased Homes (in logs)'), 
        file=GetFilename('reg1_fdr.tex'))
  
  print(paste("True discoveries:", dim(summary(reg1_fdr)$coef)[1], 
              " out of ", dim(summary(reg1)$coef)[1]))
  
  print(paste("reg1 R^2=", 1-reg1$deviance/reg1$null.deviance,
              ", reg2 R^2=", 1-reg1_fdr$deviance/reg1_fdr$null.deviance))
}
# Q2()

##
# Q3
# Fit a regression for whether the buyer had ≥ 20% down
#   (again, onto everything but AMMORT and LPRICE)
# Interpret effects for 1st home buyers and # of bathrooms
# Add + describe interaction for 1st home-buyers and #baths
##
Q3 <- function() {
  homes$gt20dwn <- factor(0.2<(homes$LPRICE-homes$AMMORT)/homes$LPRICE)
  
  reg3 <- glm(gt20dwn ~ .-AMMORT-LPRICE, data=homes, family='binomial')
  reg3_fdr <- glm_fdr(reg3, q)
  print(xtable(reg3_fdr, label='tab:reg3_fdr', 
               caption = 'Probability of Down Payment $>$ 20\\% (No Interaction Terms)'), 
        file=GetFilename('reg3_fdr.tex'))
  print(paste("(reg3) R^2=", 1-reg3$deviance/reg3$null.deviance))
  print(paste("(reg3 fdr) R^2=", 1-reg3_fdr$deviance/reg3_fdr$null.deviance))
  
  # add interaction term
  reg4 <- glm(gt20dwn ~ .-AMMORT-LPRICE+FRSTHO*BATHS, data=homes, family='binomial')
  reg4_fdr <- glm_fdr(reg4, q)
  print(xtable(reg4_fdr, label='tab:reg4_fdr', 
               caption = 'Probability of Down Payment $>$ 20\\% (With Interaction Term)'), 
        file=GetFilename('reg4_fdr.tex'))
  print(paste("(reg4) R^2=", 1-reg4$deviance/reg4$null.deviance))
  print(paste("(reg4 fdr) R^2=", 1-reg4_fdr$deviance/reg4_fdr$null.deviance))
}
# Q3()

##
# Q4
# Re-fit your model from Q3 for only homes worth > 100k
# Compare in-sample fit to R^2 for predicting homes worth <100k
##
Q4sample <- function() {
  gt100 <- which(homes$VALUE>1e5)
  homes$gt20dwn <- factor(0.2<(homes$LPRICE-homes$AMMORT)/homes$LPRICE)

  sampleIndex <- sample(1:length(gt100), size = 1000)
  
  mm <- model.matrix( ~. -AMMORT-LPRICE + FRSTHO*BATHS, data=homes)
  reg6 <- glm(gt20dwnTRUE ~., 
              data=as.data.frame(mm)[gt100[-sampleIndex],], 
              family='binomial')
  print(summary(reg6))
  
  pred_gt20wn <- predict.glm(reg6, newdata = as.data.frame(mm)[gt100[sampleIndex],], type='response')
  print(paste("OOS R^2=", R2(y=homes$gt20dwn[gt100[sampleIndex]], pred = pred_gt20wn, family = "binomial")))
  print(paste("Model R^2=", 1-reg6$deviance/reg6$null.deviance))
#   #print(paste("R^2=", R2(homes[-gt100], pred = pred_gt20wn, family = "binomial")))
#   
  PlotSetup("oos_subsample_100k")
  plot(pred_gt20wn ~ homes$gt20dwn[gt100[sampleIndex]], 
       xlab="Down Payment Actually >20%", ylab=c("Pr(Down Payment >20%)"), 
       varwidth=T, col=c("navy","red"))
  PlotDone()
}
# Q4sample()

Q4 <- function() {
  gt100 <- which(homes$VALUE>1e5)
  homes$gt20dwn <- factor(0.2<(homes$LPRICE-homes$AMMORT)/homes$LPRICE)
  
  reg <- glm(gt20dwn ~. -AMMORT-LPRICE + FRSTHO*BATHS, data=homes, family="binomial")
  signif <- fdr_signif(reg, q, TRUE, 'gt20dwn_gt100k_fdr')

  mm <- model.matrix( ~. -AMMORT-LPRICE + FRSTHO*BATHS, data=homes)
  reg6 <- glm(gt20dwnTRUE ~., 
              data=as.data.frame(mm)[gt100, append(names(signif), "gt20dwnTRUE")], 
              family='binomial')
  print(summary(reg6))
  print(xtable(reg6, label='tab:reg6_fdr', 
               caption = 'Probability of Down Payment $>20\\%$ (for Home Value $>100k$)'), 
        file=GetFilename('reg6_fdr.tex'))
  
  pred_gt20wn <- predict.glm(reg6, newdata = as.data.frame(mm)[-gt100,], type='response')
  print(paste("OOS R^2=", R2(y=homes$gt20dwn[-gt100], pred = pred_gt20wn, family = "binomial")))
  print(paste("Model R^2=", 1-reg6$deviance/reg6$null.deviance))
  #   #print(paste("R^2=", R2(homes[-gt100], pred = pred_gt20wn, family = "binomial")))
  #   
  PlotSetup("oos_lt100k")
  plot(pred_gt20wn ~ homes$gt20dwn[-gt100], 
       xlab="Down Payment Actually >20%", ylab=c("Pr(Down Payment >20%)"), 
       varwidth=T, col=c("navy","red"))
  PlotDone()
}

Q1()
Q2()
Q3()
#Q4sample()
Q4()