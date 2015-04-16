## Big Data HW 2

source('../utils/source_me.R', chdir = T)
source("../utils/fdr.R")
source("../utils/deviance.R")
OutputToFile = T

## READ DATA
homes <- read.csv('homes2004.csv')

## ORGANIZE DATA

# dependent variable: log of price 
y <- log(homes$VALUE)

# ## Q1: DATA VISUALIZATION
# #  Plot some relationships and tell a story
# 
# #par(mfrow=c(1,2))
# 
# # plot home value vs education level
# homes$HHGRAD <- factor(homes$HHGRAD, levels=c("No HS", "HS Grad", "Assoc", "Bach", "Grad"))
# PlotSetup('hhgrad')
# plot(y~homes$HHGRAD, col=rainbow(nlevels(homes$HHGRAD)), outline=F, 
#      main="", xlab="Education Level of Homeowner", ylab="Current market value (log)")
# PlotDone()
# 
# # plot home value vs household income and show college degree status
# # for plotting only, make a dummy var for has college degree
# homes$COLLEGE <- homes$HHGRAD
# levels(homes$COLLEGE) <- list("No College"=c("No HS", "HS Grad", "Assoc"), 
#                               "College"=c("Bach", "Grad"))
# 
# PlotSetup('income')
# plot(y~log(homes$ZINC2), col=c("blue","red")[as.numeric(homes$COLLEGE)],
#      main="", xlab="Income of Homeowner (log)", ylab="Current market value (log)", pch=16)
# legend("bottomleft", legend=levels(homes$COLLEGE), pch=16,
#        col=c("blue","red"))
# PlotDone()
# 
# homes$COLLEGE <- NULL
# 
# # first home and downpayment source (same story)
# PlotSetup('frstho')
# plot(y~homes$FRSTHO, outline=F, varwidth=T, col=c("red","blue"),
#      main="", xlab="Buyer's First Home?", ylab="Current market value (log)")
# PlotDone()
# 
# # change order of level to match first home y/n
# homes$DWNPAY <- factor(homes$DWNPAY, c("prev home","other"))
# PlotSetup('dwnpay')
# plot(y~homes$DWNPAY, outline=F, varwidth=T, col=c("red","blue"),
#      main="", xlab="Downpayment Source", ylab="Current market value (log)")
# PlotDone()
# 
# # of bathrooms/bedrooms (same story as well)
# PlotSetup('baths')
# boxplot(y~homes$BATHS, outline=F, varwidth=T, col=rainbow(nlevels(as.factor(homes$BATHS))),
#         main="", xlab="# of Bathrooms", ylab="Current market value (log)")
# PlotDone()
# 
# PlotSetup('bedrms')
# boxplot(y~homes$BEDRMS, outline=F, varwidth=T, col=rainbow(nlevels(as.factor(homes$BEDRMS))),
#         main="", xlab="# of Bedrooms", ylab="Current market value (log)")
# PlotDone()
# 
# # neighborhood quality factors
# # howH, howN, eaban, ejunk, ecom2
# PlotSetup('howh')
# boxplot(y~homes$HOWH, outline=F, varwidth=T, col=c("red", "blue"),
#         main="", xlab="Unit Quality", ylab="Current market value (log)")
# PlotDone()
# 
# PlotSetup('hown')
# boxplot(y~homes$HOWN, outline=F, varwidth=T, col=c("red", "blue"),
#         main="", xlab="Neighborhood Quality", ylab="Current market value (log)")
# PlotDone()
# 
# PlotSetup('first_home_vs_property_type')
# multifamily = factor(homes$NUNITS>1, labels = c('House', 'Condo'))
# plot(homes$FRSTHO ~ multifamily, xlab="Property Type", ylab="Buyer's First Home?")
# PlotDone()
# 
# PlotSetup('home_quality_vs_education')
# plot(homes$HOWH ~ homes$HHGRAD, xlab="Education Level", ylab="Unit Quality")
# PlotDone()
# 
# PlotSetup('neighborhood_quality_vs_education')
# plot(homes$HOWN ~ homes$HHGRAD, xlab="Education Level", ylab="Neighborhood Quality")
# PlotDone()
# 
# PlotSetup('first_home_vs_home_quality')
# plot(homes$FRSTHO ~ homes$HOWH, xlab="Unit Quality", ylab="Buyer's First Home?")
# PlotDone()
# 
# # plot some neighborhood quality metrics
# # cheap way to see which difference is largest between these three
# #a <- mean(y[homes$EABAN=="Y"])-mean(y[homes$EABAN=="N"])
# #b <- mean(y[homes$EJUNK=="Y"])-mean(y[homes$EJUNK=="N"])
# #c <- mean(y[homes$ECOM2=="Y"])-mean(y[homes$ECOM2=="N"])
# 
# PlotSetup('eaban')
# plot(y~homes$EABAN, outline=F, varwidth=T, col=c("red", "blue"),
#      main="", xlab="Close to Abandoned Building?", ylab="Current market value (log)")
# PlotDone()
# 
# PlotSetup('ejunk')
# plot(y~homes$EJUNK, outline=F, varwidth=T, col=c("red", "blue"),
#      main="", xlab="Trash/Junk in Street?", ylab="Current market value (log)")
# PlotDone()

## Q2: SIMPLE REGRESSION
#  Regress log value onto all but mortgage and purchase $
#  How many coefficients are significant at 10% FDR?
#  Re-run regression with only the significant covariates

reg1 <- glm(log(VALUE) ~ .-AMMORT-LPRICE, data=homes)

# -1 to drop the intercept
coefs <- summary(reg1)$coef
coefs <- coefs[-1,]

## grab the non-intercept p-values from a glm
# Extract just the p-values from the 4th column
pvals <- coefs[,4]

# Calculate the False Discovery Rate Alpha Parameter
q = 10/100
alpha <- fdr_cut(pvals, q = q, plotit = TRUE)

# Get a list of just the significant variables.
signif <- coef(reg1)[-1][pvals<=alpha]

# How many true discoveries?
print(paste("True discoveries:", length(signif), " out of ", nrow(coefs)))

## Re-fit model with only significant covariates
mm <- model.matrix( ~.-AMMORT-LPRICE, data=homes)[,-1]
reg2 <- glm( log(mm[,"VALUE"]) ~ ., data=as.data.frame(mm)[, names(signif)])
print(summary(reg2))
print(paste("reg1 R^2=", 1-reg1$deviance/reg1$null.deviance,
            ", reg2 R^2=", 1-reg2$deviance/reg2$null.deviance))

##
# Q3
# logit regression whether the buyer placed 20% down or not
##

# create new var for first home or not
homes$gt20dwn <- factor(0.2<(homes$LPRICE-homes$AMMORT)/homes$LPRICE)

# logit regression: y = 20% down or not, x = all other (less mort amt, price)
reg3 <- glm(gt20dwn ~ .-AMMORT-LPRICE, data=homes, family='binomial')
print(summary(reg3))

# logit regression: y = 20% down or not, x = all other (less mort amt, price) 
# with ineraction between # of baths and dummy for first home
reg4 <- glm(gt20dwn ~ .-AMMORT-LPRICE+FRSTHO*BATHS, data=homes, family='binomial')
print(summary(reg4))

# -1 to drop the intercept
coefs <- summary(reg4)$coef
coefs <- coefs[-1,]

## grab the non-intercept p-values from a glm
# Extract just the p-values from the 4th column
pvals <- coefs[,4]

# Calculate the False Discovery Rate Alpha Parameter
q = 10/100
alpha <- fdr_cut(pvals, q = q, plotit = TRUE, filename = 'binom_fdr')

# Get a list of just the significant variables.
signif <- coef(reg4)[-1][pvals<=alpha]

# How many true discoveries?
print(paste("True discoveries:", length(signif), " out of ", nrow(coefs)))

mm <- model.matrix( ~.-AMMORT-LPRICE+FRSTHO*BATHS, data=homes)[,-1]
reg5 <- glm( log(mm[,"VALUE"]) ~ ., data=as.data.frame(mm)[, names(signif)])
print(summary(reg5))

##
# Q4
# out of sample prediction
##
gt100 <- which(homes$VALUE>1e5)

# print(head(homes[gt100,]))
# print(head(homes[-gt100,]))

reg6 <- glm(gt20dwn ~ .-AMMORT-LPRICE+FRSTHO*BATHS, data=homes[gt100,], family='binomial')
print(summary(reg6))
pred_gt20wn <- predict.glm(reg6, newdata = homes[-gt100,], type='response')
print(paste("R^2=", R2(y=homes$gt20dwn[-gt100], pred = pred_gt20wn, family = "binomial")))
#print(paste("R^2=", R2(homes[-gt100], pred = pred_gt20wn, family = "binomial")))

PlotSetup("oos")
plot(pred_gt20wn ~ homes$gt20dwn[-gt100], 
     xlab="", ylab=c("predicted probability of large down payment"), 
     varwidth=T, col=c("navy","red"))
PlotDone()