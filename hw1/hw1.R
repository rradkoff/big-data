## source the fdr_cut function
source("fdr.R")
source('../utils/source_me.R', chdir = T)
OutputToFile = T

#### Purchases of Ben and Jerry's Ice Cream
benjer <- read.csv("BenAndJerry.csv")

## explore a bit
# print('Names:')
# print(names(benjer))

## create a new variable for price per unit
priceper1 = (benjer$price_paid_deal + benjer$price_paid_non_deal)/benjer$quantity
y <- log(1+priceper1)

## create a new variable for log of total spent
y <- log(benjer$total_spent)

## grab some covariates of interest
## we'll create a properly formatted data.frame
x <- benjer[,c("flavor_descr","size1_descr",
               "household_income","household_size")]

## relevel 'flavor' to have baseline of vanilla
x$flavor_descr <- relevel(x$flavor_descr,"VAN")
## coupon usage
# See the following for factor definitions:
# http://faculty.chicagobooth.edu/matt.taddy/teaching/HomescanDocs.htm
x$usecoup <- factor(benjer$coupon_value>0)
x$couponper1 <- benjer$coupon_value/benjer$quantity
## organize some demographics
x$region <- factor(benjer$region, levels=1:4,
                   labels=c("East","Central","South","West"))
x$married <- factor(benjer$marital_status == 1)
x$race <- factor(benjer$race, levels=1:4,
                 labels=c("white","black","asian","other"))
x$hispanic_origin <- benjer$hispanic_origin == 1
x$microwave <- benjer$kitchen_appliances %in% c(1,4,5,7)
x$dishwasher <- benjer$kitchen_appliances %in% c(2,4,6,7)
x$sfh <- benjer$type_of_residence == 1
x$internet <- benjer$household_internet_connection == 1
x$tvcable <- benjer$tv_items > 1

## combine x and y, just to follow my recommended `way to use glm'
## cbind is `column bind'.  It takes two dataframes and makes one.
xy <- cbind(x,y)

print("XY Names:")
print(paste(names(xy), collapse=', '))

## fit the regression
fit <- glm(y~., data=xy) 
# print(summary(fit))

## -1 to drop the intercept
coefs <- summary(fit)$coef
coefs <- coefs[-1,]

## grab the non-intercept p-values from a glm
# Extract just the p-values from the 4th column
pvals <- coefs[,4]

PlotSetup('pvalue_hist')
hist(pvals, xlab = 'P-Values', main = 'Histogram of P-Values')
PlotDone()

# Calculate the False Discovery Rate Alpha Parameter
q = 10/100
alpha <- fdr_cut(pvals, q = q, plotit = TRUE)

# Get a list of just the significant variables.
significant <- coefs[coefs[,4] < alpha,]

# Now order that list by increasing p-values.
print(paste("'Significant Explanatory' Variables (q<", q, ", alpha<", alpha, ")", sep=""))
print(significant[order(significant[,4]),])
