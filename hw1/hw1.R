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
#y <- log(benjer$total_spent)

## grab some covariates of interest
## we'll create a properly formatted data.frame
x <- benjer[,c("size1_descr",
               "household_income","household_size")]

## relevel 'flavor' to have baseline of vanilla
# x$flavor_descr <- relevel(x$flavor_descr,"VAN")
## coupon usage
# See the following for factor definitions:
# http://faculty.chicagobooth.edu/matt.taddy/teaching/HomescanDocs.htm
x$usecoup <- factor(benjer$coupon_value>0)
#x$couponper1 <- benjer$coupon_value/benjer$quantity
## organize some demographics
x$region <- factor(benjer$region, levels=1:4,
                   labels=c("East","Central","South","West"))
#x$married <- factor(benjer$marital_status == 1)
x$race <- factor(benjer$race, levels=1:4,
                 labels=c("white","black","asian","other"))
#x$hispanic_origin <- benjer$hispanic_origin == 1
x$microwave <- benjer$kitchen_appliances %in% c(1,4,5,7)
x$dishwasher <- benjer$kitchen_appliances %in% c(2,4,6,7)
x$sfh <- benjer$type_of_residence == 1
x$internet <- benjer$household_internet_connection == 1
x$tvcable <- benjer$tv_items > 1

CollapseFlavors <- function(initialFlavors, numFlavorsToConsider = 5) {
  # Most popular flavors
  flavorDesc <- initialFlavors
  flavorsToConsider <- levels(flavorDesc)[
    order(summary(flavorDesc), decreasing = T)][1:numFlavorsToConsider]

  levels(flavorDesc) <- list("OTHER" = levels(flavorDesc)[!levels(flavorDesc) %in% flavorsToConsider], 
                             "POPULAR" = flavorsToConsider)
  print(paste(numFlavorsToConsider, " most popular flavors: ", 
              paste(flavorsToConsider, collapse = ", ")))
  return(flavorDesc)
}
x$flavorDesc <- CollapseFlavors(benjer$flavor_descr)


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
q = 1/100
alpha <- fdr_cut(pvals, q = q, plotit = TRUE)

# Get a list of just the significant variables.
significant <- coefs[coefs[,4] < alpha,]

# Now order that list by increasing p-values.
print(paste("'Significant Explanatory' Variables (q<", q, ", alpha<", alpha, ")", sep=""))
print(significant[order(significant[,4]),])

fit2 <- glm(y ~ size1_descr + household_income + region + household_size +
              microwave + sfh + usecoup + race + tvcable,
            data = xy)
print(summary(fit2))

PlotSetup('race')
boxplot(y ~ xy$race, outline = F, varwidth = T, ylab = 'log(price)', main = 'log(price) vs Race')
PlotDone()

PlotSetup('region')
boxplot(y ~ xy$region, outline = F, varwidth = T, ylab = 'log(price)', main = 'log(price) vs Region')
PlotDone()

# incomeLevels <- c("<$5000",
#                   "$5000-$7999",
#                   "$8000-$9999",
#                   "$10,000-$11,999",
#                   "$12,000-$14,999",
#                   "$15,000-$19,999",
#                   "$20,000-$24,999",
#                   "$25,000-$29,999",
#                   "$30,000-$34,999",
#                   "$35,000-$39,999",
#                   "$40,000-$44,999",
#                   "$45,000-$49,999",
#                   "$50,000-$59,999",
#                   "$60,000-$69,999",
#                   "$70,000-$99,999",
#                   "$100,000-$124,999",
#                   "$125,000-$149,999",
#                   "$150,000-$199,999",
#                   ">$200,000")
# income <- factor(xy$household_income, labels = incomeLevels)
PlotSetup('income')
boxplot(y ~ xy$household_income, varwidth = T, outline = F, 
        xlab = "Income Factor", ylab = "log(price)", main = "log(price) vs Income")
PlotDone()