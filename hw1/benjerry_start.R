#### Purchases of Ben and Jerry's Ice Cream
benjer = read.csv("BenAndJerry.csv")

## explore a bit
names(benjer)

## create a new variable for price per unit
priceper1 = (benjer$price_paid_deal + benjer$price_paid_non_deal)/benjer$quantity
y <- log(1+priceper1)

## grab some covariates of interest
## we'll create a properly formatted data.frame
x <- benjer[,c("flavor_descr","size1_descr",
	"household_income","household_size")]

## relevel 'flavor' to have baseline of vanilla
x$flavor_descr <- relevel(x$flavor_descr,"VAN")
## coupon usage
x$usecoup = factor(benjer$coupon_value>0)
x$couponper1 <- benjer$coupon_value/benjer$quantity
## organize some demographics
x$region <- factor(benjer$region, 
	levels=1:4, labels=c("East","Central","South","West"))
x$married <- factor(benjer$marital_status==1)
x$race <- factor(benjer$race,
	levels=1:4,labels=c("white","black","asian","other"))
x$hispanic_origin <- benjer$hispanic_origin==1
x$microwave <- benjer$kitchen_appliances %in% c(1,4,5,7)
x$dishwasher <- benjer$kitchen_appliances %in% c(2,4,6,7)
x$sfh <- benjer$type_of_residence==1
x$internet <- benjer$household_internet_connection==1
x$tvcable <- benjer$tv_items>1

## for TV cable, replace NA with zero
x$tvcable[is.na(x$tvcable)] <- 0

## look for multicollinearity
# check for correlations
cc <- cor( cbind(x$flavor_descr, x$size1_descr, x$household_income, x$household_size, 
                 x$married, x$race, x$usecoup, x$couponper1, x$region, x$hispanic_origin,
                x$microwave, x$dishwasher, x$sfh, x$internet, x$tvcable) )
# highlight correlations larger than [0.4]
cc[cc<0.4] <- 0
# 3+5, 4+5, 7+8, 6+10
# married correlates w/ household income and size
# use coupon and coupon per 1 are highly correlated
# race and hispanic origin correlate

## combine x and y, just to follow my recommended `way to use glm'
## cbind is `column bind'.  It takes two dataframes and makes one.
xy <- cbind(x,y)

## fit the regression
fit <- glm(y~., data=xy) 

## grab the non-intercept p-values from a glm
## -1 to drop the intercept, 4 is 4th column
pvals <- summary(fit)$coef[-1,4] 

## source the fdr_cut function
source("fdr.R")



