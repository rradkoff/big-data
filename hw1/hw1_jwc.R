# read data from CSV file
benjer <- read.csv("BenAndJerry.csv")

# dependent variable
y <- log(benjer$total_spent)

# possible independent variables
x <- benjer[,c("household_income","household_size","marital_status","region",
                 "age_and_presence_of_children")]
x$race <- factor(benjer$race, levels=1:4,
                 labels=c("white","black","asian","other"))
x$married <- factor(benjer$marital_status == 1)
x$usecoup <- factor(benjer$coupon_value>0)
x$promo_type <- factor(benjer$promotion_type, levels=1:4, 
                      labels=c("store feat", "store cpn", "mfg cpn", "other"))

# some interesting plots
plot(y/x$household_size~x$household_size) # small households spend more per person (careful w/ logs though)
plot(y ~ x$usecoup) # coupons don't seem to influence purchasing behavior (in aggregate)
plot(y ~ x$promo_type) # manufacturer coupons seem to work better
plot(y ~ x$race) # whie people buy more ice cream (could be an income story though)