## Big Data HW 2

source('../utils/source_me.R', chdir = T)
OutputToFile = T

## read data
homes <- read.csv('homes2004.csv')

## organize data

# dependent variable: log of price 
y <- log(homes$VALUE)

# independent variables
x <- homes
x$LAMMORT <- log(x$AMMORT) # log of amt of first mortgage
x$LPRICE <- log(x$LPRICE) # log of land + unit price
x$LZINC2 <- log(x$ZINC2) # log of household income
x$AMMORT <- NULL # drop variables that are tracked in logs (above)
x$ZINC2 <- NULL
x$VALUE <- NULL # drop dependent variable

## make some interesting plots

#par(mfrow=c(1,2))

# plot home value vs education level
x$HHGRAD <- factor(x$HHGRAD, levels=c("No HS", "HS Grad", "Assoc", "Bach", "Grad"))
PlotSetup('hhgrad')
plot(y~x$HHGRAD, col=rainbow(nlevels(x$HHGRAD)), outline=F, 
     main="", xlab="Education Level of Homeowner", ylab="Current market value (log)")
PlotDone()

# plot home value vs household income and show college degree status
# for plotting only, make a dummy var for has college degree
x$COLLEGE <- x$HHGRAD
levels(x$COLLEGE) <- list("No College"=c("No HS", "HS Grad", "Assoc"), 
                          "College"=c("Bach", "Grad"))

PlotSetup('income')
plot(y~x$LZINC2, col=c("blue","red")[as.numeric(x$COLLEGE)],
     main="", xlab="Income of Homeowner (log)", ylab="Current market value (log)")
legend("bottomleft", legend=levels(x$COLLEGE), pch=c(1,1),
       col=c("blue","red"))
PlotDone()

# first home and downpayment source (same story)
PlotSetup('frstho')
plot(y~x$FRSTHO, outline=F, varwidth=T, col=c("red","blue"),
     main="", xlab="Buyer's First Home?", ylab="Current market value (log)")
PlotDone()

# change order of level to match first home y/n
x$DWNPAY <- factor(x$DWNPAY, c("prev home","other"))
PlotSetup('dwnpay')
plot(y~x$DWNPAY, outline=F, varwidth=T, col=c("red","blue"),
     main="", xlab="Downpayment Source", ylab="Current market value (log)")
PlotDone()

# of bathrooms/bedrooms (same story as well)
PlotSetup('baths')
boxplot(y~x$BATHS, outline=F, varwidth=T, col=rainbow(nlevels(as.factor(x$BATHS))),
        main="", xlab="# of Bathrooms", ylab="Current market value (log)")
PlotDone()

PlotSetup('bedrms')
boxplot(y~x$BEDRMS, outline=F, varwidth=T, col=rainbow(nlevels(as.factor(x$BEDRMS))),
        main="", xlab="# of Bedrooms", ylab="Current market value (log)")
PlotDone()

# neighborhood quality factors
# howH, howN, eaban, ejunk, ecom2
PlotSetup('howh')
boxplot(y~x$HOWH, outline=F, varwidth=T, col=c("red", "blue"),
        main="", xlab="Unit Quality", ylab="Current market value (log)")
PlotDone()

PlotSetup('hown')
boxplot(y~x$HOWN, outline=F, varwidth=T, col=c("red", "blue"),
        main="", xlab="Neighborhood Quality", ylab="Current market value (log)")
PlotDone()

# plot some neighborhood quality metrics
# cheap way to see which difference is largest between these three
#a <- mean(y[x$EABAN=="Y"])-mean(y[x$EABAN=="N"])
#b <- mean(y[x$EJUNK=="Y"])-mean(y[x$EJUNK=="N"])
#c <- mean(y[x$ECOM2=="Y"])-mean(y[x$ECOM2=="N"])

PlotSetup('eaban')
plot(y~x$EABAN, outline=F, varwidth=T, col=c("red", "blue"),
     main="", xlab="Close to Abandoned Building?", ylab="Current market value (log)")
PlotDone()

PlotSetup('ejunk')
plot(y~x$EJUNK, outline=F, varwidth=T, col=c("red", "blue"),
     main="", xlab="Trash/Junk in Street?", ylab="Current market value (log)")
PlotDone()