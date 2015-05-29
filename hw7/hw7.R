##
## housekeeping
##

rm(list = ls())
source('../utils/source_me.R', chdir = T)
OutputToFile = T
plotOpts$Prefix = "writeup/"


###############################################################################
# Read Data
###############################################################################
sp <- read.csv('sp500.csv')
fx <- read.csv("FXmonthly.csv")
codes <- read.csv('currency_codes.txt', sep=' ', quote="'", header=F,
                  col.names=c("code", "country"), row.names="code")

# Convert to a percentage return over the previous month.
fx <- (fx[2:120,]-fx[1:119,])/(fx[1:119,])

# Substitute country names for the hard-to-read codes
colNames <- gsub("\\w{2}(\\w{2})\\w{2}", "\\1", colnames(fx))
library(stringr)
colnames(fx) <- str_replace(codes[colNames,], " ", "_")

###############################################################################
# 1)
###############################################################################
# heatmap(cor(fx), Rowv = NA, Colv = NA, symm = T)
heatmap(cor(fx), symm = T, main=('US exchange-rate Correlation Heatmap'),
        col=heat.colors(32))

###############################################################################
# 2)
###############################################################################
# TODO(mdelio) should we be transposing - evidence says yes, logic says no
pca <- prcomp(fx, scale = T)

PlotSetup('screeplot')
plot(pca)
PlotDone()

zpca <- predict(pca)

print(round(pca$rotation, digits=2))

# I'm confused if we want to scale the columns or the rows (I think the columns?)
# Now I think the rows, so the columns of the transposed fx.
Km <- 4
fx.km <- kmeans(scale(fx), centers = Km)

# TODO(mdelio) - add xlims so that text doesn't get chopped
plot(zpca[,1:2], type="n")
text(x=zpca[,1], y=zpca[,2], labels=colnames(fx),
     col=rainbow(Km, alpha=0.7)[fx.km$cluster])

plot(zpca[,3:4], type="n")
text(x=zpca[,3], y=zpca[,4], labels=colnames(fx),
     col=rainbow(Km, alpha=0.7)[fx.km$cluster])

###############################################################################
# 3)
###############################################################################
# Is this what he means by "glm on first K"? Or does he mean glm on the first
# cluster of currencies, or something else?
spReg.glm <- glm(sp$sp500 ~ pca$x[,"PC1"])

require(gamlr)
spReg <- gamlr(pca$x, sp$sp500)
spReg.cv <- cv.gamlr(pca$x, sp$sp500, verb=T)
plot(spReg.cv)
PlotICFunctions(spReg, spReg.cv, 'spreg_ics')
PlotICs(spReg, spReg.cv, 'spreg')
SaveICTable(spReg, spReg.cv, 'spreg_ics',
            "ICs for S\\&P500 Returns Regressed on FX Principal Components")

###############################################################################
# 4)
###############################################################################
spReg.fx <- gamlr(fx, sp$sp500)
spReg.fx.cv <- cv.gamlr(fx, sp$sp500, verb=T)
plot(spReg.fx.cv)
PlotICFunctions(spReg.fx, spReg.fx.cv, 'spregfx_ics')
PlotICs(spReg.fx, spReg.fx.cv, 'spregfx')
SaveICTable(spReg.fx, spReg.fx.cv, 'spregfx_ics',
            "ICs for S\\&P500 Returns Regressed on FX Movements")

# TODO(wclark3) -- lasso with PCs and covariates


###############################################################################
# Bonus)
###############################################################################

