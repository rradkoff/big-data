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
require(gplots)
PlotSetup('heatmap')
# Cannot figure out how to make it square...what a pain!
gplots::heatmap.2(cor(fx), symm = T, trace="none", density.info="none", margins = c(6,6))
PlotDone()

###############################################################################
# 2)
###############################################################################
# TODO(mdelio) should we be transposing - evidence says yes, logic says no
pca <- prcomp(fx, scale = T)

PlotSetup('screeplot')
plot(pca, main="Scree Plot of Principal Component Analysis", 
     xlab="Principal Components")
PlotDone()

zpca <- predict(pca)

print(round(pca$rotation, digits=2))

## Look for pattern in PC1
print(round(sort(pca$rotation[,"PC1"]), digits=2))

## plot pegged vs floating currencies
pegs <- append(rep(0, 19), rep(1, 4))
colors <- c("red","blue")[pegs+1]
pc1 <- sort(pca$rotation[,"PC1"])
PlotSetup("pc1_pegs")
plot(pc1, col=colors, pch=19, main="Principal Component 1", 
     xlab="Country", ylab="Rotation", xaxt="n")
axis(1, at=1:23, labels=names(pc1))
PlotDone()

# I'm confused if we want to scale the columns or the rows (I think the columns?)
# Now I think the rows, so the columns of the transposed fx.
Km <- 4
fx.km <- kmeans(scale(fx), centers = Km)

# TODO(mdelio) - add xlims so that text doesn't get chopped
PlotSetup('pca1_2')
plot(zpca[,1:2], type="n", xlim = c(-20, 10))
text(x=zpca[,1], y=zpca[,2], labels=rownames(fx),
     col=rainbow(Km, alpha=0.7)[fx.km$cluster])
PlotDone()

PlotSetup('pca3_4')
plot(zpca[,3:4], type="n", xlim = c(-6, 4))
text(x=zpca[,3], y=zpca[,4], labels=rownames(fx),
     col=rainbow(Km, alpha=0.7)[fx.km$cluster])
PlotDone()

##
## look under the hood of PCA
##

cc <- cov(scale(fx))
eig <- eigen(cc)
barplot(sort(eig$values,decreasing=T)[1:10])

###############################################################################
# 3)
###############################################################################
# Is this what he means by "glm on first K"? Or does he mean glm on the first
# cluster of currencies, or something else?

# spReg.glm <- glm(sp$sp500 ~ pca$x[,"PC1"], data=df1)
spReg.glm <- glm(sp$sp500 ~ zpca[,"PC1"])
1-spReg.glm$deviance/spReg.glm$null.deviance ## R2 = 16 percent

require(gamlr)
spReg <- gamlr(pca$x, sp$sp500)
spReg.cv <- cv.gamlr(pca$x, sp$sp500, verb=T, select="1se")
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

