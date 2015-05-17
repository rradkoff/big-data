##
## housekeeping
##

rm(list = ls())
source('../utils/source_me.R', chdir = T)
OutputToFile = T
plotOpts$Prefix = "output/"

library(textir)
library(tictoc)

data(congress109)

##
## Q1
## Fit K-means to speech text for K in 5,10,15,20,25.
## Use BIC to choose the K and interpret the selected model.
##

## standardize to mu=0; sigma=1
xcounts <- scale(congress109Counts)
apply(xcounts, 2, sd) # sd=1
apply(xcounts, 2, mean) # mean=0

kmGroups = vector("list", 5)
ics <- data.frame(aicc=rep(NA, 5), bic=rep(NA, 5)) 
for (k in rep(1:5)*5) {
  print( paste("k-means with", k, "centers...") )
  tic()
  ## run k-means
  grp <- kmeans(x=congress109Counts, centers=k, nstart=10)
  toc()
  ## keep for model selection
  kmGroups[[k/5]] <- grp
  ics$aicc[k/5] <- kIC(grp, "A")
  ics$bic[k/5] <- kIC(grp, "B")
}

## plot results
PlotSetup('kmeans_ic_plot')
plot(ics$aicc/1000, type="o", col="red", pch=19, main="Model Selection", 
     xlab="# of Clusters", ylab="Information Criteria (thousands)", xaxt="n")
axis(1, at=rep(1:5), labels=rep(1:5)*5)     
lines(ics$bic/1000, type="o", col="blue", pch=19)
legend("topright", c("AICc", "BIC"), lty=1, pch=19, col=c("red", "blue"), bty="n")
PlotDone()