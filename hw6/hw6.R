##
## housekeeping
##

rm(list = ls())
source('../utils/source_me.R', chdir = T)
OutputToFile = T
plotOpts$Prefix = "output/"

library(textir)
library(tictoc)
library(maptpx)

## get data from 109th Congress
## note: Jan 05 to Jan 2007
data(congress109)

##
## Q1
## Fit K-means to speech text for K in 5,10,15,20,25.
## Use BIC to choose the K and interpret the selected model.
##

## standardize to mu=0; sigma=1
counts109.x <- scale(congress109Counts)
apply(counts109.x, 2, sd) # sd=1
apply(counts109.x, 2, mean) # mean=0

kmGroups = vector("list", 5)
ics <- data.frame(aicc=rep(NA, 5), bic=rep(NA, 5)) 
for (k in rep(1:5)*5) {
  print( paste("k-means with", k, "centers...") )
  tic()
  ## run k-means
  grp <- kmeans(x=counts109.x, centers=k, nstart=10) # x=congress109Counts
  toc()
  ## keep for model selection
  kmGroups[[k/5]] <- grp
  ics$aicc[k/5] <- kIC(grp, "A")
  ics$bic[k/5] <- kIC(grp, "B")
}

kmK <- which.min(ics$bic)*5
print( paste("BIC selects model with", kmK, "clusters") )

## look at most common words in each cluster
g<-kmeans(x=counts109.x, centers=kmK, nstart=10)
print(apply(g$centers,1,function(c) colnames(counts109.x)[order(-c)[1:10]]))

## plot results
PlotSetup('kmeans_ic_plot')
plot(ics$bic/1000, type="o", col="red", pch=19, main="Model Selection", 
     xlab="# of Clusters", ylab="Info. Criteria (thousands)", xaxt="n")
axis(1, at=rep(1:5), labels=rep(1:5)*5)     
par(new=TRUE)
plot(ics$aicc/1000, type="o", col="blue", pch=19, xaxt="n",yaxt="n",
     xlab="",ylab="")
legend("topright", c("BIC (L axis)", "AICc (R axis)"), lty=1, pch=19, col=c("red", "blue"), bty="n")
axis(4)
PlotDone()

##
## Q2
## Fit a topic model for the speech counts. Use Bayes factors to
## choose the number of topics, and interpret your chosen model.
##

## convert Matrix to `slam' simple_triplet_matrix
counts109.stm <- as.simple_triplet_matrix(congress109Counts) 

## chooses the number of topics using Bayes factor
tic()
## tol = 0.1 --> selects K=13
## tol = 0.01 --> selects K=12
tpcs <- topics(counts109.stm, K=(5:15), verb=TRUE, tol=0.1)
toc()
print( paste("Selected the K =", tpcs$K, "topic model") )

##
## Q3
## Connect the unsupervised clusters to partisanship
## -- Tabulate party membership by K-means cluster.
##    Are there any non-partisan topics?
## -- Fit topic regressions for each of party and repshare.
##    Compare to regression onto phrase percentages:
##

## tabulate party membership by K-means cluster.
clust_party <- data.frame(numD=rep(NA, kmK), numI=rep(NA, kmK), 
                          numR=rep(NA, kmK), meanRepshare=rep(NA, kmK))
clust_party$numD <- sapply(1:kmK, function(k) 
  sum(congress109Ideology[names(g$cluster[g$cluster==k]),]$party=="D"))
clust_party$numR <- sapply(1:kmK, function(k) 
  sum(congress109Ideology[names(g$cluster[g$cluster==k]),]$party=="R"))
clust_party$numI <- sapply(1:kmK, function(k) 
  sum(congress109Ideology[names(g$cluster[g$cluster==k]),]$party=="I"))
clust_party$meanRepshare <- sapply(1:kmK, function(k) 
  mean(congress109Ideology[names(g$cluster[g$cluster==k]),]$repshare))

## to do: see what happens if we force 2 clusters