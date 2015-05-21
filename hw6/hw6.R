##
## housekeeping
##

rm(list = ls())
source('../utils/source_me.R', chdir = T)
OutputToFile = F
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

clusterSizes <- c(2, 3, 4, seq(5, 35, 5))
kmGroups = vector("list", length(clusterSizes))
ics <- data.frame(k=clusterSizes,
                  aicc=rep(NA, length(clusterSizes)),
                  bic=rep(NA, length(clusterSizes))) 
for (i in 1:length(clusterSizes)) {
  k <- clusterSizes[i]
  print( paste("k-means with", k, "centers...") )
  tic()
  ## run k-means
  grp <- kmeans(x=counts109.x, centers=k, nstart=10) # x=congress109Counts
  toc()
  ## keep for model selection
  kmGroups[[i]] <- grp
  ics$aicc[i] <- kIC(grp, "A")
  ics$bic[i] <- kIC(grp, "B")
}

kmK <- clusterSizes[which.min(ics$bic)]
print( paste("BIC selects model with", kmK, "clusters") )

## look at most common words in each cluster
g <- kmGroups[[which.min(ics$bic)]]
print(apply(g$centers,1,function(c) colnames(counts109.x)[order(-c)[1:10]]))

## plot results
ylimits=c(min(ics$aicc/1000, ics$bic/1000), max(ics$aicc/1000, ics$bic/1000))
PlotSetup('kmeans_ic_plot')
plot(clusterSizes, ics$aicc/1000, type="o", col="red", pch=19, main="Model Selection", 
     xlab="# of Clusters", ylab="Information Criteria (thousands)",
     ylim=ylimits)
lines(clusterSizes, ics$bic/1000, type="o", col="blue", pch=19)
legend("bottomleft", c("AICc", "BIC"), lty=1, pch=19, col=c("red", "blue"), bty="n")
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

summarizeClusters <- function(km) {
  sumIdeology <- function(km, party, k) {
    classKReps <- names(km$cluster[km$cluster==k])
    sum(congress109Ideology[classKReps,]$party==party)
  }
  
  numD <- sapply(1:length(km$size), function(k) { return(sumIdeology(km, "D", k)) })
  numR <- sapply(1:length(km$size), function(k) { return(sumIdeology(km, "R", k)) })
  numI <- sapply(1:length(km$size), function(k) { return(sumIdeology(km, "I", k)) })
  num <- sapply(1:length(km$size), function(k) { return(sum(km$cluster == k)) })
  meanRepshare <- sapply(1:length(km$size), function(k) {
    classKReps <- names(km$cluster[km$cluster==k])
    return(mean(congress109Ideology[classKReps,]$repshare))
  })
  return(data.frame(cluster=1:length(km$size), numD, numI, numR, num, meanRepshare))
}

## tabulate party membership by K-means cluster.
clust_party <- summarizeClusters(g)
idx <- which.min(abs(clust_party$numD / clust_party$num - 0.5))
non_partisan_cluster <- clust_party[idx, 'cluster']

non_partisan_words <- as.data.frame(
  sort(g$centers[non_partisan_cluster,], decreasing = T)[1:10])
colnames(non_partisan_words) = c('relative_frequency')

# 2-means cluster
print(summarizeClusters(kmGroups[[which(clusterSizes == 2)]]))
