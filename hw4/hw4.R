rm(list = ls())

source('../utils/source_me.R', chdir = T)

## microfinance network 
## data from BANERJEE, CHANDRASEKHAR, DUFLO, JACKSON 2012

## data on 8622 households
hh <- read.csv("microfi_households.csv", row.names="hh")
hh$village <- factor(hh$village)

## We'll kick off with a bunch of network stuff.
## This will be covered in more detail in lecture 6.
## get igraph off of CRAN if you don't have it
## install.packages("igraph")
## this is a tool for network analysis
## (see http://igraph.sourceforge.net/)
library(igraph)
edges <- read.table("microfi_edges.txt", colClasses="character")
## edges holds connections between the household ids
hhnet <- graph.edgelist(as.matrix(edges))
hhnet <- as.undirected(hhnet) # two-way connections.

## igraph is all about plotting.  
V(hhnet) ## our 8000+ household vertices
## Each vertex (node) has some attributes, and we can add more.
V(hhnet)$village <- as.character(hh[V(hhnet),'village'])
## we'll color them by village membership
vilcol <- rainbow(nlevels(hh$village))
names(vilcol) <- levels(hh$village)
V(hhnet)$color = vilcol[V(hhnet)$village]
## drop HH labels from plot
V(hhnet)$label=NA

# graph plots try to force distances proportional to connectivity
# imagine nodes connected by elastic bands that you are pulling apart
# The graphs can take a very long time, but I've found
# edge.curved=FALSE speeds things up a lot.  Not sure why.

## we'll use induced.subgraph and plot a couple villages 
village1 <- induced.subgraph(hhnet, v=which(V(hhnet)$village=="1"))
village33 <- induced.subgraph(hhnet, v=which(V(hhnet)$village=="33"))

# vertex.size=3 is small.  default is 15
plot(village1, vertex.size=3, edge.curved=FALSE)
plot(village33, vertex.size=3, edge.curved=FALSE)

######  now, on to your homework stuff

library(gamlr)

## match id's; I call these 'zebras' because they are like crosswalks
# zebras are actually indices into hhnet (this is somewhat like %in%), but in 
# the order of hh
# example: match(1:10, c(4,3,7,1))
# [1]  4 NA  2  1 NA NA  3 NA NA NA
zebra <- match(rownames(hh), V(hhnet)$name)
# there are some NAs in zebra

## calculate the `degree' of each hh: 
##  number of commerce/friend/family connections
degree <- degree(hhnet)[zebra]
names(degree) <- rownames(hh)
degree[is.na(degree)] <- 0 # unconnected houses, not in our graph

## if you run a full glm, it takes forever and is an overfit mess
# > summary(full <- glm(loan ~ degree + .^2, data=hh, family="binomial"))
# Warning messages:
# 1: glm.fit: algorithm did not converge 
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred 

################################################################################
# Q1: Transform Degree
################################################################################

# Add loan to the vertices
V(hhnet)$loan <- hh[V(hhnet), 'loan']
neighborsWithLoans <- unlist(lapply(get.adjlist(hhnet), function (neis) {
  sum(V(hhnet)[neis]$loan)
}))[zebra]
fractionWithLoan <- neighborsWithLoans/degree
names(fractionWithLoan) <- rownames(hh)
fractionWithLoan[is.na(fractionWithLoan)] <- 0

PlotSetup('degrees_frac_with_loan')
hist(fractionWithLoan)
PlotDone()

# Need to add one before taking log (3477 have zero fraction)
# print(sum(fractionWithLoan == 0))
PlotSetup('degrees_frac_with_loan_log')
hist(log(fractionWithLoan+1))
PlotDone()

PlotSetup('degrees')
hist(degree)
PlotDone()

# Need to add one before taking log (449 have zero degree)
# print(sum(degree == 0))
PlotSetup('degrees_log')
hist(log(degree+1))
PlotDone()

################################################################################
# Q2: Model & Predict Degree
################################################################################
require(gamlr)
RefactorBaseNA <- function(var) {
  return(factor(var, levels = c(NA, levels(var)), exclude = NULL))
}

# Add a NA factor to each, and make the base NA
hh$village <- RefactorBaseNA(hh$village)
hh$religion <- RefactorBaseNA(hh$religion)
hh$roof <- RefactorBaseNA(hh$roof)
hh$ownership <- RefactorBaseNA(hh$ownership)

#
# Do we need to remove loan from the model matrix?  I think we might.
#
hhNoLoan <- hh[,-which(colnames(hh) %in% c('loan'))]

# Create the sparse model matrix interacting everything with everything
# Is this what we want to do here?
x <- sparse.model.matrix(~.^2, data=hhNoLoan)[,-1]
print(dim(x)) # it's big

# Create a model for the log(degree+1) based on the model matrix
d <- log(degree + 1)
treat <- gamlr(x, d)
plot(treat)

# Predict dhat
dhat <- predict(treat, x, type = "response")
# What do we plot here?  I think this is what we want, but not sure.
plot(dhat, d, bty = "n", pch = 21, bg = 8)
print(sprintf("cor(d, dhat) = %f", cor(drop(dhat), d)))

################################################################################
# Q3: Predict estimator for d on loan
################################################################################
causal <- gamlr(cBind(d, dhat, x), hh$loan, free = 2, type = "binary")
plot(causal)
print(sprintf("coef(causal)['d']=%f", coef(causal)["d",]))
