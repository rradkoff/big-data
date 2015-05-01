rm(list = ls())

source('../utils/source_me.R', chdir = T)
OutputToFile = T
plotOpts$Prefix = "writeup/"

library(stringr)

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

# The main objective in these transformations is to achieve linear relationships 
# with the dependent variable (or, really, with its logit)
# dependent variable = loan
# independent variable = degree


################################################################################
# Q2: Model & Predict Degree
################################################################################
require(gamlr)

# Add a NA factor to each, and make the base NA
hh <- naref(hh)

# Do we need to remove loan from the model matrix?  I think we might.
hhNoLoan <- hh[,-which(colnames(hh) %in% c('loan'))]

# Create the sparse model matrix interacting everything with everything
# Is this what we want to do here?
x <- sparse.model.matrix(~.^2, data=hhNoLoan)[,-1]
print(dim(x)) # it's big

# Create a model for the log(degree+1) based on the model matrix
d <- log(degree + 1)
treat <- gamlr(x, d)
treat.cv <- cv.gamlr(x, d, verb=TRUE)

SaveICTable(treat, treat.cv, "tab:treat_ic", "Treatment IC Table", "treat_ic")
PlotICs(treat, treat.cv, "treat_aic")

# Predict dhat
dhat <- predict(treat, x, type = "response")

# Plot d vs dhat
dhatCor <- cor(drop(dhat), d)
PlotSetup('d_vs_dhat')
plot(dhat, d, bty = "n", pch = 21, bg = 'gray',
     main = 'd vs. dhat')
legend("bottomright", bty = 'y', pch = 21, pt.bg = 'gray',
       legend = sprintf('cor(d, dhat) = %4.2f', dhatCor))
PlotDone()
print(sprintf("cor(d, dhat) = %f", dhatCor))

################################################################################
# Addendum: see how errors are distributed 
################################################################################
eps <- d - dhat
hist(as.matrix(eps), breaks=50,
     main="Errors of X on log(1+degree)",
     xlab="Regression Residual", ylab="Frequency")

treat_log <- gamlr(x, degree)
dhat_log <- predict(treat_log, x, type = "response")
eps_log <- degree - dhat_log
plot(as.matrix(eps_log) ~ x)
hist(as.matrix(eps_log), breaks=50,
     main="Errors of X on degree",
     xlab="Regression Residual", ylab="Frequency")

################################################################################
# Q3: Predict estimator for d on loan
################################################################################

colnames(dhat) <- c("dhat")
causal <- gamlr(cBind(d, dhat, x), hh$loan, free = 2, type = "binary")
causal.cv <- cv.gamlr(cBind(d, dhat, x), hh$loan, free = 2, type = "binary")

SaveICTable(causal, causal.cv, "tab:causal_ic", "Causal IC Table", "causal_ic")
PlotICs(causal, causal.cv, "causal_aic")

print(sprintf("coef(causal)['d']=%f", coef(causal)["d",]))
print(sprintf("odds increase by %f percent", 100*(exp(coef(causal)["d",])-1) ))

## see which are the significant predictors
AICc <- coef(causal)[,1]
AICc <- AICc[AICc!=0]
AICc <- sort(AICc, decreasing=TRUE)
print("5 most positive predictors")
print(head(AICc, n=5))
print("5 most negative predictors")
print(tail(AICc, n=5))

pospred <- head(AICc, n=5)
tab_pos = cbind(str_replace(colnames(x)[match(names(pospred), colnames(x))], "_", " "),
                sprintf(pospred, fmt='%#.4f'), 
                sprintf(100*(exp(pospred)-1), fmt='%#.4f'), 
                sprintf(colSums(x[,match(names(pospred), colnames(x))]), fmt='%i'))
colnames(tab_pos) <- c("$x$", "$\\beta_j$", "odds multiplier", "n")
print(xtable(tab_pos, label="tab:pos", 
             caption="Most Significant Positive Predictors"),
      sanitize.text.function=function(x){x}, file=GetFilename('pos.tex'))

negpred <- tail(AICc, n=5)
tab_neg = cbind(str_replace(colnames(x)[match(names(negpred), colnames(x))], "_", " "), 
                sprintf(negpred, fmt='%#.4f'), 
                sprintf(100*(exp(negpred)-1), fmt='%#.4f'),
                sprintf(colSums(x[,match(names(negpred), colnames(x))]), fmt='%i'))
colnames(tab_neg) <- c("$x$", "$\\beta_j$", "odds multiplier", "n")
print(xtable(tab_neg, label="tab:neg", 
             caption="Most Significant Negative Predictors"),
      sanitize.text.function=function(x){x}, file=GetFilename('neg.tex'))

## gut check
# dim(hh[hh$village==4&hh$roof=="thatch",])

################################################################################
# Q4: Naive lasso
################################################################################

naive <- gamlr(cBind(d, x), hh$loan, type = "binary")
naive.cv <- cv.gamlr(cBind(d, x), hh$loan, type = "binary")

SaveICTable(naive, naive.cv, "tab:naive_ic", "Naive IC Table", "naive_ic")
PlotICs(naive, naive.cv, "naive_aic")

print(sprintf("coef(naive)['d']=%f", coef(naive)["d",]))

################################################################################
# Q5: Bootstrapping
################################################################################

gamma <- c();
n <- nrow(hh)
numBootstrap <- 100
cat(sprintf('bootstrap(%d) ', numBootstrap))
for (b in 1:numBootstrap) {
  ib <- sample(1:n,n,replace=TRUE)
  
  # Sample x and d
  xb <- x[ib,]
  db <- d[ib]
  
  # Predict dhat for sample
  treatb <- gamlr(xb, db)
  dhatb <- predict(treat, xb, type = 'response')
  colnames(dhatb) <- c('dhat')
  
  # Now regress loan on dhat, d, and x
  loanb <- hh$loan[ib]
  fb <- gamlr(cBind(db, dhatb, xb), loanb, free = 2, type = "binary")
  gamma <- c(gamma,coef(fb)["db",])
  
  cat(sprintf("%d,", b))
}
cat("done.\n")
PlotSetup('bootstrap_hist')
hist(gamma, breaks=10);
abline(v=coef(causal)["d",], col=2)
PlotDone()
summary(gamma)
print(sprintf("gamma mean=%f, std-dev=%f", mean(gamma), sqrt(var(gamma))))

#
# Piazza questions:
# 1) Why do we need to bootstrap with n observations?
# 2) How do we know when to create a model matrix that interacts
#    all covariates with each other?
# 3) Is there an optimal n:p ratio (p is the number of x's in the model matrix)
# 4) Does loan == 0 mean that someone (1) applied for a loan and was rejected,
#    (2) they didn't apply for a loan, or, (3) either/both.
#
