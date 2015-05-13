## actors network example

library(igraph)

### GRAPH
## read in a graph in the `graphml' formal: xml for graphs.
## it warns about pre-specified ids, but we want this here
## (these ids match up with the castlists in movies.txt)
actnet <- read.graph("actors.graphml",format="graphml")

### TRANSACTION
## read in the table of actor ids for movies
## this is a bit complex, because the movie names
## contain all sorts of special characters.
movies <- read.table("movies.txt", sep="\t", 
                     row.names=1, as.is=TRUE, comment.char="", quote="")
## it's a 1 column matrix.  treat it like a vector
movies <- drop(as.matrix(movies))
## each element is a comma-separated set of actor ids.  
## use `strsplit' to break these out
movies <- strsplit(movies,",")
## and finally, match ids to names from actnet
casts <- lapply(movies, 
                function(m) V(actnet)$name[match(m,V(actnet)$id)])
## check it
casts['True Romance']
## format as arules transaction baskets
library(arules)
casttrans <- as(casts, "transactions")


## Set up STM information
castsize <- unlist(lapply(casts, function(m) length(m)))
## see ?rep.int: we're just repeating movie names for each cast member
acti <- factor(rep.int(names(casts),times=castsize))
## actors
actj <- factor(unlist(casts), levels=V(actnet)$name)
## format as STM (if you specify without `x', its binary 0/1)
actmat <- sparseMatrix(i=as.numeric(acti),j=as.numeric(actj),
                       dimnames=list(movie=levels(acti),actor=levels(actj)))

## count the number of appearences by actor
nroles <- colSums(actmat)
names(nroles) <- colnames(actmat)

##
## most common and connected actors
##

nroles <- nroles[order(nroles,decreasing=TRUE)]
print("most common actors")
print( head(nroles) )

degree <- degree(actnet)
degree <- degree[order(degree,decreasing=TRUE)]
print("most connected actors")
print( head(degree) )

## 
## pick 2 (random) actors and describe the shortest path between them
##

names <- colnames(actmat)
nactors <- dim(actmat)[2]
a1 <- sample(1:nactors, 1)
a2 <- sample(1:nactors, 1)
print( paste("two random actors:", names[a1], "and", names[a2]) )
common_movies <- c()

spath <- get.shortest.paths(actnet, from=a1, to=a2)$vpath[[1]]
print( paste("-- shortest path between", names[a1], "and", names[a2], "--") )
for ( k in 1:(length(spath)-1) ) {
  m1 <- seq_along(casts)[sapply(casts, FUN=function(X) names[spath[k]] %in% X)]
  m2 <- seq_along(casts)[sapply(casts, FUN=function(X) names[spath[k+1]] %in% X)]
#   common_movies[k] <- names(casts)[intersect(m1, m2)]
  print( paste(names[spath[k]], "was in", names(casts)[intersect(m1, m2)], 
               "with ", names[spath[k+1]]) ) 
}

##
## Find pairwise actor-cast association rules with at least 0.01% 
## support and 10% confidence
##

## There are none?
## Highest support is nroles[1]/nfilms = 0.004
## Change minimum support to 0.001

nfilms <- length(names(casts))
actrules <- apriori(casttrans, 
                      parameter=list(support=.01, confidence=.1))
