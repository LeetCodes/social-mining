library(networkD3)

### Load Data

graph.edges <- read.csv(file = 'data/facebook/0.edges',
                  sep=" ", header=F,
                  col.names=c("source", "target"))
graph.edges <- graph.edges - min(graph.edges) + 1
graph.nodes <- data.frame(id=seq(max(graph.edges) - min(graph.edges) + 1))
graph.nodes$group <- 1

### Create graph 

# network <- forceNetwork(
#   Links = graph.edges - 1, Nodes = graph.nodes,
#   Source = "source", Target = "target",
#   NodeID = "id",
#   Group = "group", opacity = 0.8)
# 
# print(network)

### Cluster with MCL

library(MCL)
graph.cluster <- function(nodes, edges){
  n <- nrow(graph.nodes)
  adjacency <- matrix(0, n, n)
  index <- cbind(
    graph.edges$source, graph.edges$target)
  adjacency[index] <- 1
  cluster <- mcl(
    x = adjacency, addLoops=TRUE, ESM = TRUE)
  print(cluster$Cluster)
  cluster$Cluster
}


### Create graph of clustered ego network
graph.nodes$group <- graph.cluster(graph.nodes, graph.edges)

## draw clustered network
## notice that with NetworkD3, index of edges start from 0
network.cluster <- forceNetwork(
  Links = graph.edges - 1, Nodes = graph.nodes,
  Source = "source", Target = "target",
  NodeID = "id",
  Group = "group", opacity = 0.8)
print(network.cluster)
