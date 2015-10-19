# function to compute all parameters of a graph
#
# params:
#   g : graph for which parameters will be computed
#
# returns:
#   degree
#   betweenness
#   clustering.coefficient
#   vertex.entropy
#   degree.entropy
#   betweenness.entropy
#   clustering.coefficient.entropy

compute.graph.entropy <- function(g) {
  require("igraph")
  require("acss")
  
  d <- mean(degree(g))
  b <- mean(betweenness(g))
  cc <- mean(transitivity(g, type = "local"))
  
  ve <- mean(diversity(g))
  de <- unname(entropy(paste(degree(g), collapse = "")))
  be <- unname(entropy(paste(betweenness(g), collapse = "")))
  cce <- unname(entropy(paste(transitivity(g, type = "local"), collapse = "")))
  
  result <- list(degree = d, betweenness = b, clustering.coefficient = cc, 
              vertex.entropy = ve, degree.entropy = de, betweenness.entropy = be, clustering.coefficient.entropy = cce)
  result
}