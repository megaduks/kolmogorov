# function for generating a given number of graphs from four popular classes
#
# params:
#   n.graphs - the desired number of graphs to be generated
#   n.nodes  - the number of vertices in each generated graph

generate.test.graphs <- function(n.graphs, n.nodes) {
  
  require(igraph)

  num.graphs <- n.graphs
  num.nodes <- n.nodes
  main.parameter.index <- sample(1:100, num.graphs, replace = TRUE)

  edge.probability     <- seq(0.01, 0.1, length.out = 100) 
  rewiring.probability <- seq(0.001, 0.05, length.out = 100)
  alpha.coefficients   <- seq(1.0, 3.0, length.out = 100)
  forward.burning      <- seq(0.01, 1, length.out = 100)

  dice.roll <- sample(1:4, num.graphs, replace = TRUE)

  graphs <- list()

  for (i in 1:num.graphs) {
    switch(dice.roll[i],
         g <- sample_gnp(n = num.nodes, p = edge.probability[main.parameter.index[i]], directed = FALSE, loops = FALSE),
         g <- sample_smallworld(dim = 1, nei = 2, size = num.nodes, p = rewiring.probability[main.parameter.index[i]], loops = FALSE),
         g <- sample_pa(n = num.nodes, power = alpha.coefficients[main.parameter.index[i]], directed = FALSE)
    )
    E(g)$weight <- runif(ecount(g))
    
    graphs[[i]] <- g
  }
  
  graphs$type <- dice.roll
  graphs$parameter <- main.parameter.index
  
  graphs
}
