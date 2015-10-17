# Measure the entropy of various graphs generated from the Erdos-Renyi random graph model
#

# load required libraries

library(igraph)
library(ggplot2)
library(acss)

# universal graph parameters

num.graphs <- 50
num.nodes  <- 250

# graph parameters specific to a given network class

edge.probability     <- seq(0.01, 0.1, length.out = 100) 
rewiring.probability <- seq(0.001, 0.05, length.out = 100)
alpha.coefficients   <- seq(1.0, 3.0, length.out = 100)
forward.burning      <- seq(0.01, 1, length.out = 100)

graph.class.parameter <- forward.burning
graph.class.name <- "forest.fire"                     # "small.world" "preferential.attachment" "forest.fire"
graph.class.parameter.name <- "forward burning"       # "rewiring probability" "alpha coefficient" "forward burning"

# variables to store means and standard deviations of all variables

vertex.entropy <- numeric(0)
degree.entropy <- numeric(0)
betweenness.entropy <- numeric(0)
clustering.coefficient.entropy <- numeric(0)

sd.vertex.entropy <- numeric(0)
sd.degree.entropy <- numeric(0)
sd.betweenness.entropy <- numeric(0)
sd.clustering.coefficient.entropy <- numeric(0)

d.degree <- numeric(0)
d.betweenness <- numeric(0)
d.clustering.coefficient <- numeric(0)

mean.degree <- numeric(0)
mean.betweenness <- numeric(0)
mean.clustering.coefficient <- numeric(0)

sd.degree <- numeric(0)
sd.betweenness <- numeric(0)
sd.clustering.coefficient <- numeric(0)

df <- data.frame(graph.class.parameter=NA, 
                 vertex.entropy=NA, degree.entropy=NA, betweenness.entropy=NA, clustering.coefficient.entropy=NA,
                 sd.vertex.entropy=NA, sd.degree.entropy=NA, sd.betweenness.entropy=NA, sd.clustering.coefficient.entropy=NA,
                 d.degree=NA, d.betweenness=NA, d.clustering.coefficient=NA, 
                 sd.degree=NA, sd.betweenness=NA, sd.clustering.coefficient=NA)[numeric(0),]

# create num.graphs realizations of the random graph model given a particular value of the edge creation probability
for (k in 1:length(graph.class.parameter)) {
  
  #graphs <- lapply(1:num.graphs, sample_gnp, n = num.nodes, p = graph.class.parameter[k])
  #graphs <- lapply(1:num.graphs, sample_smallworld, dim = 1, size = num.nodes, nei = 4, p = graph.class.parameter[k])
  #graphs <- lapply(1:num.graphs, sample_pa, n = num.nodes, power = graph.class.parameter[k])
  graphs <- lapply(1:num.graphs, sample_forestfire, nodes = num.nodes, fw.prob = graph.class.parameter[k], bw.factor = 0.9, directed = FALSE)
  
  # set uniform weights of edges
  for (i in 1:num.graphs) 
    E(graphs[[i]])$weight <- runif(ecount(graphs[[i]]))
  
  # compute the vertex entropy distribution 

  vertex.entropy <- lapply(graphs, diversity)
  
  # remove the "NaN" and "Inf" points from entropy distributions
  
  for (i in 1:num.graphs) {
    vertex.entropy[[i]] <- vertex.entropy[[i]][!is.nan(vertex.entropy[[i]])]
    vertex.entropy[[i]] <- vertex.entropy[[i]][!is.infinite(vertex.entropy[[i]])]
  }
  
  mean.vertex.entropy <- mean(unlist(vertex.entropy))
  sd.vertex.entropy   <- sd(unlist(vertex.entropy))

  # compute the entropy of the degree distribution
  
  d.degree      <- lapply(graphs, degree)
  s.degree      <- lapply(d.degree, paste, collapse = "")
  degree.entropy <- lapply(s.degree, entropy)

  mean.degree.entropy <- mean(unlist(degree.entropy))
  sd.degree.entropy   <- sd(unlist(degree.entropy))

  # compute the entropy of the betweenness distribution
  
  d.betweenness       <- lapply(graphs, betweenness)
  s.betweenness       <- lapply(d.betweenness, paste, collapse = "")
  betweenness.entropy <- lapply(s.betweenness, entropy)
  
  mean.betweenness.entropy <- mean(unlist(betweenness.entropy))
  sd.betweenness.entropy   <- sd(unlist(betweenness.entropy))

  # compute the entropy of the clustering coefficient distribution
  
  d.clustering.coefficient <- lapply(graphs, transitivity, type = "local", isolates = "zero")
  s.clustering.coefficient <- lapply(d.clustering.coefficient, paste, collapse = "")
  clustering.coefficient.entropy <- lapply(s.clustering.coefficient, entropy)
  
  mean.clustering.coefficient.entropy <- mean(unlist(clustering.coefficient.entropy))
  sd.clustering.coefficient.entropy   <- sd(unlist(clustering.coefficient.entropy))
  
  # compute the mean of centrality measures for each graph
  
  mean.degree <- mean(unlist(lapply(d.degree, mean)))
  mean.betweenness <- mean(unlist(lapply(d.betweenness, mean)))
  mean.clustering.coefficient <- mean(unlist(lapply(d.clustering.coefficient, mean)))
  
  sd.degree <- mean(unlist(lapply(d.degree, sd)))
  sd.betweenness <- mean(unlist(lapply(d.betweenness, sd)))
  sd.clustering.coefficient <- mean(unlist(lapply(d.clustering.coefficient, sd)))
  
  # store all computed statistics in the data frame
  
  df[k,] <- c(graph.class.parameter[k], 
             mean.vertex.entropy, mean.degree.entropy, mean.betweenness.entropy, mean.clustering.coefficient.entropy, 
             sd.vertex.entropy, sd.degree.entropy, sd.betweenness.entropy, sd.clustering.coefficient.entropy, 
             mean.degree, mean.betweenness, mean.clustering.coefficient, 
             sd.degree, sd.betweenness, sd.clustering.coefficient)
}

  df$graph.class.name <- graph.class.name

# plot the entropies of centrality measures and centrality measures

p.entropy <- ggplot(df, aes(x = graph.class.parameter, y = value, color = variable)) +
  geom_line(aes(y = degree.entropy, color = "degree entropy")) +
  geom_line(aes(y = betweenness.entropy, color = "betweenness entropy")) +
  geom_line(aes(y = vertex.entropy, color = "vertex entropy")) +
  geom_line(aes(y = clustering.coefficient.entropy, color = "clustering coefficient entropy")) +
  xlab(graph.class.parameter.name)

p.centrality <- ggplot(df, aes(x = edge.probability, y = value, color = variable)) +
  geom_line(aes(y = d.degree, color = "degree")) +
  geom_line(aes(y = d.betweenness, color = "betweenness")) +
  geom_line(aes(y = d.clustering.coefficient, color = "clustering coefficient")) +
  xlab(graph.class.parameter.name)

# filename structure: graph class, entropy|centrality, n, number of vertices: random.graph.entropy.n.50.png
file.name <- paste("figures/", graph.class.name, ".entropy.n", num.nodes, ".png", sep = "")
ggsave(p.entropy, filename = file.name)
file.name <- paste("figures/", graph.class.name, ".centrality.n", num.nodes, ".png", sep = "")
ggsave(p.centrality, filename = file.name)

# save results to a csv file
file.name <- paste("data/", graph.class.name, ".n", num.nodes, ".csv", sep = "")
write.table(df, file = file.name, sep = ",", row.names = FALSE, col.names = TRUE)
