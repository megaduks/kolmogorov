# Verify the applicability of entropy to condensed representation of adjacency matrices for graphs

library(acss)
library(igraph)
library(compositions)
library(ggplot2)
library(dplyr)

# pre-compute Kolmogorov complexity of every binary string of length 10
# indexes are shifted by 1 so that null address c[0] is avoided

complexities <- numeric(0)

for (i in 0:1023)
  complexities[i+1] <- acss(binary(i), alphabet = 2)[1]

# generate a set of random graphs for a range of edge probabilities

num.graphs <- 100
num.nodes <- 200

edge.probability <- seq(0.001, 0.05, by = 0.001)

means <- numeric(0)
std.devs <- numeric(0)

# a function to sort an adjacency matrix
sort.matrix <- function(m) { m[do.call(order, lapply(1:ncol(m), function(x) m[,x])), ]}

for (k in 1:length(edge.probability)) {
  
  graphs <- lapply(1:num.graphs, sample_gnp, n = num.nodes, p = edge.probability[k])
  
  adjacency.matrices <- lapply(graphs, as_adjacency_matrix, type = "lower")
  adjacency.strings <- lapply(adjacency.matrices, paste, collapse = "")
  
  # sorted.adjacency.matrices <- lapply(adjacency.matrices, sort.matrix)
  # adjacency.strings <- lapply(sorted.adjacency.matrices, paste, collapse = '')
  
  local.means <- numeric(0)
  for (j in 1:num.graphs) {
    adjacency.string <- adjacency.strings[[j]]
    
    adjacency.string.chunks <- substring(adjacency.string, seq(1, nchar(adjacency.string), 10), seq(10, nchar(adjacency.string), 10))
    
    # look-up Kolmogorov Complexity of each chunk of length 10
    
    kolmogorov.complexities <- numeric(0)
    for (i in 1:length(adjacency.string.chunks))
      kolmogorov.complexities[i] <- complexities[unbinary(adjacency.string.chunks[i]) + 1]
    
    local.means[j] <- mean(kolmogorov.complexities)
  }
  
  means[k] <- mean(unlist(local.means))
  std.devs[k] <- sd(unlist(local.means)) 
}

# plot the resulting complexity of graphs 

d <- data.frame(x = edge.probability, y = means, z = std.devs)

limits <- aes(ymax = d$y + d$z, ymin = d$y - d$z, color = "grey")

plot <- ggplot(data = d, aes(x = x, y = y)) + geom_line() + geom_point(aes(size = 3, color = "red")) + 
  geom_errorbar(limits) + theme(legend.position = "none") + xlab("edge probability") + ylab("graph complexity")

ggsave("er.kolmogorov.png", plot = plot)
