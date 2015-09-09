# Measure the entropy of various graphs generated from the Erdos-Renyi random graph model
#

library(igraph)
library(ggplot2)

# generate a set of random graphs

num.graphs <- 1000
num.nodes <- 200
edge.probability <- 0.01

graphs <- lapply(1:num.graphs, sample_gnp, n = num.nodes, p = edge.probability)

# select random edge weights from a uniform distribution

for (i in 1:num.graphs) {
  # get the number of edges in the current graph
  num.edges <- ecount(graphs[[i]])
  # sample weights from the uniform distribution
  weights <- runif(num.edges)
  # assign sampled weights to edges in the current graph
  E(graphs[[i]])$weight <- weights
}
  
# compute the entropy distribution for all graphs

entropy.distributions <- lapply(graphs, diversity)

# remove the "NaN" and "Inf" points from entropy distributions

for (i in 1:num.graphs) {
  entropy.distributions[[i]] <- entropy.distributions[[i]][!is.nan(entropy.distributions[[i]])]
  entropy.distributions[[i]] <- entropy.distributions[[i]][!is.infinite(entropy.distributions[[i]])]
}

# compute the mean entropy for a given class of random graphs

mean.entropies <- unlist(lapply(entropy.distributions, mean))
mu <- mean(mean.entropies)
sd <- sd(mean.entropies)

# draw the histogram of mean entropies

df <- data.frame(me = mean.entropies)
plot <- ggplot(df, aes(x = me)) + 
  geom_histogram(binwidth = 0.002, colour = "black", fill = "white") + 
  geom_density() + 
  geom_vline(aes(xintercept = mu), colour = "red")
  
plot

# save the histogram

ggsave(filename = "er.entropy.png")
