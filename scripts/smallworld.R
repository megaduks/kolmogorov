# Measure the entropy of various graphs generated from the Watts-Strogatz small world graph model
#

library(igraph)
library(ggplot2)

# generate a set of random graphs for a range of edge probabilities

num.graphs <- 50
num.nodes <- 200
rewiring.probability <- seq(0.001, 0.05, length.out = 100)

mu <- numeric(0)
sd <- numeric(0)

centrality.entropies <- data.frame(rewiring.probability = numeric(0), degree.entropy = numeric(0), betweenness.entropy = numeric(0))

for (k in 1:length(rewiring.probability)) {
  
graphs <- lapply(1:num.graphs, sample_pa, n = num.nodes, power = rewiring.probability[k])

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

# compute the entropy of the degree distribution

d.degrees <- lapply(graphs, degree)
s.degrees <- lapply(d.degrees, paste, collapse = "")
degree.entropy <- lapply(s.degrees, entropy)

mean.degree.entropy <- mean(unlist(degree.entropy))

# compute the entropy of the betweenness distribution

d.betweenness <- lapply(graphs, betweenness)
s.betweenness <- lapply(d.betweenness, paste, collapse = "")
betweenness.entropy <- lapply(s.betweenness, entropy)

mean.betweenness.entropy <- mean(unlist(betweenness.entropy))

# remove the "NaN" and "Inf" points from entropy distributions

for (i in 1:num.graphs) {
  entropy.distributions[[i]] <- entropy.distributions[[i]][!is.nan(entropy.distributions[[i]])]
  entropy.distributions[[i]] <- entropy.distributions[[i]][!is.infinite(entropy.distributions[[i]])]
}

# compute the mean entropy for a given class of random graphs

mean.entropies <- unlist(lapply(entropy.distributions, mean))
mu[k] <- mean(mean.entropies)
sd[k] <- sd(mean.entropies)

centrality.entropies[k,] <- c(rewiring.probability[k], mean.degree.entropy, mean.betweenness.entropy)

# draw the histogram of mean entropies

# df <- data.frame(me = mean.entropies)
# plot <- ggplot(df, aes(x = me)) + 
#   geom_histogram(binwidth = 0.002, colour = "black", fill = "white") + 
#   geom_density() + 
#   geom_vline(aes(xintercept = mu), colour = "red")
  
# save the histogram
# ggsave(filename = "er.entropy.png")

}

# plot the resulting distribution of mean entropies and their standard deviations

# df <- data.frame(rp = rewiring.probability, mu, sd)
# plot <- ggplot(df, aes(x = rp, y = value, color = variable)) +
#   geom_line(aes(y = mu, color = "mean entropy")) +
#   geom_line(aes(y = sd, color = "standard deviation")) +
#   xlab("rewiring probability")
# ggsave(filename = "smallworld.entropy.png", plot = plot)

p <- ggplot(centrality.entropies, aes(x = rewiring.probability, y = entropy, color = variable)) +
  geom_line(aes(y = degree.entropy, color = "degree entropy")) +
  geom_line(aes(y = betweenness.entropy, color = "betweenness entropy")) +
  xlab("rewiring probability")
ggsave(filename = "figures/smallworld.centrality.entropy.png")