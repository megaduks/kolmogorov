# Measure the entropy of various graphs generated from the Watts-Strogatz small world graph model
#

library(igraph)
library(ggplot2)

# generate a set of random graphs for a range of edge probabilities

num.graphs <- 1000
num.nodes <- 200
rewiring.probability <- seq(0.001, 0.03, by = 0.001)

mu <- numeric(0)
sd <- numeric(0)

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

# remove the "NaN" and "Inf" points from entropy distributions

for (i in 1:num.graphs) {
  entropy.distributions[[i]] <- entropy.distributions[[i]][!is.nan(entropy.distributions[[i]])]
  entropy.distributions[[i]] <- entropy.distributions[[i]][!is.infinite(entropy.distributions[[i]])]
}

# compute the mean entropy for a given class of random graphs

mean.entropies <- unlist(lapply(entropy.distributions, mean))
mu[k] <- mean(mean.entropies)
sd[k] <- sd(mean.entropies)

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

df <- data.frame(rp = rewiring.probability, mu, sd)
plot <- ggplot(df, aes(x = rp, y = value, color = variable)) +
  geom_line(aes(y = mu, color = "mean entropy")) +
  geom_line(aes(y = sd, color = "standard deviation")) +
  xlab("rewiring probability")
ggsave(filename = "smallworld.entropy.png", plot = plot)
