# 
# A simple example showing the use of Kolmogorov Complexity to measure
# the unexpectedness of the adjacency matrix of a graph
#

library(igraph)
library(acss)

# create a random graph according to the Erdos-Renyi model

graph <- sample_gnp(10, 0.5)

# transform the graph into the adjacency matrix

adjacency.matrix <- as_adjacency_matrix(graph, type = 'lower')

# sort the matrix row-wise

sorted.adjacency.matrix <- adjacency.matrix[ do.call(order, lapply(1:ncol(adjacency.matrix), function(x) adjacency.matrix[, x])), ]

# change the matrix into a string

adjacency.string <- paste(sorted.adjacency.matrix, collapse = '')

# chop string into equal length chunks
# acss can compute Kolmogorov's Complexity only for short strings

adjacency.string.chunks <- substring(adjacency.string, seq(1, nchar(adjacency.string), 10), seq(10, nchar(adjacency.string), 10))

# compute Kolmogorov's Complexity for all the chunks from the adjacency matrix

kolmogorov.complexities <- lapply(adjacency.string.chunks, acss, alphabet = 2)

# plot Kolmogorov's Complexities of all the chunks

barplot(unlist(kolmogorov.complexities))

# compute the mean Kolmogorov's Complexity over all chunks

mean.kolmogorov.complexity <- lapply(unlist(kolmogorov.complexities)[1], mean)

mean.kolmogorov.complexity
