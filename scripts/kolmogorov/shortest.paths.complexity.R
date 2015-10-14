library(acss)
library(igraph)
library(ggplot2)
library(dplyr)

num.nodes <- 100

g.erdos.renyi <- sample_gnp(num.nodes, 0.05)
g.small.world <- sample_smallworld(1, num.nodes, 4, 0.05)
g.preferential.attachment <- sample_pa(num.nodes, 1.5)

g <- g.preferential.attachment
name <- "pa"

# compute all the shortest paths between all pairs of vertices
v.shortest.paths <- distances(g)

# compute the degrees of all vertices
v.degrees <- degree(g)

# sort the vertices accordingly to the vertex degree
v.sorted.degrees <- cbind(node = 1:100, deg = v.degrees)
v.sorted.degrees <- v.sorted.degrees[order(v.sorted.degrees[,2], decreasing = T), ]

# sort the shortest path distances by the decreasing degree of each node
# i.e. for each node sort the list of shortest paths so that the first
# distance is to the highest degree node, second distance is to the second highest
# degree node, etc.

# sort shortest paths accordingly to the vertex degree
v.sorted.shortest.paths <- numeric(0)

means <- numeric(0)
std.devs <- numeric(0)

for (i in 1:nrow(v.shortest.paths))
  v.sorted.shortest.paths <- rbind(v.shortest.paths, (v.shortest.paths[i,])[v.sorted.degrees[,1]])

distance.string <- numeric(0)

for (j in 1:num.nodes) {
  
  distance.string <- v.sorted.shortest.paths[j,]
  distance.string <- LETTERS[distance.string+1]
  distance.string <- paste(distance.string, collapse = '')
  distance.string <- gsub("NA", "x", distance.string)
  
  distance.string.chunks <- substring(distance.string, seq(1, nchar(distance.string), 10), seq(10, nchar(distance.string), 10))
  
  # look-up Kolmogorov Complexity of each chunk of length 10
    
  kolmogorov.complexities <- numeric(0)
  
  for (i in 1:length(distance.string.chunks))
    kolmogorov.complexities[i] <- acss(distance.string.chunks[i])[1]
    
  means[j] <- mean(kolmogorov.complexities, na.rm = TRUE)
  std.devs[j] <- sd(kolmogorov.complexities, na.rm = TRUE)
}

# plot the results
#
# for each network we need three plots:
# - distance vs kolmogorov (point)
# - degree vs distance (boxplot)
# - degree vs kolmogorov (boxplot)

v.shortest.paths[is.infinite(v.shortest.paths)] <- NA
df <- data.frame(degree = v.degrees, distance = rowMeans(v.shortest.paths, na.rm = TRUE), kolmogorov = means, sd = std.devs)
plot <- ggplot(data = df, aes(x, y)) + geom_point(aes(distance, kolmogorov)) + xlab("mean shortest path distance") + ylab("mean Kolmogorov complexity")
ggsave(filename = paste(name, ".paths.distance.kolmogorov.png", sep = ''), plot)

plot <- ggplot(data = df, aes(x, y)) + geom_boxplot(aes(as.factor(degree), kolmogorov)) + xlab("degree") + ylab("Kolmogorov complexity")
ggsave(filename = paste(name, ".paths.degree.kolmogorov.png", sep = ''), plot)

plot <- ggplot(data = df, aes(x, y)) + geom_boxplot(aes(as.factor(degree), distance)) + xlab("degree") + ylab("shortest path distance")
ggsave(filename = paste(name, ".paths.degree.distance.png", sep = ''), plot)
