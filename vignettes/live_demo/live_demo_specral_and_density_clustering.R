# LIVE DEMONSTRATION

library(clustering)

# generate sample data
set.seed(188)
n <- 50
x <- cbind(
  x = runif(5, 0, 2) + rnorm(n, sd = 0.05),
  y = runif(5, 0, 2) + rnorm(n, sd = 0.05)
)
x <- t(x)
d <- c(x, .6, .75, .8, 1.3, 0, 1.6, .6, 1, .9, .9, .5, 1.5, 1, .4, .1, .6);
x <- matrix(d, nrow=2);
t <- generate_2d_cluster(100)
t <- c(t, generate_2d_cluster(100, c(2,2)))
t <- matrix(t, nrow=2)

two_clusters <- t;
four_clusters <- x;

par(mfrow=c(3,2), mar=c(3,3,2,1))

## Start

# math utility

x <- c(1,2,3)
y <- c(4,5,6)

# lots of norms, e.g.
euclidean_norm(x)

# distance function, e.g.
euclidean_distance(x,y)

# function factories, e.g.
p8_norm <- p_norm_factory(8)
p8_norm(x)

infty_distance <- induced_metric_factory(infinity_norm)
infty_distance(x,y)

# general utility
single_cluster <- generate_2d_cluster(100)
plot(single_cluster[1,], single_cluster[2,], pch=20, cex=.5)

nested_clusters <- generate_nested_2d_training_data(400);
plot(nested_clusters[1,], nested_clusters[2,], pch=20, cex=.5)

# Proximity-based clustering algorithms

plot(single_cluster[1,], single_cluster[2,])
# DBSCAN
dbscan_c <- DBSCAN(single_cluster, .2, 10)
plot_clustered_2d_data(dbscan_c)

# OPTICS
opt <- OPTICS(single_cluster, .2, 10)
# Reachability Plot
reachability_plot(opt)

# Extract DBSCAN from OPTICS data
extr_dbscan <- extract_DBSCAN_clustering(opt)
# extra information can be used to connect points with their predecessors
plot_clustered_2d_data(extr_dbscan, connect_to_predecessor = TRUE)

# spectral clustering
