library(clustering)


## math utility

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

## general utility
par(mfrow=c(2,2), mar=c(3,3,2,1))

single_cluster <- generate_2d_cluster(100)
plot(single_cluster[1,], single_cluster[2,], pch=20, cex=.5)

nested_clusters <- generate_nested_2d_training_data(400);
plot(nested_clusters[1,], nested_clusters[2,], pch=20, cex=.5)

### Proximity-based clustering algorithms

dev.off(); par(mfrow=c(2,2), mar=c(3,3,2,1))

plot(single_cluster[1,], single_cluster[2,])
## DBSCAN
dbscan_c <- DBSCAN(single_cluster, eps=.2, minPts=10)
plot_clustered_2d_data(dbscan_c)

## OPTICS
opt <- OPTICS(single_cluster, eps=.2, minPts=10)
# Reachability Plot
reachability_plot(opt)

# Extract DBSCAN from OPTICS data
extr_dbscan <- extract_DBSCAN_clustering(opt)
# extra information can be used to connect points with their predecessors
plot_clustered_2d_data(extr_dbscan, connect_to_predecessor = TRUE)

## spectral clustering

dev.off(); par(mfrow=c(3,2), mar=c(3,3,2,1))

plot(nested_clusters[1,], nested_clusters[2,], col=c("darkred", "darkblue")[attr(nested_clusters, "innerOrOuter")])

projections <- clustering::spectral_clustering(nested_clusters, k=2)
plot(projections[1,], projections[2,], col=c("darkred", "darkblue")[attr(nested_clusters, "innerOrOuter")])

clustered <- k_medoids(projections, 2)
plot_clustered_2d_data(clustered)

attr(nested_clusters, "cluster") <- attr(clustered, "cluster")
plot_clustered_2d_data(nested_clusters)

cData <- clustering::spectral_clustering(nested_clusters, k=2, mercer_kernel=gaussian_kernel_with_fixed_gamma(30), clustering=k_medoids, 2);
plot_clustered_2d_data(cData)


## Erroneous Input Handling

# incorrect input data layout
DBSCAN(1:100, .1, 5)
OPTICS(1:100, .1, 5)

# non-numeric or invalid minPts and epsilon values
DBSCAN(single_cluster, .2, 1000)
OPTICS(single_cluster, .2, 10000)
DBSCAN(single_cluster, -3, 10)
OPTICS(single_cluster, -3, 10)

# nonsensical projection dimension
spectral_clustering(nested_clusters, k=3)

