
#' Ordering points to identify the clustering structure (OPTICS)
#' @description Finds clusters in the input data using the OPTICS algorithm.
#'
#' @param data matrix; matrix columns are (mathematical) vectors. Input data.
#' @param eps double; neighborhood radius
#' @param MinPts integer; minimum number of points in neighborhood required
#' @param distanceFunction function; function calculating the distance between two vectors of length \code{nrow(data)}
#'
#' @return matrix; input data with attribute "cluster" assigning a cluster to every column-vector.
#' @export
#'
#' @examples
#' data <- matrix(c(1,1.1,1,1,2,2,2,2.1), ncol=4)
#' DBSCAN(data, .2, 1)
OPTICS <- function (data, eps, minPts, distanceFunction = euclidean_distance, threshold=NULL) {
  stopifnot( "data has to be a matrix. columns are vectors." = is.matrix(data));
  stopifnot( "eps has to be a positive number" = eps > 0);
  stopifnot( "minPts has to be a positive integer." = as.integer(minPts) > 0);

  n <- ncol(data)

  o <- c()

  getNeighbors <- function (idx) {
    P <- data[, idx];
    distances <- apply(data, 2, function(D){ distanceFunction(P, D) })

    neighbors <- seq(n)[distances <= eps];
    attr(neighbors, "core-distance") <- distances[order(distances)[minPts]];
    return(neighbors)
  }

  update <- function (N, p_idx) {
    coredist <- attr(data, "core-distance")[p_idx]
    for (neighbor in N) {
      if (attr(data, "visited")[neighbor] == FALSE) {
        new_reach_dist <- max(coredist, distanceFunction(data[, p_idx], data[, neighbor]));
        if (attr(data, "reachability-distance")[neighbor] == -1) {
          attr(data, "reachability-distance")[neighbor] <<- new_reach_dist;
          in_seeds[neighbor] <<- TRUE;
        } else {
          if (new_reach_dist < attr(data, "reachability-distance")[neighbor]) {
            attr(data, "reachability-distance")[neighbor] <<- new_reach_dist;
            in_seeds[neighbor] <<- TRUE;
          }
        }
      }
    }
  }

  attr(data, "visited") <- rep(FALSE, n);
  attr(data, "reachability-distance") <- rep(-1, n);
  attr(data, "core-distance") <- rep(-1, n);

  for (p_idx in seq(n)) {
    if (attr(data, "visited")[p_idx] == FALSE) {
      neighbors <- getNeighbors(p_idx);
      attr(data, "visited")[p_idx] <- TRUE;
      if(length(neighbors) >= minPts) {
        attr(data, "core-distance")[p_idx] <- attr(neighbors, "core-distance");
      }
      o <- c(o, p_idx)
      if (attr(data, "core-distance")[p_idx] != -1){
        in_seeds <- rep(FALSE, n);
        update(neighbors, p_idx);
        while (sum(in_seeds) > 0){
          q_idx = which(in_seeds, TRUE)[which.min(attr(data, "reachability-distance")[in_seeds])];
          in_seeds[q_idx] <- FALSE;

          neighbors_prime <- getNeighbors(q_idx);
          attr(data, "visited")[q_idx] <- TRUE;
          o <- c(o, q_idx);
          if (length(neighbors_prime) >= minPts) {
            attr(data, "core-distance")[q_idx] <- attr(neighbors_prime, "core-distance");
          }
          if (attr(data, "core-distance")[q_idx] != -1) {
            update(neighbors_prime, q_idx);
          }
        }
      }
    }
  }

  attr(data, "visited") <- NULL;
  attr(data, "ordering") <- o;

  if (!missing(threshold)) {
    data <- cluster_by_reachability(data, threshold);
  }

  return(data)
}


#' Reachabilty Plot
#' @description Generates a reachability plot for data resulting from the OPTICS function.
#'
#' @param data matrix; data has to be the result of a previous run of the OPTICS algorithm.
#' @param threshold numeric; show the given threshold in the plot, optinal.
#'
#' @return
#' @export
#'
#' @examples
#' data <- generate_2d_cluster(200);
#' data <- OPTICS(data, .2, 25);
#' reachability_plot(data, .15);
reachability_plot <- function (data, threshold=NULL) {
  n <- ncol(data);
  noise_indices <- seq(n)[attr(data, "reachability-distance") == -1];
  numNoise <- length(noise_indices);

  y <- c();
  for (i in seq(n)) {
    idx <- attr(data, "ordering")[i];

    if (idx %in% noise_indices) next; # skip if noise

    reach_dist <- attr(data, "reachability-distance")[idx];
    y <- c(y, reach_dist);
  }

  plot(seq(n - numNoise), y, type="h");
  if (!missing(threshold)) abline(h=threshold, col="red");
}


#' Cluster by Reachability
#' @description Extract cluster via a reachability threshold. Using the correct threshold extracts the DBSCAN clustering. This correct threshold might for example be guessed by looking at the reachability plot with each valley being a cluster.
#'
#' @param data matrix; input vectors, with each column of the matrix being a vector. \code{nrow(data)} is the ector dimension.
#' @param threshold double threshold used to determine clusters
#'
#' @return returns \code{data} with an addition \code{"cluster"} attribute containing the clusters. Noise is marked as -1.
#' @export
#'
#' @examples
#' data <- generate_2d_cluster(200);
#' data <- OPTICS(data, .2, 25);
#' cluster_by_reachability(data, .15);
cluster_by_reachability <- function (data, threshold) {
  stopifnot("threshold has to be numeric!" = is.numeric(threshold));

  n <- ncol(data)
  cluster <- c()
  curCluster <- 0;
  prevWasNoise <- TRUE;
  for (i in seq(n)) {
    idx <- attr(data, "ordering")[i];
    rd <- attr(data, "reachability-distance")[idx];
    if (rd >= threshold || rd == -1) {
      cluster[idx] <- -1;
      prevWasNoise <- TRUE;
    } else if (rd < threshold) {
      if (prevWasNoise) {
        curCluster <- curCluster + 1;
        prevWasNoise <- FALSE;
      }
      cluster[idx] <- curCluster;
    }
  }

  attr(data, "cluster") <- cluster;

  return(data);
}
