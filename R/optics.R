
#' Ordering points to identify the clustering structure (OPTICS)
#' @description Finds clusters in the input data using the OPTICS algorithm.
#'
#' @param data matrix; matrix columns are (mathematical) vectors. Input data.
#' @param eps double; neighborhood radius
#' @param minPts integer; minimum number of points in neighborhood required
#' @param distanceFunction function; function calculating the distance between two vectors of length \code{nrow(data)}
#' @param extractDBSCAN logical; if TRUE the DBSCAN will be extracted from the OPTICS data before returning.
#'
#' @return matrix; input data with the data produced by the OPTICS algorithm added as attributes.
#' @export
#'
#' @examples
#' data <- matrix(c(1,1.1,1,1,2,2,2,2.1), ncol=4)
#' OPTICS(data, .2, 1)
OPTICS <- function (data, eps, minPts, extractDBSCAN=FALSE, distanceFunction = euclidean_distance) {
  stopifnot("data cannot be empty!" = length(data) > 0);
  stopifnot("data has to be a matrix. columns are vectors." = is.matrix(data));

  stopifnot("eps has to be numeric" = is.numeric(eps));
  stopifnot("eps has to be a positive number" = eps > 0);
  stopifnot("minPts has to be numeric" = is.numeric(minPts));

  n <- ncol(data);
  minPts <- as.integer(minPts);

  stopifnot("minPts has to be a positive integer." = minPts > 0);
  stopifnot("minPts's value cannot be higher than the number of vectors." = minPts <= n);

  getNeighbors <- function (idx) {
    P <- data[, idx];
    distances <- apply(data, 2, function(D){ distanceFunction(P, D) })

    neighbors <- seq(n)[distances < eps];
    attr(neighbors, "core-distance") <- distances[order(distances)[minPts]];
    return(neighbors)
  }

  update <- function (N, p_idx) {
    coredist <- core_dist[p_idx]
    for (neighbor in N) {
      if (!visited[neighbor]) {
        new_reach_dist <- max(coredist, distanceFunction(data[, p_idx], data[, neighbor]));
        if (reach_dist[neighbor] == UNDEFINED) {
          reach_dist[neighbor] <<- new_reach_dist;
          in_seeds[neighbor] <<- TRUE;
          predecessor[neighbor] <<- p_idx;
        } else {
          if (new_reach_dist < reach_dist[neighbor]) {
            reach_dist[neighbor] <<- new_reach_dist;
            in_seeds[neighbor] <<- TRUE;
            predecessor[neighbor] <<- p_idx;
          }
        }
      }
    }
  }

  getNextSeedIndex <- function () {
    which(in_seeds, TRUE)[which.min(reach_dist[in_seeds])];
  }

  writeToOrder <- function (idx) {
    attr(data, "reachability-distance")[idx] <<- reach_dist[idx];
    order <<- c(order, idx)
  }

  UNDEFINED <- Inf;



  attr(data, "reachability-distance") <- rep(UNDEFINED, n);

  visited <- logical(n);
  reach_dist <- rep(UNDEFINED, n);
  core_dist <- rep(UNDEFINED, n);
  order <- c();

  predecessor <- rep(UNDEFINED, n);

  for (p_idx in seq(n)) {
    if (visited[p_idx] == FALSE) {
      # expandClusterOrder
      neighbors <- getNeighbors(p_idx);
      visited[p_idx] <- TRUE;
      reach_dist[p_idx] <- UNDEFINED;
      if(length(neighbors) >= minPts) {
        core_dist[p_idx] <- attr(neighbors, "core-distance");
      }
      writeToOrder(p_idx)
      if (core_dist[p_idx] != UNDEFINED){
        in_seeds <- rep(FALSE, n);
        update(neighbors, p_idx);
        while (sum(in_seeds) > 0){
          q_idx = getNextSeedIndex();
          in_seeds[q_idx] <- FALSE;

          neighbors_prime <- getNeighbors(q_idx);
          visited[q_idx] <- TRUE;
          writeToOrder(q_idx)
          if (length(neighbors_prime) >= minPts) {
            core_dist[q_idx] <- attr(neighbors_prime, "core-distance");
          }
          if (core_dist[q_idx] != UNDEFINED) {
            update(neighbors_prime, q_idx);
          }
        }
      }
      # END expandClusterOrder
    }
  }


  attr(data, "core-distance") <- core_dist;
  attr(data, "ordering") <- order;
  attr(data, "eps") <- eps;
  attr(data, "minPts") <- minPts;
  attr(data, "predecessor") <- predecessor;

  if (extractDBSCAN) {
    data <- extract_DBSCAN_clustering(data);
  }

  return(data);
}


#' Reachabilty Plot
#' @description Generates a reachability plot for data resulting from the OPTICS function.
#'
#' @param data matrix; data has to be the result of a previous run of the OPTICS algorithm.
#' @param threshold numeric; show the given threshold in the plot, optinal.
#'
#' @export
#'
#' @examples
#' data <- generate_2d_cluster(200);
#' data <- OPTICS(data, .2, 25);
#' reachability_plot(data, .15);
reachability_plot <- function (data, threshold=NULL) {
  stopifnot("data has to be processed by the OPTICS algorithm first" = 5 == sum(
    c(
      "reachability-distance",
      "core-distance",
      "ordering",
      "eps",
      "minPts"
    ) %in% names(attributes(data))
  ))

  stopifnot("threshold has to be numeric" = is.numeric(threshold))
  stopifnot("threshold has to be positive" = threshold > 0)


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

  plot(seq(n - numNoise), y, type="h", cex.axis=.75, xlab="", ylab="");
  if (!missing(threshold)) graphics::abline(h=threshold, col="red");
}


#' Cluster by Reachability
#' @description Extract cluster via a reachability threshold. This naive approach is inferior to the extract_DBSCAN_clustering function!
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
  stopifnot("data has to be processed by the OPTICS algorithm first" = 5 == sum(
    c(
      "reachability-distance",
      "core-distance",
      "ordering",
      "eps",
      "minPts"
    ) %in% names(attributes(data))
  ))

  stopifnot("threshold has to be numeric" = is.numeric(threshold))
  stopifnot("threshold has to be positive" = threshold > 0)

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

#' Extract DBSCAN Clustering
#' @description Extract the DBSCAN clustering from data processed by the OPTICS algorithm according to the paper "OPTICS: Ordering Points To Identify the Clustering Structure".
#'
#' @param data matrix; data processed by the OPTICS algorithm
#' @param eps_prime double; radius used for cluster extraction. \code{eps_prime} has to be less than or equal to the \code{eps} used in the OPTICS algorithm. If this is not set the OPTICS's \code{eps} value is used instead.
#'
#' @return The input data with the \code{"cluster"} attribute added which contains the clustering information.
#' @export
#'
#' @examples
#' data <- matrix(c(1,1.1,1,1,2,2,2,2.1), ncol=4)
#' opt_data <- OPTICS(data, .2, 1)
#' extract_DBSCAN_clustering(opt_data, .2)
extract_DBSCAN_clustering <- function (data, eps_prime=NULL) {
  stopifnot("data has to be processed by the OPTICS algorithm first" = 5 == sum(
    c(
      "reachability-distance",
      "core-distance",
      "ordering",
      "eps",
      "minPts"
    ) %in% names(attributes(data))
  ))

  if (missing(eps_prime)) {
    eps_prime <- attr(data, "eps");
  } else {
    stopifnot("eps_prime has to be numeric" = is.numeric(eps_prime))
    stopifnot("eps_prime has to be positive" = eps_prime > 0)
    stopifnot("eps_prime has to be less than or equal to the epsilon used in the OPTICS algorithm" = eps_prime <= attr(data, "eps"));
  }

  UNDEFINED <- Inf;
  NOISE <- -1;

  n <- ncol(data);
  order <-attr(data, "ordering");
  rd <- attr(data, "reachability-distance");
  cd <- attr(data, "core-distance");

  cluster <- integer(n);

  curCluster <- 0;
  for (j in seq(n)) {
    i <- order[j];
    if (rd[i] > eps_prime) {
      if (cd[i] <= eps_prime) {
        curCluster <- curCluster + 1;
        cluster[i] <- curCluster;
      } else {
        cluster[i] <- NOISE;
      }
    } else {
      cluster[i] <- curCluster;
    }
  }

  attr(data, "cluster") <- cluster;

  return(data);
}
