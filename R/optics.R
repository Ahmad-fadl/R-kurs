

OPTICS <- function (data, eps, minPts, distanceFunction = euclidean_distance) {
  stopifnot( "data has to be a matrix. columns are vectors." = is.matrix(data));
  stopifnot( "eps has to be a positive number" = eps > 0);
  stopifnot( "minPts has to be a positive integer." = as.integer(minPts) > 0);

  n <- ncol(data)

  o <- c()

  getNeighbors <- function (idx) {
    P <- data[, idx];
    distances <- apply(data, 2, function(D){ distanceFunction(P, D) })

    neighbors <- seq(ncol(data))[distances < eps];
    attr(neighbors, "core-distance") <- min(distances[-idx])
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
          }
        }
      }
    }
  }

  attr(data, "visited") <- rep(FALSE, n);
  attr(data, "reachability-distance") <- rep(-1, n);
  attr(data, "core-distance") <- rep(-1, n);
  # attr(data, "cluster") <- rep(-1, n);

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

  return(data)
}

reachability_plot <- function (data) {
  n <- ncol(data);
  x <- seq(n);
  y <- c();
  for (i in x) {
    idx <- attr(data, "ordering")[i];
    reach_dist <- attr(data, "reachability-distance")[idx];
    if (reach_dist != -1) {
      y <- c(y, reach_dist);
    } else {
      y <- c(y, 0);
    }
  }

  plot(x,y, type="h");
}
